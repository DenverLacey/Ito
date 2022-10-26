const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const CodeLocation = @import("parser.zig").CodeLocation;

const errors = @import("errors.zig");
const ErrMsg = errors.ErrMsg;
const raise = errors.raise;
const todo = errors.todo;

const BucketArrayUnmanaged = @import("bucket_array.zig").BucketArrayUnmanaged;

const interpreter = @import("interpreter.zig");
const Interpreter = interpreter.Interpreter;
const LambdaDefinition = interpreter.LambdaDefinition;

const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstKind = ast.AstKind;
const AstLiteral = ast.AstLiteral;
const AstIdent = ast.AstIdent;
const AstGetVar = ast.AstGetVar;
const AstGetType = ast.AstGetType;
const AstGetTag = ast.AstGetTag;
const AstUnary = ast.AstUnary;
const AstBinary = ast.AstBinary;
const AstBlock = ast.AstBlock;
const AstReturn = ast.AstReturn;
const AstIf = ast.AstIf;
const AstWhile = ast.AstWhile;
const AstFor = ast.AstFor;
const AstCase = ast.AstCase;
const AstContinue = ast.AstContinue;
const AstDef = ast.AstDef;
const AstInstantiateLambda = ast.AstInstantiateLambda;
const AstParam = ast.AstParam;
const AstVarBlock = ast.AstVarBlock;
const AstVar = ast.AstVar;
const AstTypeBlock = ast.AstTypeBlock;
const AstType = ast.AstType;
const AstTypeSignature = ast.AstTypeSignature;

const values = @import("values.zig");
const Type = values.Type;
const NamedParam = values.NamedParam;
const TypeDefinition = values.TypeDefinition;
const RecordTypeDefinition = values.RecordTypeDefinition;
const TagTypeDefinition = values.TagTypeDefinition;
const UnionTypeDefinition = values.UnionTypeDefinition;
const LambdaTypeDefinition = values.LambdaTypeDefinition;

const Binding = union(enum) {
    Var: VarBinding,
    Lambda: VarBinding,
    Type: Type,
    Tag: TagBinding,

    const This = @This();

    fn goesOnStack(this: This) bool {
        return this == .Var or this == .Lambda;
    }
};

const VarBinding = struct {
    typ: Type,
    type_open: bool = false,
    index: usize,
    global: bool,
};

const TagBinding = struct {
    tag: []const u8,
    typ: Type,
};

const Scope = struct {
    parent: ?*This = null,
    bindings: StringArrayHashMapUnmanaged(Binding) = .{},
    num_vars: usize = 0,

    const This = @This();

    fn init(parent: ?*This) This {
        var this = This{ .parent = parent };
        this.num_vars = if (parent) |p| p.num_vars else 0;
        return this;
    }

    fn deinit(this: *This, allocator: Allocator) void {
        this.bindings.deinit(allocator);
    }

    fn makeVarBinding(this: *This, typ: Type, type_open: bool, global: bool) Binding {
        return Binding{ .Var = .{ 
            .typ = typ,
            .type_open = type_open,
            .index = this.num_vars,
            .global = global,
        }};
    }

    fn addBinding(this: *This,
        allocator: Allocator,
        ident: *AstIdent,
        binding: Binding,
        err_msg: *ErrMsg
    ) !*Binding {
        const entry = try this.bindings.getOrPut(allocator, ident.ident);
        if (entry.found_existing) {
            return raise(error.TypeError, err_msg, ident.token.location, "Redeclared identifier `{s}`.", .{ident.ident});
        }

        if (binding.goesOnStack()) {
            this.num_vars += 1;
        }

        entry.value_ptr.* = binding;
        return entry.value_ptr;
    }

    fn findBinding(this: *This, ident: []const u8) ?*Binding {
        var it: ?*This = this;
        while (it) |scope| {
            if (scope.bindings.getPtr(ident)) |binding| {
                return binding;
            }

            it = scope.parent;
        }

        return null;
    }
};

const CurrentFunction = struct {
    name: []const u8,
    infer_return_type: bool = false,
    return_types: ArrayListUnmanaged(Type) = .{},
};

pub const Typer = struct {
    allocator: Allocator,
    interp: *Interpreter,
    nodes: ArrayListUnmanaged(*Ast),

    global_scope: *Scope,
    scopes: BucketArrayUnmanaged(8, Scope),

    current_function: ?CurrentFunction,

    err_msg: ErrMsg,

    const This = @This();

    pub fn init(interp: *Interpreter) !This {
        var scopes = BucketArrayUnmanaged(8, Scope){};
        try scopes.push(interp.allocator, Scope{});
        const global_scope = scopes.top().?;

        return This{ 
            .allocator = interp.allocator,
            .interp = interp,
            .nodes = ArrayListUnmanaged(*Ast){},
            .global_scope = global_scope,
            .scopes = scopes,
            .current_function = null,
            .err_msg = ErrMsg{},
        };
    }

    fn currentScope(this: *This) *Scope {
        return this.scopes.top().?;
    }

    fn beginScope(this: *This) !void {
        return this.scopes.push(this.allocator, Scope.init(this.currentScope()));
    }

    fn endScope(this: *This) void {
        const scope = this.currentScope();
        std.debug.assert(scope != this.global_scope);

        scope.deinit(this.allocator);

        this.scopes.pop(this.allocator);
    }

    fn addNode(this: *This, node: *Ast) !void {
        try this.nodes.append(this.allocator, node);
    }

    fn typecheckNodeAndAdd(this: *This, node: *Ast) !void {
        const t_node = (try this.typecheckNode(node)) orelse return;
        try this.addNode(t_node);
    }

    fn typecheckNode(this: *This, node: *Ast) anyerror!?*Ast {
        var t_node: ?*Ast = undefined;

        switch (node.kind) {
            // Literals
            .None,
            .Bool,
            .Char,
            .Int,
            .Num,
            .Str => {
                const l = node.downcast(AstLiteral);
                t_node = typecheckLiteral(l).asAst();
            },
            .Ident => {
                const ident = node.downcast(AstIdent);
                t_node = try this.typecheckIdent(ident);
            },

            // Unary
            .Negate,
            .Not,
            .Unwrap,
            .Show => {
                const unary = node.downcast(AstUnary);
                t_node = (try this.typecheckUnary(unary)).asAst();
            },

            // Binary
            .Add,
            .Subtract,
            .Multiply,
            .Divide,
            .Equal,
            .NotEqual,
            .LessThan,
            .GreaterThan,
            .Or,
            .And,
            .Index,
            .Call,
            .ExclusiveRange,
            .NoneOr,
            .CaseBranch,
            .Format,
            .PartialCopy => {
                const binary = node.downcast(AstBinary);
                t_node = (try this.typecheckBinary(binary)).asAst();
            },
            .Assign => {
                const assign = node.downcast(AstBinary);
                t_node = (try this.typecheckAssign(assign)).asAst();
            },
            .Dot => {
                const dot = node.downcast(AstBinary);
                t_node = (try this.typecheckDot(dot)).asAst();
            },

            // Blocks
            .Block,
            .Comma => {
                const block = node.downcast(AstBlock);
                t_node = (try this.typecheckBlock(block)).asAst();
            },
            .List => {
                const list = node.downcast(AstBlock);
                t_node = (try this.typecheckList(list)).asAst();
            },
            .Record => {
                const record = node.downcast(AstBlock);
                t_node = (try this.typecheckRecordLiteral(record)).asAst();
            },

            // Returns
            .Return => {
                const ret = node.downcast(AstReturn);
                t_node = (try this.typecheckReturn(ret)).asAst();
            },
            .Break => {
                const _break = node.downcast(AstReturn);
                t_node = (try this.typecheckBreak(_break)).asAst();
            },

            .If => {
                const _if = node.downcast(AstIf);
                t_node = (try this.typecheckIf(_if)).asAst();
            },
            .While => {
                const _while = node.downcast(AstWhile);
                t_node = (try this.typecheckWhile(_while)).asAst();
            },
            .For => {
                const _for = node.downcast(AstFor);
                t_node = (try this.typecheckFor(_for)).asAst();
            },
            .Case => {
                const case = node.downcast(AstCase);
                t_node = (try this.typecheckCase(case)).asAst();
            },
            .Continue => {
                // @TODO: Check that we're in a loop
                t_node = node;
            },
            .Def => {
                const def = node.downcast(AstDef);
                t_node = (try this.typecheckDef(def)).asAst();
            },
            .Param => {
                unreachable;
                // const param = node.downcast(AstParam);
                // t_node = (try this.typecheckParam(param)).asAst();
            },
            .Var => {
                const _var = node.downcast(AstVar);
                t_node = (try this.typecheckVar(_var)).asAst();
            },
            .VarBlock => {
                const var_block = node.downcast(AstVarBlock);
                for (var_block.declarations) |decl| {
                    try this.typecheckNodeAndAdd(decl.asAst());
                }
                t_node = null;
            },
            .Type => {
                const _type = node.downcast(AstType);
                t_node = (try this.typecheckType(_type)).asAst();
            },
            .TypeBlock => {
                const type_block = node.downcast(AstTypeBlock);
                for (type_block.declarations) |decl| {
                    try this.typecheckNodeAndAdd(decl.asAst());
                }
                t_node = null;
            },
            .TypeSignature => {
                todo("Implement typechecking type signatures.");
                // const sig = node.downcast(AstTypeSignature);
                // t_node = (try this.typecheckTypeSignature(sig)).asAst();
            },

            // Requires Greater Context
            .Bind => {
                const bind = node.downcast(AstBinary);
                bind.rhs = (try this.typecheckNode(bind.rhs)).?;
                bind.typ = bind.rhs.typ;
                t_node = bind.asAst();
            },

            // Typechecked specific
            .GetGlobal => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Attempt to typecheck GetGlobal node made.", .{});
            },
            .GetVar => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Attempt to typecheck GetVar node made.", .{});
            },
            .GetType => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Attempt to typecheck GetVar node made.", .{});
            },
            .GetTag => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Attempt to typecheck GetTag node made.", .{});
            },
            .InstantiateLambda => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Attempt to typecheck InstantiateLambda node made.", .{});
            },
        }

        return t_node;
    }

    fn typecheckLiteral(literal: *AstLiteral) *AstLiteral {
        switch (literal.literal) {
            .None => literal.typ = .None,
            .Bool => literal.typ = .Bool,
            .Char => literal.typ = .Char,
            .Int => literal.typ = .Int,
            .Num => literal.typ = .Num,
            .Str => literal.typ = .Str,
        }

        return literal;
    }

    fn typecheckIdent(this: *This, ident: *AstIdent) !*Ast {
        var t_node: *Ast = undefined;

        if (this.currentScope().findBinding(ident.ident)) |binding| {
            switch (binding.*) {
                .Var, .Lambda => |var_binding| {
                    var get = try this.allocator.create(AstGetVar);
                    get.* = AstGetVar.init(ident.token, var_binding.typ, var_binding.index, var_binding.global);
                    t_node = get.asAst();
                },
                .Type => |typ| {
                    var get = try this.allocator.create(AstGetType);
                    get.* = AstGetType.init(ident.token, typ);
                    t_node = get.asAst();
                },
                .Tag => |tag_binding| {
                    var get = try this.allocator.create(AstGetTag);
                    const tag = try this.interp.findOrAddTag(ident.ident);
                    get.* = AstGetTag.init(ident.token, tag_binding.typ, tag);
                    t_node = get.asAst();
                },
            }
        } else if (ident.big) {
            const tag = try this.interp.findOrAddTag(ident.ident);
            const variants = try this.allocator.alloc(TagTypeDefinition.Variant, 1);
            variants[0] = .{ .name = tag };
            const tag_type = try this.interp.findOrAddTagType(variants);

            const tag_binding = TagBinding{ .tag = tag, .typ = tag_type };
            _ = try this.currentScope().addBinding(this.allocator, ident, Binding{ .Tag = tag_binding }, &this.err_msg);

            var get = try this.allocator.create(AstGetTag);
            get.* = AstGetTag.init(ident.token, tag_type, tag);
            t_node = get.asAst();
        } else {
            return raise(error.TypeError, &this.err_msg, ident.token.location, "Undeclarated identifier `{s}`.", .{ident.ident});
        }

        return t_node;
    }

    fn typecheckUnary(this: *This, unary: *AstUnary) !*AstUnary {
        const t_sub = (try this.typecheckNode(unary.sub)).?;

        switch (unary.kind) {
            .Negate => {
                if (t_sub.typ.? == .Int or t_sub.typ.? == .Num) {
                    unary.typ = t_sub.typ;
                } else {
                    return raise(error.TypeError, &this.err_msg, t_sub.token.location, "`-` operator expected either an `Int` or `Num` value but encountered a `{}` value.", .{t_sub.typ.?});
                }
            },
            .Not => {
                if (t_sub.typ.? == .Bool) {
                    unary.typ = t_sub.typ;
                } else {
                    return raise(error.TypeError, &this.err_msg, t_sub.token.location, "`!` operator expected a `Bool` value but encountered a `{}` value.", .{t_sub.typ.?});
                }
            },
            .Unwrap => {
                switch (t_sub.typ.?) {
                    .Union => |union_index| {
                        const union_type = this.interp.union_types.items[union_index];
                        
                        const copied = try this.allocator.alloc(Type, union_type.variants.len - 1);

                        var write_copied: usize = 0;
                        for (union_type.variants) |variant| {
                            if (variant == .None) continue;

                            if (write_copied >= copied.len) {
                                return raise(error.TypeError, &this.err_msg, t_sub.token.location, "`?` operator expected a potentially `None` value but encountered a `{}` value.", .{t_sub.typ.?});
                            }

                            copied[write_copied] = variant;
                            write_copied += 1;
                        }

                        unary.typ = try this.interp.unionizeTypes(copied);
                    },
                    .None => return raise(error.TypeError, &this.err_msg, t_sub.token.location, "Unwrap of `None` value will always fail.", .{}),
                    else => return raise(error.TypeError, &this.err_msg, t_sub.token.location, "`?` operator expected a potentially `None` value but encountered a `{}` value.", .{t_sub.typ.?}),
                }
            },
            .Show => {
                unary.typ = .Str;
            },
            else => return raise(error.InternalError, &this.err_msg, unary.token.location, "`{}` is not a unary node.", .{unary.kind}),
        }

        unary.sub = t_sub;
        return unary;
    }

    fn typecheckBinary(this: *This, binary: *AstBinary) !*AstBinary {
        const t_lhs = (try this.typecheckNode(binary.lhs)).?;
        const t_rhs = (try this.typecheckNode(binary.rhs));

        switch (binary.kind) {
            .Add => {
                if (t_lhs.typ.? == .Str and t_rhs.?.typ.? == .Str) {
                    binary.typ = .Str;
                } else {
                    binary.typ = try this.inferReturnTypeOfArithmetic("+", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
                }
            },
            .Subtract => {
                binary.typ = try this.inferReturnTypeOfArithmetic("-", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
            },
            .Multiply => {
                if ((t_lhs.typ.? == .Str and t_rhs.?.typ.? == .Int) or
                    (t_lhs.typ.? == .Int and t_rhs.?.typ.? == .Str))
                {
                    binary.typ = .Str;
                } else {
                    binary.typ = try this.inferReturnTypeOfArithmetic("*", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
                }
            },
            .Divide => {
                binary.typ = try this.inferReturnTypeOfArithmetic("/", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
            },
            .Equal => {
                binary.typ = .Bool;
            },
            .NotEqual => {
                binary.typ = .Bool;
            },
            .LessThan => {
                if ((t_lhs.typ.? == .Int or t_lhs.typ.? == .Num) and (t_rhs.?.typ.? == .Int or t_rhs.?.typ.? == .Num)) {
                    binary.typ = .Bool;
                } else {
                    return raise(error.TypeError, &this.err_msg, binary.token.location, "`<` requires its operands to be Bool values.", .{});
                }
            },
            .GreaterThan => {
                if ((t_lhs.typ.? == .Int or t_lhs.typ.? == .Num) and (t_rhs.?.typ.? == .Int or t_rhs.?.typ.? == .Num)) {
                    binary.typ = .Bool;
                } else {
                    return raise(error.TypeError, &this.err_msg, binary.token.location, "`>` requires its operands to be Bool values.", .{});
                }
            },
            .Or => {
                if ((t_lhs.typ.? == .Bool) and (t_rhs.?.typ.? == .Bool)) {
                    binary.typ = .Bool;
                } else {
                    return raise(error.TypeError, &this.err_msg, binary.token.location, "`or` requires both operands to be a Bool value.", .{});
                }
            },
            .And => {
                if ((t_lhs.typ.? == .Bool) and (t_rhs.?.typ.? == .Bool)) {
                    binary.typ = .Bool;
                } else {
                    return raise(error.TypeError, &this.err_msg, binary.token.location, "`and` requires both operands to be a Bool value.", .{});
                }
            },
            .Index => {
                todo("Implement typechecking index.");
            },
            .Call => {
                try this.typecheckCall(binary, t_lhs, t_rhs.?);
            },
            // .Dot => {
            //     try this.typecheckDot(binary, t_lhs, t_rhs.?);
            // },
            .ExclusiveRange => {
                if (t_lhs.typ.? == .Int and t_rhs.?.typ.? == .Int) {
                    binary.typ = .RangeInt;
                } else {
                    return raise(error.TypeError, &this.err_msg, binary.token.location, "`..` requires its operands to be Int values.", .{});
                }
            },
            .NoneOr => {
                // @TODO:
                // Check that rhs won't be null
                //

                switch (t_lhs.typ.?) {
                    .Union => |union_index| {
                        const union_type = this.interp.union_types.items[union_index];
                        if (!union_type.isSuperset(&[_]Type{.None})) {
                            return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "`??` requires its first operand to be a potentially `None` value but encountered a `{}` value.", .{t_lhs.typ.?});
                        }

                        // @TODO:
                        // Unionize lhs type with rhs type.
                        //

                        binary.typ = try this.interp.unionTypeWithVariantsRemoved(union_type, &[_]Type{.None});
                    },
                    .None => {
                        binary.typ = t_rhs.?.typ.?;
                    },
                    else => return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "`??` requires its first operand to ba a potentially `None` value but encountered a `{}` value.", .{t_lhs.typ.?}),
                }
            },
            .Format => {
                if (t_lhs.typ.? != .Str) {
                    return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "`$` requires its first operand to be a `Str` value but encountered a `{}` value.", .{t_lhs.typ.?});
                }

                if (t_rhs.?.typ.? != .List) {
                    return raise(error.TypeError, &this.err_msg, t_rhs.?.token.location, "`$` requires its second oeprand to be a `List` value but encountered a `{}` value.", .{t_rhs.?.typ.?});
                }

                binary.typ = .Str;
            },
            .PartialCopy => {
                const record_type = switch (t_lhs.typ.?) {
                    .Record => |record_index| this.interp.record_types.items[record_index],
                    else => return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "Cannot partially copy something that isn't a record value.", .{}),
                };

                var updated_fields = ArrayListUnmanaged([]const u8){};
                defer updated_fields.deinit(this.allocator);

                if (t_rhs.?.kind != .Comma) {
                    return raise(error.InternalError, &this.err_msg, t_rhs.?.token.location, "rhs of partial copy not a block.", .{});
                }

                const updated_field_nodes = t_rhs.?.downcastConst(AstBlock);
                for (updated_field_nodes.nodes) |node| {
                    if (node.kind != .Bind) {
                        return raise(error.TypeError, &this.err_msg, node.token.location, "Expected a named parameter for partial record copy.", .{});
                    }

                    const bind = node.downcastConst(AstBinary);

                    if (bind.lhs.kind != .Ident) {
                        return raise(error.TypeError, &this.err_msg, bind.lhs.token.location, "Expected a field identifier in partial record copy.", .{});
                    }

                    const field_ident = bind.lhs.downcastConst(AstIdent);
                    for (updated_fields.items) |uf| {
                        if (std.mem.eql(u8, uf, field_ident.ident)) {
                            return raise(error.TypeError, &this.err_msg, field_ident.token.location, "`{s}` was already updated.", .{uf});
                        }
                    }

                    try updated_fields.append(this.allocator, field_ident.ident);

                    const field = record_type.findFieldByName(field_ident.ident) orelse return raise(error.TypeError, &this.err_msg, field_ident.token.location, "`{s}` is not a field of the type `{}`.", .{field_ident.ident, t_lhs.typ.?});

                    if (!t_rhs.?.typ.?.compat(field.typ)) {
                        return raise(error.TypeError, &this.err_msg, t_rhs.?.token.location, "Expected a `{}` value but found a `{}` value.", .{field.typ, t_rhs.?.typ.?});
                    }
                }

                binary.typ = t_lhs.typ.?;
            },
            else => return raise(error.InternalError, &this.err_msg, binary.token.location, "`{}` is not a binary node.", .{binary.kind}),
        }

        binary.lhs = t_lhs;
        binary.rhs = t_rhs.?;
        return binary;
    }

    fn inferReturnTypeOfArithmetic(
        this: *This,
        comptime op: []const u8,
        location: CodeLocation,
        type_a: Type,
        type_b: Type
    ) !Type {
        const int_or_num_type = try this.interp.unionizeTypes(&[_]Type{ .Int, .Num });

        var ret_type: Type = undefined;
        if (type_a.compat(int_or_num_type) and type_b.compat(int_or_num_type)) {
            if (type_a == .Num or type_b == .Num) {
                ret_type = .Num;
            } else {
                ret_type = try this.interp.unionizeTypes(&[_]Type{ type_a, type_b });
            }
        } else {
            return raise(error.TypeError, &this.err_msg, location, "`" ++ op ++ "` requires both its operands to be either an `Int` or `Num` value.", .{});
        }

        return ret_type;
    }

    fn typecheckAssign(this: *This, assign: *AstBinary) !*AstBinary {
        const t_rhs = (try this.typecheckNode(assign.rhs)).?;
        if (t_rhs.typ.? == .Tag and assign.lhs.kind == .Ident) {
            const ident = assign.lhs.downcast(AstIdent);
            try this.typecheckAssignTag(assign, ident, t_rhs);
        } else {
            const t_lhs = (try this.typecheckNode(assign.lhs)).?;
            if (!t_rhs.typ.?.compat(t_lhs.typ.?)) {
                return raise(error.TypeError, &this.err_msg, t_rhs.token.location, "Expected a `{}` value but encountered a `{}` value.", .{t_lhs.typ.?, t_rhs.typ.?});
            }

            assign.lhs = t_lhs;
        }
        
        assign.typ = .None;
        assign.rhs = t_rhs;
        return assign;
    }

    fn typecheckAssignTag(this: *This, assign: *AstBinary, ident: *AstIdent, expr: *Ast) !void {
        if (this.currentScope().findBinding(ident.ident)) |binding| {
            switch (binding.*) {
                .Var => |*var_binding| {
                    if (!var_binding.type_open) {
                        todo("Error Message");
                    }

                    switch (var_binding.typ) {
                        .Tag => |binding_tag_index| {
                            const expr_tag_index = switch (expr.typ.?) {
                                .Tag => |eti| eti,
                                else => unreachable,
                            };

                            const binding_tag_type = this.interp.tag_types.items[binding_tag_index];
                            const expr_tag_type = this.interp.tag_types.items[expr_tag_index];
                            
                            var_binding.typ = try this.interp.combineTagSets(binding_tag_type, expr_tag_type);

                            assign.lhs = (try this.typecheckNode(ident.asAst())).?;
                        },
                        else => todo("Error Message"),
                    }
                },
                else => todo("Error Message"),
            }
        } else {
            return raise(error.TypeError, &this.err_msg, ident.token.location, "Unknown identifier `{s}`.", .{ident.ident});
        }
    }

    fn typecheckBlock(this: *This, block: *AstBlock) !*AstBlock {
        // @TODO:
        // Scopes
        //
        if (block.kind == .Block) {
            try this.beginScope();
        }

        defer if (block.kind == .Block) {
            this.endScope();
        };

        var i: usize = 0;
        while (i < block.nodes.len) {
            if (try this.typecheckNode(block.nodes[i])) |t_node| {
                block.nodes[i] = t_node;
                i += 1;
            } else {
                // @TODO: Remove null nodes
                // block.nodes[i..] = block.nodes[i+1..];
            }
        }

        if (block.nodes.len > 0) {
            block.typ = block.nodes[block.nodes.len-1].typ;
        } else {
            block.typ = .None;
        }

        return block;
    }

    fn typecheckList(this: *This, list: *AstBlock) !*AstBlock {
        var item_types = ArrayListUnmanaged(Type){};
        defer item_types.deinit(this.allocator);

        var any = false;

        var i: usize = 0;
        while (i < list.nodes.len) : (i += 1) {
            const t_node = (try this.typecheckNode(list.nodes[i])).?;
            list.nodes[i] = t_node;

            if (t_node.typ.? == .Any or any) {
                any = true;
            } else {
                var unique = true;
                for (item_types.items) |typ| {
                    if (typ.eql(t_node.typ.?)) {
                        unique = false;
                        break;
                    }
                }
                if (unique) try item_types.append(this.allocator, t_node.typ.?);
            }
        }

        if (any) {
            list.typ = try this.interp.findOrAddListType(.Any);
        } else if (item_types.items.len == 1) {
            list.typ = try this.interp.findOrAddListType(item_types.items[0]);
        } else {
            const item_type = try this.interp.unionizeTypes(item_types.items);
            list.typ = try this.interp.findOrAddListType(item_type);
        }

        return list;
    }

    fn typecheckRecordLiteral(this: *This, record: *AstBlock) anyerror!*AstBlock {
        var fields = ArrayListUnmanaged(RecordTypeDefinition.Field){};
        errdefer fields.deinit(this.allocator);

        for (record.nodes) |*node| {
            if (node.*.kind != .Bind) {
                return raise(error.TypeError, &this.err_msg, node.*.token.location, "Expecting a binding.", .{});
            }

            const bind = node.*.downcast(AstBinary);
            const ident = bind.lhs.downcast(AstIdent);
            const t_rhs = (try this.typecheckNode(bind.rhs)).?;

            try fields.append(this.allocator, .{ .name = ident.ident, .typ = t_rhs.typ.? });

            node.* = t_rhs;
        }

        record.typ = try this.interp.findOrAddRecordType(fields.items);
        return record;
    }

    fn typecheckCall(this: *This, call: *AstBinary, lhs: *Ast, rhs: *Ast) !void {
        var typ = lhs.typ.?;
        if (lhs.kind == .GetType) {
            const get = lhs.downcast(AstGetType);
            typ = get.typ_of;
        }

        switch (typ) {
            .Record => |record_index| {
                const record_type = this.interp.record_types.items[record_index];
                
                std.debug.assert(rhs.kind == .Comma);
                const args = rhs.downcast(AstBlock);

                try this.typecheckCallArguments(record_type.fields, args);
                
                call.typ = Type{ .Record = record_index };
            } ,
            .Tag => |_| todo("Implement typecheck for calling tag types."),
            .Lambda => |lambda_index| {
                const lambda_type = this.interp.lambda_types.items[lambda_index];

                std.debug.assert(rhs.kind == .Comma);
                const args = rhs.downcast(AstBlock);

                try this.typecheckCallArguments(lambda_type.parameters, args);

                call.typ = lambda_type.returns;
            },
            else => return raise(error.TypeError, &this.err_msg, lhs.token.location, "A {} value is not callable.", .{lhs.typ.?}),
        }
    }

    fn typecheckCallArguments(this: *This, params: []NamedParam, args: *AstBlock) !void {
        if (params.len != args.nodes.len) {
            // @TODO: Send first extra args location instead
            return raise(error.TypeError, &this.err_msg, args.token.location, "Incorrect number of arguments! Expected {} but found {}.", .{ params.len, args.nodes.len });
        }

        var reordered_args = try this.allocator.alloc(?*Ast, params.len);
        defer this.allocator.free(reordered_args);

        for (reordered_args) |*arg| {
            arg.* = null;
        }

        var began_named_args = false;
        var i: usize = 0;
        while (i < args.nodes.len) : (i += 1) {
            const arg = args.nodes[i];
            var reordered_arg = arg;

            var param: NamedParam = undefined;
            var reordered_arg_index = i;

            if (arg.kind == .Bind) {
                const bind = arg.downcastConst(AstBinary);

                if (bind.lhs.kind != .Ident) {
                    return raise(error.TypeError, &this.err_msg, bind.lhs.token.location, "Expected an identifier here.", .{});
                }

                const ident = bind.lhs.downcastConst(AstIdent);

                for (params) |p, param_index| {
                    if (std.mem.eql(u8, p.name, ident.ident)) {
                        const ra = reordered_args[param_index];
                        if (ra == null) {
                            param = p;
                            reordered_arg_index = param_index;
                            reordered_arg = bind.rhs;
                            break;
                        } else {
                            return raise(error.TypeError, &this.err_msg, ident.token.location, "The parameter `{s}` has already been passed.", .{ident.ident});
                        }
                    }
                } else {
                    return raise(error.TypeError, &this.err_msg, ident.token.location, "There is no parameter named `{s}`.", .{ident.ident});
                }

                began_named_args = true;
            } else if (began_named_args) {
                return raise(error.TypeError, &this.err_msg, arg.token.location, "Cannot pass positional arguments after named arguments.", .{});
            } else {
                param = params[i];
                reordered_arg_index = i;
            }

            if (!arg.typ.?.compat(param.typ)) {
                return raise(error.TypeError, &this.err_msg, arg.token.location, "This argument is a {} value but the parameter `{s}` is specified as a {} value.", .{ arg.typ.?, param.name, param.typ });
            }

            reordered_args[reordered_arg_index] = reordered_arg;
        }

        if (began_named_args) {
            for (args.nodes) |*arg, arg_index| {
                if (reordered_args[arg_index]) |reordered_arg| {
                    arg.* = reordered_arg;
                } else {
                    const expected_param = params[i];
                    return raise(error.TypeError, &this.err_msg, args.token.location, "The parameter `{s}` was never passed.", .{expected_param.name});
                }
            }
        }
    }

    fn typecheckDot(this: *This, dot: *AstBinary) !*AstBinary {
        const t_lhs = (try this.typecheckNode(dot.lhs)).?;

        std.debug.assert(dot.rhs.kind == .Ident);
        const ident = dot.rhs.downcast(AstIdent);

        switch (t_lhs.typ.?) {
            .Record => |record_index| {
                const record_type = this.interp.record_types.items[record_index];

                // find index of field
                var field_index: ?usize = null;
                for (record_type.fields) |field, i| {
                    if (std.mem.eql(u8, field.name, ident.ident)) {
                        field_index = i;
                        break;
                    }
                }

                if (field_index) |idx| {
                    dot.lhs = t_lhs;
                    dot.typ = record_type.fields[idx].typ;
                    return dot;
                }
            },
            .Tag => |tag_index| {
                _ = tag_index;
                todo("Implement typechecking dot for records.");
            },
            else => {},
        }

        todo("Implement typechecking dot for associated functions.");
    }

    fn typecheckReturn(this: *This, ret: *AstReturn) !*AstReturn {
        _ = this;
        _ = ret;
        todo("Implement typechecking return.");
    }

    fn typecheckBreak(this: *This, _break: *AstReturn) !*AstReturn {
        _ = this;
        _ = _break;
        todo("Implement typechecking break.");
    }

    fn typecheckIf(this: *This, _if: *AstIf) !*AstIf {
        const t_cond = (try this.typecheckNode(_if.condition)).?;
        
        var t_then: ?*AstBlock = null;
        t_then = (try this.typecheckBlock(_if.then_block));

        var t_else: ?*Ast = undefined;
        if (_if.else_block) |else_block| {
            t_else = (try this.typecheckNode(else_block)).?;
        } else {
            t_else = null;
        }

        _if.condition = t_cond;
        _if.then_block = t_then.?;
        _if.else_block = t_else;
        _if.typ = try this.interp.unionizeTypes(&[_]Type{t_then.?.typ.?, if (t_else) |te| te.typ.? else .None});
        return _if;
    }

    fn typecheckWhile(this: *This, _while: *AstWhile) !*AstWhile {
        const t_cond = (try this.typecheckNode(_while.condition)).?;

        var t_block: ?*AstBlock = null;
        t_block = try this.typecheckBlock(_while.block);

        _while.condition = t_cond;
        _while.block = t_block.?;
        _while.typ = try this.interp.unionizeTypes(&[_]Type{ t_block.?.typ.?, .None });
        return _while;
    }

    fn typecheckFor(this: *This, _for: *AstFor) !*AstFor {
        try this.beginScope();
        defer this.endScope();

        const t_con = (try this.typecheckNode(_for.container)).?;

        const it_type = try this.inferIteratorVariableType(t_con.typ.?, t_con.token.location);

        var current_scope = this.currentScope();
        const it_binding = current_scope.makeVarBinding(it_type, false, this.current_function == null);
        _ = try current_scope.addBinding(this.allocator, _for.iterator, it_binding, &this.err_msg);

        const t_block = try this.typecheckBlock(_for.block);

        _for.typ = try this.interp.unionizeTypes(&[_]Type{ t_block.typ.?, .None });
        _for.container = t_con;
        _for.block = t_block;
        return _for;
    }

    fn inferIteratorVariableType(this: *This, container_type: Type, container_location: CodeLocation) !Type {
        switch (container_type) {
            .Any => return .Any,
            .Str => return .Char,
            .List => |list_index| {
                const list_type = this.interp.list_types.items[list_index];
                return list_type.item_type;
            },
            .RangeInt => return .Int,
            else => return raise(error.TypeError, &this.err_msg, container_location, "Cannot iterator over a `{}` value.", .{container_type}),
        }
    }

    fn typecheckCase(this: *This, case: *AstCase) !*AstCase {
        const t_cond = (try this.typecheckNode(case.condition)).?;

        var branch_types = ArrayListUnmanaged(Type){};
        errdefer branch_types.deinit(this.allocator);

        if (case.default) |default| {
            const t_default = (try this.typecheckNode(default)).?;

            try branch_types.append(this.allocator, t_default.typ.?);

            case.default = t_default;
        } else {
            try branch_types.append(this.allocator, .None);
        }

        for (case.branches) |*branch| {
            const t_gate = (try this.typecheckNode(branch.*.lhs)).?;
            const t_block = (try this.typecheckNode(branch.*.rhs)).?;

            if (!t_gate.typ.?.compat(t_cond.typ.?)) {
                return raise(error.TypeError, &this.err_msg, t_gate.token.location, "Gate expression of case branch does not match the type of the condition. Expected a `{}` value but found a `{}` value.", .{ t_cond.typ.?, t_gate.typ.? });
            }

            try branch_types.append(this.allocator, t_block.typ.?);

            branch.*.lhs = t_gate;
            branch.*.rhs = t_block;
        }

        case.typ = try this.interp.unionizeTypes(branch_types.items);
        case.condition = t_cond;

        return case;
    }

    fn typecheckDef(this: *This, def: *AstDef) !*AstInstantiateLambda {
        const old_function = this.current_function;
        defer this.current_function = old_function;

        this.current_function = CurrentFunction{ .name = def.ident.ident, .infer_return_type = def.ret_type == null };

        if (def.ret_type) |ret_type_sig| {
            const ret_type = try this.typecheckTypeSignature(ret_type_sig);
            try this.current_function.?.return_types.append(this.allocator, ret_type);
        } else {
            try this.current_function.?.return_types.append(this.allocator, .Any);
        }

        var params = try this.allocator.alloc(LambdaTypeDefinition.Parameter, def.params.len);
        for (def.params) |param, i| {
            const param_type: Type = if (param.sig) |s|
                try this.typecheckTypeSignature(s)
            else
                Type.Any;
            
            params[i] = .{ .name = param.ident.ident, .typ = param_type };
        }

        // @NOTE:
        // If we go down the inferring return types
        // route then this is gonna be tricky to do.
        //
        const def_type = try this.interp.findOrAddLambdaType(params, this.current_function.?.return_types.items[0]);

        const type_def = switch (def_type) {
            .Lambda => |lambda_index|
                this.interp.lambda_types.items[lambda_index],
            else => unreachable,
        };

        const lambda_def = LambdaDefinition{
            .name = def.ident.ident,
            .type_def = type_def,
            .code = def.body, // @NOTE: We assume that typechecking the body will not return a new AstBlock.
            .closed_values = &.{}, // @TODO: CLOSE THE VALUES!!!
            .closed_value_names = &.{}, // @TODO: See above
        };

        const lambda_index = try this.interp.addLambda(lambda_def);

        // @NOTE:
        // We are extracting this out so we can change it later for reasons
        // outlined in the :RecursionDuplicate.
        //
        var lambda_binding = VarBinding{ .typ = def_type, .index = this.currentScope().num_vars, .global = this.currentScope() == this.global_scope };

        const def_binding = Binding{ .Lambda = lambda_binding };
        _ = try this.currentScope().addBinding(this.allocator, def.ident, def_binding, &this.err_msg);

        // @NOTE: :LexicalScoping
        // This isn't the full story. We want to be able to refer to things in
        // the scopes above us and not just global scope but I think it'll
        // require specific machinery to achieve and so just beginning a new
        // scope isn't good enough.
        //
        try this.beginScope();
        defer this.endScope();

        const function_scope = this.currentScope();

        // @HACK:
        // This is a hacky workaround. Usually scopes are still apart of the
        // same function and so `num_vars` needs to continue on from the parent
        // but in this case, since we are starting a new function, we need to
        // start at 0.
        //
        // This is a consequence of the :LexicalScoping note and should be fixed alongside
        // that.
        //
        function_scope.num_vars = 0;

        // @NOTE: :RecursionDuplicate
        // We're adding the binding again because the evaluator will put a copy
        // of the closure on the stack because otherwise it won't be able to
        // recurse.
        lambda_binding.index = 0;
        lambda_binding.global = false;
        const recursive_binding = Binding{ .Lambda = lambda_binding };
        _ = try function_scope.addBinding(this.allocator, def.ident, recursive_binding, &this.err_msg);

        //
        // Typecheck Parameters
        //
        for (def.params) |param| {
            const param_type: Type = if (param.sig) |sig|
                try this.typecheckTypeSignature(sig)
            else
                Type.Any;
            
            _ = try function_scope.addBinding(
                this.allocator,
                param.ident,
                Binding{ .Var = VarBinding{
                    .typ = param_type,
                    .index = function_scope.num_vars,
                    .global = false,
                }},
                &this.err_msg,
            );
        }

        // @TODO:
        // - Discover closed values
        // - Stuff them into the lambda definition through the interpreter.
        //
        const function_body = try this.typecheckBlock(def.body);     
        std.debug.assert(function_body == def.body); // @NOTE: See above about `lambda_def`

        // @TODO:
        // This will need to check that it's compatible with at least one of
        // the return types.
        //
        const return_type = this.current_function.?.return_types.items[0];
        if (function_body.typ) |body_type| {
            if (!body_type.compat(return_type)) {
                // @TODO: Get location of the expression that is the wrong type
                // instead of just using the block.
                //
                return raise(error.TypeError, &this.err_msg, function_body.token.location, "Return type specified to be `{}` but the lambda body returns a {} value.", .{return_type, function_body.typ.?});
            }
        } else if (return_type != .Any and return_type != .None) {
            return raise(error.TypeError, &this.err_msg, function_body.token.location, "Return type specified to be `{}` but the lambda body returns a None value.", .{return_type});
        }

        var lambda_node = try this.allocator.create(AstInstantiateLambda);
        lambda_node.* = AstInstantiateLambda.init(def.token, lambda_index);

        return lambda_node;
    }

    fn typecheckVar(this: *This, _var: *AstVar) !*AstVar {
        const t_init = (try this.typecheckNode(_var.initializer)).?;

        var var_type = t_init.typ.?;
        var can_be_open = true;
        if (_var.specified_type) |specified_type_signature| {
            const specified_type = try this.typecheckTypeSignature(specified_type_signature);
            if (!t_init.typ.?.compat(specified_type)) {
                return raise(error.TypeError, &this.err_msg, t_init.token.location, "Cannot initialize a variable with a `{}` value when it was specified to be a `{}` value.", .{ t_init.typ.?, specified_type });
            }

            var_type = specified_type;
            can_be_open = false;
        }

        const current_scope = this.currentScope();
        const binding = current_scope.makeVarBinding(var_type, can_be_open and var_type == .Tag, this.current_function == null);
        _ = try current_scope.addBinding(this.allocator, _var.ident, binding, &this.err_msg);

        _var.initializer = t_init;
        return _var;
    }

    fn typecheckType(this: *This, _type: *AstType) !*AstType {
        var typ: ?Type = null;
        typ = try this.typecheckTypeSignature(_type.signature);

        const binding = Binding{ .Type = typ.? };
        _ = try this.currentScope().addBinding(this.allocator, _type.ident, binding, &this.err_msg);

        return _type;
    }

    fn typecheckTypeSignature(this: *This, sig: *AstTypeSignature) anyerror!Type {
        var typ: Type = undefined;
        switch (sig.data) {
            .Name => |type_name| {
                if (this.currentScope().findBinding(type_name)) |binding| {
                    switch (binding.*) {
                        .Type => |t| {
                            typ = t;
                        },
                        else => return raise(error.TypeError, &this.err_msg, sig.token.location, "`{s}` does not refer to type.", .{type_name}),
                    }
                } else if (std.mem.eql(u8, "Any", type_name)) {
                    typ = .Any;
                } else if (std.mem.eql(u8, "None", type_name)) {
                    typ = .None;
                } else if (std.mem.eql(u8, "Bool", type_name)) {
                    typ = .Bool;
                } else if (std.mem.eql(u8, "Char", type_name)) {
                    typ = .Char;
                } else if (std.mem.eql(u8, "Int", type_name)) {
                    typ = .Int;
                } else if (std.mem.eql(u8, "Num", type_name)) {
                    typ = .Num;
                } else if (std.mem.eql(u8, "Str", type_name)) {
                    typ = .Str;
                } else if (std.mem.eql(u8, "Type", type_name)) {
                    typ = .Type;
                } else {
                    return raise(error.TypeError, &this.err_msg, sig.token.location, "Undeclared identifier `{s}`.", .{type_name});
                }
            },
            .Record => |record_data| {
                std.debug.assert(record_data.field_names.len == record_data.field_types.len);

                const fields = try this.allocator.alloc(RecordTypeDefinition.Field, record_data.field_names.len);
                var i: usize = 0;
                while (i < record_data.field_names.len) : (i += 1) {
                    const field_name = record_data.field_names[i];
                    const field_type: Type = if (record_data.field_types[i]) |ft| 
                        try this.typecheckTypeSignature(ft)
                    else
                        Type.Any;

                    fields[i] = RecordTypeDefinition.Field{ .name = field_name.ident, .typ = field_type };
                }

                typ = try this.interp.findOrAddRecordType(fields);
            },
            .Tag => |tag_data| {
                const variants = try this.allocator.alloc(TagTypeDefinition.Variant, tag_data.tag_names.len);
                var i: usize = 0;
                while (i < tag_data.tag_names.len) : (i += 1) {
                    variants[i].name = tag_data.tag_names[i].ident;
                }
                typ = try this.interp.findOrAddTagType(variants);
            },
            .Union => |union_data| {
                const variants = try this.allocator.alloc(Type, union_data.variants.len);
                defer this.allocator.free(variants);

                var i: usize = 0;
                while (i < union_data.variants.len) : (i += 1) {
                    variants[i] = try this.typecheckTypeSignature(union_data.variants[i]);
                }

                typ = try this.interp.unionizeTypes(variants);
            },
            .Optional => |inner_sig| {
                const inner_type = try this.typecheckTypeSignature(inner_sig);
                typ = try this.interp.unionizeTypes(&[_]Type{inner_type, .None});
            }
        }

        return typ;
    }

    pub fn typecheck(this: *This, nodes: ArrayListUnmanaged(*Ast)) !ArrayListUnmanaged(*Ast) {
        for (nodes.items) |node| {
            try this.typecheckNodeAndAdd(node);
        }
        return this.nodes;
    }
};
