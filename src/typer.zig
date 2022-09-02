const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const CodeLocation = @import("parser.zig").CodeLocation;

const errors = @import("errors.zig");
const ErrMsg = errors.ErrMsg;
const raise = errors.raise;
const todo = errors.todo;

const BucketArrayUnmanaged = @import("bucket_array.zig").BucketArrayUnmanaged;

const Interpreter = @import("interpreter.zig").Interpreter;

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
const AstIf = ast.AstIf;
const AstWhile = ast.AstWhile;
const AstFor = ast.AstFor;
const AstDef = ast.AstDef;
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
const TupleTypeDefinition = values.TupleTypeDefinition;
const TagTypeDefinition = values.TagTypeDefinition;
const UnionTypeDefinition = values.UnionTypeDefinition;
const LambdaTypeDefinition = values.LambdaTypeDefinition;

const Binding = union(enum) {
    Var: VarBinding,
    Type: Type,
    Tag: Type,
};

const VarBinding = struct {
    typ: Type,
    index: usize,
    global: bool,
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

        if (binding == .Var) {
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

pub const Typer = struct {
    allocator: Allocator,
    interp: *Interpreter,
    nodes: ArrayListUnmanaged(*Ast),

    global_scope: *Scope,
    scopes: BucketArrayUnmanaged(8, Scope),

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

    fn unionizeTypes(this: *This, types: []Type) !Type {
        std.debug.assert(types.len > 0);

        var flattened = ArrayListUnmanaged(Type){};
        defer flattened.deinit(this.allocator);
        {
            for (types) |typ| {
                switch (typ) {
                    .Union => |union_index| {
                        const union_type = this.interp.union_types.items[union_index];
                        for (union_type.variants) |variant| {
                            try flattened.append(this.allocator, variant);
                        }
                    },
                    else => try flattened.append(this.allocator, typ),
                }
            }
        }
        
        if (flattened.items.len == 1) {
            return flattened.items[0];
        }

        // check for equality and Any
        {
            var i: usize = 0;
            while (i < flattened.items.len - 1) : (i += 1) {
                const ta = flattened.items[i];
                const tb = flattened.items[i + 1];

                if (ta == .Any or tb == .Any) {
                    return .Any;
                }

                if (!ta.eql(tb)) {
                    break;
                }
            } else {
                return flattened.items[0];
            }
        }

        var found_index: ?usize = null;
        for (this.interp.union_types.items) |union_type, union_index| {
            if (union_type.variants.len != flattened.items.len)
                continue;
            
            for (union_type.variants) |union_variant| {
                var found = false;
                for (flattened.items) |types_variant| {
                    if (types_variant.eql(union_variant)) {
                        found = true;
                        break;
                    }
                }

                if (!found) 
                    break;
            } else {
                found_index = union_index;
            }
            
            break;
        }

        if (found_index) |union_index| {
            return Type{ .Union = @intCast(u32, union_index) };
        }

        const variants = try this.allocator.alloc(Type, flattened.items.len);
        for (flattened.items) |typ, i| {
            variants[i] = typ;
        }

        const defn = UnionTypeDefinition{ .variants = variants };

        try this.interp.union_types.append(this.allocator, defn);
        return Type{ .Union = @intCast(u32, this.interp.union_types.items.len - 1) };
    }

    fn unionTypeWithVariantsRemoved(this: *This, union_type: UnionTypeDefinition, to_remove: []const Type) !Type {
        var variants = ArrayListUnmanaged(Type){};
        errdefer variants.deinit(this.allocator);

        for (union_type.variants) |variant| {
            for (to_remove) |tr| {
                if (variant.eql(tr)) {
                    break;
                }
            } else {
                try variants.append(this.allocator, variant);
            }
        }

        return try this.unionizeTypes(variants.items);
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
            .Unwrap => {
                const unary = node.downcast(AstUnary);
                t_node = (try this.typecheckUnary(unary)).asAst();
            },

            // Binary
            .Assign,
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
            .NoneOr => {
                const binary = node.downcast(AstBinary);
                t_node = (try this.typecheckBinary(binary)).asAst();
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
            .Def => {
                const def = node.downcast(AstDef);
                t_node = (try this.typecheckDef(def)).asAst();
            },
            .Param => {
                const param = node.downcast(AstParam);
                t_node = (try this.typecheckParam(param)).asAst();
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
                .Var => |var_binding| {
                    var get = try this.allocator.create(AstGetVar);
                    get.* = AstGetVar.init(ident.token, var_binding.typ, var_binding.index, var_binding.global);
                    t_node = get.asAst();
                },
                .Type => |typ| {
                    var get = try this.allocator.create(AstGetType);
                    get.* = AstGetType.init(ident.token, typ);
                    t_node = get.asAst();
                },
                .Tag => |typ| {
                    var get = try this.allocator.create(AstGetTag);
                    get.* = AstGetTag.init(ident.token, typ, ident.ident);
                    t_node = get.asAst();
                },
            }
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

                        unary.typ = try this.unionizeTypes(copied);
                    },
                    .None => return raise(error.TypeError, &this.err_msg, t_sub.token.location, "Unwrap of `None` value will always fail.", .{}),
                    else => return raise(error.TypeError, &this.err_msg, t_sub.token.location, "`?` operator expected a potentially `None` value but encountered a `{}` value.", .{t_sub.typ.?}),
                }
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
            .Assign => {
                if (!t_rhs.?.typ.?.compat(t_lhs.typ.?)) {
                    return raise(error.TypeError, &this.err_msg, t_rhs.?.token.location, "Expected a `{}` value but encountered a `{}` value.", .{t_lhs.typ.?, t_rhs.?.typ.?});
                }
                binary.typ = .None;
            },
            .Add => {
                binary.typ = try this.inferReturnTypeOfArithmetic("+", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
            },
            .Subtract => {
                binary.typ = try this.inferReturnTypeOfArithmetic("-", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
            },
            .Multiply => {
                binary.typ = try this.inferReturnTypeOfArithmetic("*", binary.token.location, t_lhs.typ.?, t_rhs.?.typ.?);
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
                    binary.typ = .Any; // @TODO: Make Range type
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
                            return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "`??` requires it's first operand to be a potentially `None` value but encountered a `{}` value.", .{t_lhs.typ.?});
                        }

                        // @TODO:
                        // Unionize lhs type with rhs type.
                        //

                        binary.typ = try this.unionTypeWithVariantsRemoved(union_type, &[_]Type{.None});
                    },
                    .None => {
                        binary.typ = t_rhs.?.typ.?;
                    },
                    else => return raise(error.TypeError, &this.err_msg, t_lhs.token.location, "`??` requires it's first operand to ba a potentially `None` value but encountered a `{}` value.", .{t_lhs.typ.?}),
                }
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
        var ret_type: Type = undefined;
        if ((type_a == .Int or type_a == .Num) and (type_b == .Int or type_b == .Num)) {
            if (type_a == .Num or type_b == .Num) {
                ret_type = .Num;
            } else {
                ret_type = .Int;
            }
        } else {
            return raise(error.TypeError, &this.err_msg, location, "`" ++ op ++ "` requires both its operands to be either an Int or Num value.", .{}); // @TODO: Error
        }

        return ret_type;
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
            const item_type = try this.unionizeTypes(item_types.items);
            list.typ = try this.interp.findOrAddListType(item_type);
        }

        return list;
    }

    fn typecheckCall(this: *This, call: *AstBinary, lhs: *Ast, rhs: *Ast) !void {
        var typ = lhs.typ.?;
        if (lhs.kind == .GetType) {
            const get = lhs.downcast(AstGetType);
            typ = get.typ_of;
        }

        switch (typ) {
            .Tuple => |tuple_index| {
                const tuple_type = this.interp.tuple_types.items[tuple_index];
                
                std.debug.assert(rhs.kind == .Comma);
                try this.typecheckCallArguments(tuple_type.fields, rhs.downcast(AstBlock));
                
                call.typ = Type{ .Tuple = tuple_index };
            } ,
            .Tag => |_| todo("Implement typecheck for calling tag types."),
            .Lambda => |_| todo("Implement typecheck for calling lambda types."),
            else => return raise(error.TypeError, &this.err_msg, lhs.token.location, "A {} value is not callable.", .{lhs.typ.?}),
        }
    }

    fn typecheckCallArguments(this: *This, params: []NamedParam, args: *AstBlock) !void {
        // @TODO:
        // Implement keyword arguments.
        //
        if (params.len != args.nodes.len) {
            // @TODO: Send first extra args location instead
            return raise(error.TypeError, &this.err_msg, args.token.location, "Incorrect number of arguments! Expected {} but found {}.", .{ params.len, args.nodes.len });
        }

        var i: usize = 0;
        while (i < params.len) : (i += 1) {
            const param = params[i];
            const arg = args.nodes[i];

            if (!arg.typ.?.compat(param.typ)) {
                return raise(error.TypeError, &this.err_msg, arg.token.location, "This argument is a {} value but the parameter `{s}` is specified as a {} value.", .{ arg.typ.?, param.name, param.typ });
            }
        }
    }

    fn typecheckDot(this: *This, dot: *AstBinary) !*AstBinary {
        const t_lhs = (try this.typecheckNode(dot.lhs)).?;

        std.debug.print("rhs.kind = {}", .{dot.rhs.kind});
        std.debug.assert(dot.rhs.kind == .Ident);
        const ident = dot.rhs.downcast(AstIdent);

        switch (t_lhs.typ.?) {
            .Tuple => |tuple_index| {
                const tuple_type = this.interp.tuple_types.items[tuple_index];

                // find index of field
                var field_index: ?usize = null;
                for (tuple_type.fields) |field, i| {
                    if (std.mem.eql(u8, field.name, ident.ident)) {
                        field_index = i;
                        break;
                    }
                }

                if (field_index) |idx| {
                    dot.lhs = t_lhs;
                    dot.typ = tuple_type.fields[idx].typ;
                    return dot;
                }
            },
            .Tag => |tag_index| {
                _ = tag_index;
                todo("Implement typechecking dot for tuples.");
            },
            else => {},
        }

        todo("Implement typechecking dot for associated functions.");
    }

    fn typecheckIf(this: *This, _if: *AstIf) !*AstIf {
        const t_cond = (try this.typecheckNode(_if.condition)).?;
        if (t_cond.typ.? != .Bool) {
            return raise(error.TypeError, &this.err_msg, t_cond.token.location, "`if` expression expected a `Bool` condition value but encountered a `{}` value.", .{t_cond.typ.?});
        }

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
        _if.typ = try this.unionizeTypes(&[_]Type{t_then.?.typ.?, if (t_else) |te| te.typ.? else .None});
        return _if;
    }

    

    fn typecheckWhile(this: *This, _while: *AstWhile) !*AstWhile {
        const t_cond = (try this.typecheckNode(_while.condition)).?;
        if (t_cond.typ.? != .Bool) {
            return raise(error.TypeError, &this.err_msg, t_cond.token.location, "`while` expression expected a `Bool` condition value but encountered a `{}` value.", .{t_cond.typ.?});
        }

        var t_block: ?*AstBlock = null;
        t_block = try this.typecheckBlock(_while.block);

        _while.condition = t_cond;
        _while.block = t_block.?;
        _while.typ = t_block.?.typ;
        return _while;
    }

    fn typecheckFor(this: *This, _for: *AstFor) !*AstFor {
        _ = this;
        _ = _for;
        todo("Implement typecheckFor().");
    }

    fn typecheckDef(this: *This, def: *AstDef) !*AstDef {
        _ = this;
        _ = def;
        todo("Implement typecheckDef().");
    }

    fn typecheckParam(this: *This, param: *AstParam) !*AstParam {
        _ = this;
        _ = param;
        todo("Implement typecheckParam().");
    }

    fn typecheckVar(this: *This, _var: *AstVar) !*AstVar {
        const t_init = (try this.typecheckNode(_var.initializer)).?;

        var var_type = t_init.typ.?;
        if (_var.specified_type) |specified_type_signature| {
            const specified_type = try this.typecheckTypeSignature(specified_type_signature);
            if (!t_init.typ.?.compat(specified_type)) {
                return raise(error.TypeError, &this.err_msg, t_init.token.location, "Cannot initialize a variable with a `{}` value when it was specified to be a `{}` value.", .{ t_init.typ.?, specified_type });
            }

            var_type = specified_type;
        }

        const current_scope = this.currentScope();
        const binding = Binding{ .Var = .{ .typ = var_type, .index = current_scope.num_vars, .global = true } }; // @TODO: Actually determine if global or not by comparing to current functions scope
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
            .Tuple => |tuple_data| {
                std.debug.assert(tuple_data.field_names.len == tuple_data.field_types.len);

                const fields = try this.allocator.alloc(TupleTypeDefinition.Field, tuple_data.field_names.len);
                var i: usize = 0;
                while (i < tuple_data.field_names.len) : (i += 1) {
                    const field_name = tuple_data.field_names[i];
                    const field_type: Type = if (tuple_data.field_types[i]) |ft| 
                        try this.typecheckTypeSignature(ft)
                    else
                        Type.Any;

                    fields[i] = TupleTypeDefinition.Field{ .name = field_name.ident, .typ = field_type };
                }

                typ = try this.interp.findOrAddTupleType(fields);
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

                typ = try this.unionizeTypes(variants);
            },
            .Optional => |inner_sig| {
                const inner_type = try this.typecheckTypeSignature(inner_sig);
                typ = try this.unionizeTypes(&[_]Type{inner_type, .None});
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
