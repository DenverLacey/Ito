const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMap = std.StringArrayHashMap;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;

const interpreter = @import("interpreter.zig");
const Interpreter = interpreter.Interpreter;
const LambdaDefinition = interpreter.LambdaDefinition;

const CodeLocation = @import("parser.zig").CodeLocation;

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

const errors = @import("errors.zig");
const ErrMsg = errors.ErrMsg;
const raise = errors.raise;
const todo = errors.todo;

const values = @import("values.zig");
const Type = values.Type;
const TypeDefinition = values.TypeDefinition;
const RecordTypeDefinition = values.RecordTypeDefinition;
const TagTypeDefinition = values.TagTypeDefinition;
const UnionTypeDefinition = values.UnionTypeDefinition;
const LambdaTypeDefinition = values.LambdaTypeDefinition;
const Value = values.Value;
const ValueKind = values.ValueKind;
const Char = values.Char;
const Range = values.Range;
const List = values.List;
const Closure = values.Closure;
const Record = values.Record;
const Tag = values.Tag;

const GarbageCollector = @import("gc.zig").GarbageCollector;


const DEBUG_PRINT_BASE_NODE_RESULTS = true;
const DEBUG_PRINT_SCOPE_VAR_LOOKUP  = false;

const ReturnSignals = error{ReturnSignal, BreakSignal};


pub const Scope = struct {
    parent: ?*This,
    start: usize,
    end: usize,

    const This = @This();

    fn init(parent: ?*This) This {
        var this: This = undefined;
        this.parent = parent;
        this.start = if (parent) |p| p.end else 0;
        this.end = this.start;
        return this;
    }
};

pub const VariableStack = struct {
    names: ArrayListUnmanaged([]const u8) = .{},
    values: ArrayListUnmanaged(Value) = .{},

    const This = @This();

    fn pushVariable(
        this: *This,
        allocator: Allocator,
        scope: *Scope,
        ident: []const u8,
        value: Value,
    ) !void {
        std.debug.assert(this.values.items.len == scope.end); // @NOTE: We expect to only add variables to the current scope which should be at the end of the variable stack

        try this.names.append(allocator, ident);
        try this.values.append(allocator, value);
        scope.end += 1;
    }

    fn popScope(this: *This, scope: Scope) void {
        this.names.shrinkRetainingCapacity(scope.start);
        this.values.shrinkRetainingCapacity(scope.start);
    }
};

const CallFrame = struct {
    start: usize,
};

pub const Evaluator = struct {
    allocator: Allocator,
    interp: *Interpreter,
    gc: GarbageCollector,
    scopes: ArrayListUnmanaged(Scope),
    stack: VariableStack,
    call_frames: ArrayListUnmanaged(CallFrame),
    err_msg: ErrMsg,

    const This = @This();

    pub fn init(interp: *Interpreter) anyerror!This {
        var scopes = ArrayListUnmanaged(Scope){};
        try scopes.append(interp.allocator, Scope.init(null));

        return Evaluator{
            .allocator = interp.allocator,
            .interp = interp,
            .gc = GarbageCollector.init(interp.allocator),
            .scopes = scopes,
            .stack = VariableStack{},
            .call_frames = ArrayListUnmanaged(CallFrame){},
            .err_msg = ErrMsg{},
        };
    }

    pub fn deinit(this: *This) void {
        this.gc.deinit();
    }

    fn globalScope(this: *This) *Scope {
        std.debug.assert(this.scopes.items.len != 0);
        return &this.scopes.items[0];
    }

    fn currentScope(this: *This) *Scope {
        std.debug.assert(this.scopes.items.len != 0);
        return &this.scopes.items[this.scopes.items.len - 1];
    }

    fn beginScope(this: *This) !void {
        return this.pushScope(Scope.init(this.currentScope()));
    }

    fn pushScope(this: *This, scope: Scope) !void {
        try this.scopes.append(this.allocator, scope);
    }

    fn endScope(this: *This) void {
        std.debug.assert(this.scopes.items.len != 1); // Guarentee that we're not trying to end the global scope

        const scope = this.scopes.pop();
        this.stack.popScope(scope);

        this.gc.collectGarbage(this.stack.values.items);
    }

    fn pushCallFrame(this: *This) !void {
        const new_frame = CallFrame{ .start = this.stack.values.items.len };
        try this.call_frames.append(this.allocator, new_frame);
    }

    fn popCallFrame(this: *This) void {
        _ = this.call_frames.pop();
    }

    fn currentFrame(this: *This) ?CallFrame {
        if (this.call_frames.items.len == 0) {
            return null;
        }
        return this.call_frames.items[this.call_frames.items.len - 1];
    }

    pub fn evaluate(this: *This, nodes: []*Ast) anyerror!void {
        for (nodes) |node| {
            const v = try this.evaluateNode(node);
            if (DEBUG_PRINT_BASE_NODE_RESULTS) {
                if (node.typ) |typ| {
                    std.debug.print("?> {} :: {}", .{v, typ});
                    std.debug.print("\n", .{});
                }
            }
        }
    }

    fn evaluateNode(this: *This, node: *Ast) anyerror!Value {
        switch (node.kind) {
            // Literals
            .None, .Bool, .Char, .Int, .Num, .Str => {
                const l = node.downcast(AstLiteral);
                return try this.evaluateLiteral(l);
            },
            .Ident => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Ident node reached evaluation.", .{});
            },

            // Unary
            .Negate,
            .Not,
            .Unwrap,
            .Show => {
                const unary = node.downcast(AstUnary);
                return try this.evaluateUnary(unary);
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
            .Dot,
            .ExclusiveRange,
            .NoneOr,
            .Format,
            .PartialCopy => {
                const binary = node.downcast(AstBinary);
                return try this.evaluateBinary(binary);
            },

            .Assign => {
                const assign = node.downcast(AstBinary);
                try this.evaluateAssign(assign);
                return Value.None;
            },

            .Bind => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "Bind node reached evaluation.", .{});
            },
            .CaseBranch => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "CaseBranch node reached evaluation.", .{});
            },

            // Blocks
            .Block, .Comma => {
                const block = node.downcast(AstBlock);
                return try this.evaluateBlock(block);
            },
            .List => {
                const list = node.downcast(AstBlock);
                return try this.evaluateList(list);
            },
            .Record => {
                const record = node.downcast(AstBlock);
                return try this.evaluateRecordLiteral(record);
            },

            // Returns
            .Return => {
                const ret = node.downcast(AstReturn);
                try this.evaluateReturn(ret, error.ReturnSignal);
                return Value.None;
            },
            .Break => {
                const _break = node.downcast(AstReturn);
                try this.evaluateReturn(_break, error.BreakSignal);
                return Value.None;
            },

            .If => {
                const _if = node.downcast(AstIf);
                return try this.evaluateIf(_if);
            },
            .While => {
                const _while = node.downcast(AstWhile);
                return try this.evaluateWhile(_while);
            },
            .For => {
                const _for = node.downcast(AstFor);
                return try this.evaluateFor(_for);
            },
            .Case => {
                const case = node.downcast(AstCase);
                return try this.evaluateCase(case);
            },
            .Continue => return error.ContinueSignal,
            .Def => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "AstDef node reached evaluation.", .{});
            },
            .InstantiateLambda => {
                const lambda = node.downcast(AstInstantiateLambda);
                try this.evaluateInstantiateLambda(lambda);
                return Value.None;
            },
            // .Extend => blk: {
            //     const extend = node.downcast(AstExtend);
            //     try this.evaluateExtend(extend);
            //     break :blk Value.None;
            // },
            .Var => {
                const _var = node.downcast(AstVar);
                try this.evaluateVar(_var);
                return Value.None;
            },
            .Param => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "AstParam node evaluated outside def context.", .{});  
            },
            .VarBlock => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "AstVarBlock node reached evaluation.", .{});
            },
            .Type => {
                return Value.None;
            },
            .TypeBlock => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "AstTypeBlock node reached evaluation.", .{});
            },
            .TypeSignature => {
                return raise(error.InternalError, &this.err_msg, node.token.location, "AstTypeSignature node reached evaluation.", .{});
            },

            // Typechecked Specific
            .GetGlobal => {
                const get = node.downcast(AstGetVar);
                return this.stack.values.items[get.index];
            },
            .GetVar => {
                const get = node.downcast(AstGetVar);
                const frame = this.currentFrame() orelse return raise(error.InternalError, &this.err_msg, get.token.location, "Attempt to evaluate GetVar node without a call frame.", .{});
                return this.stack.values.items[get.index + frame.start];
            },
            .GetType => {
                const get = node.downcast(AstGetType);
                return Value{ .Type = get.typ_of };
            },
            .GetTag => {
                const get = node.downcast(AstGetTag);
                const allocated = try this.gc.copyTag(Tag{ .tag = get.tag_name });
                return Value{ .Tag = allocated };
            },
        }
    }

    fn evaluateLiteral(this: *This, literal: *AstLiteral) anyerror!Value {
        return switch (literal.literal) {
            .None => Value.None,
            .Bool => |value| Value{ .Bool = value },
            .Char => |value| Value{ .Char = value },
            .Int => |value| Value{ .Int = value },
            .Num => |value| Value{ .Num = value },
            .Str => |value| blk: {
                const allocated = try this.gc.copyString(value);
                const v = Value{ .Str = allocated };
                break :blk v;
            },
        };
    }

    fn evaluateIdent(this: *This, ident: *AstIdent) anyerror!Value {
        if (this.currentScope().findVariable(ident.ident)) |variable_ptr| {
            return variable_ptr.*;
        }
        return raise(error.RuntimeError, &this.err_msg, ident.token.location, "Unknown identifier!", .{});
    }

    fn evaluateUnary(this: *This, unary: *AstUnary) anyerror!Value {
        return switch (unary.kind) {
            .Negate => this.evaluateNegate(unary.sub),
            .Not => this.evaluateNot(unary.sub),
            .Unwrap => this.evaluateUnwrap(unary.sub),
            .Show => this.evaluateShow(unary.sub),
            else => raise(error.RuntimeError, &this.err_msg, unary.token.location, "Invalid unary operation", .{}),
        };
    }

    fn evaluateNegate(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);

        return switch (sub) {
            .Int => |value| Value{ .Int = -value },
            .Num => |value| Value{ .Num = -value },
            else => raise(error.RuntimeError, &this.err_msg, sub_node.token.location, "`-` requires its operand to be either an `Int` or a `Num`.", .{}),
        };
    }

    fn evaluateNot(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);
        return switch (sub) {
            .Bool => |value| Value{ .Bool = !value },
            else => raise(error.RuntimeError, &this.err_msg, sub_node.token.location, "`!` requires its operand to be a `Bool`.", .{}),
        };
    }

    fn evaluateUnwrap(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);
        return if (sub == .None)
            raise(error.RuntimeError, &this.err_msg, sub_node.token.location, "Attempted unwrap of a `None` value.", .{})
        else
            sub;
    }

    fn evaluateShow(this: *This, sub_node: *Ast) anyerror!Value {
        const sub = try this.evaluateNode(sub_node);

        const s = try std.fmt.allocPrint(this.allocator, "{}", .{sub});
        defer this.allocator.free(s);

        const rval = Value{ .Str = try this.gc.copyString(s) };
        return rval;
    }

    // fn evaluatePrintln(this: *This, sub_node: *Ast) anyerror!void {
    //     const args = sub_node.downcast(AstBlock);
    //     var stdout = std.io.getStdOut().writer();

    //     for (args.nodes) |node| {
    //         const arg = try this.evaluateNode(node);
    //         try stdout.print("{} ", .{arg});
    //     }

    //     _ = try stdout.write("\n");
    // }

    fn evaluateBinary(this: *This, binary: *AstBinary) anyerror!Value {
        return switch (binary.kind) {
            .Add => this.evaluateAdd(binary.lhs, binary.rhs),
            .Subtract => this.evaluateSubtract(binary.lhs, binary.rhs),
            .Multiply => this.evaluateMultiply(binary.lhs, binary.rhs),
            .Divide => this.evaluateDivide(binary.lhs, binary.rhs),
            .Equal => this.evaluateEqual(binary.lhs, binary.rhs),
            .NotEqual => this.evaluateNotEqual(binary.lhs, binary.rhs),
            .LessThan => this.evaluateLessThan(binary.lhs, binary.rhs),
            .GreaterThan => this.evaluateGreaterThan(binary.lhs, binary.rhs),
            .Or => this.evaluateOr(binary.lhs, binary.rhs),
            .And => this.evaluateAnd(binary.lhs, binary.rhs),
            .Index => this.evaluateIndex(binary.lhs, binary.rhs),
            .Call => this.evaluateCall(binary.lhs, binary.rhs.downcast(AstBlock)),
            .Dot => this.evaluateDot(binary.lhs, binary.rhs.downcast(AstIdent)),
            .ExclusiveRange => this.evaluateExclusiveRange(binary.lhs, binary.rhs),
            .NoneOr => this.evaluateNoneOr(binary.lhs, binary.rhs),
            .Format => this.evaluateFormat(binary.lhs, binary.rhs),
            .PartialCopy => this.evaluatePartialCopy(binary.lhs, binary.rhs),
            else => raise(error.RuntimeError, &this.err_msg, binary.token.location, "Invalid binary operation", .{}),
        };
    }

    fn evaluateAdd(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const error_message = "`+` requires its first operand to be either an `Int`, `Num` or `Str` value.";

        switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| return Value{ .Int = lhs_value + rhs_value },
                .Num => |rhs_value| return Value{ .Num = @intToFloat(f64, lhs_value) + rhs_value },
                else => return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, error_message, .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| return Value{ .Num = lhs_value + @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| return Value{ .Num = lhs_value + rhs_value },
                else => return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, error_message, .{}),
            },
            .Str => |lhs_value| switch (rhs) {
                .Str => |rhs_value| {
                    const allocated_str = try std.mem.concat(this.allocator, u8, &[_][]const u8{ lhs_value, rhs_value });
                    try this.gc.manageString(allocated_str);
                    return Value{ .Str = allocated_str };
                },
                else => return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, error_message, .{}),
            },
            else => return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, error_message, .{}),
        }
    }

    fn evaluateSubtract(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = lhs_value - rhs_value },
                .Num => |rhs_value| Value{ .Num = @intToFloat(f64, lhs_value) - rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = lhs_value - @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Num = lhs_value - rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`-` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            else => raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`-` requires its first operand to be either an `Int` or a `Num`.", .{}),
        };
    }

    fn evaluateMultiply(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const error_message = "`*` requires its first operand to be either an `Int`, `Num` or `Str` value.";

        switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| return Value{ .Int = lhs_value * rhs_value },
                .Num => |rhs_value| return Value{ .Num = @intToFloat(f64, lhs_value) * rhs_value },
                .Str => |rhs_value| return this.evaluateMultiplyStr(rhs_value, lhs_value, lhs_node.token.location),
                else => return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, error_message, .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| return Value{ .Num = lhs_value * @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| return Value{ .Num = lhs_value * rhs_value },
                else => return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, error_message, .{}),
            },
            .Str => |lhs_value| switch (rhs) {
                .Int => |rhs_value| return this.evaluateMultiplyStr(lhs_value, rhs_value, rhs_node.token.location),
                else => return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, error_message, .{}),
            },
            else => return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, error_message, .{}),
        }
    }

    fn evaluateMultiplyStr(this: *This, s: []const u8, n: i64, n_location: CodeLocation) !Value {
        if (n < 0) {
            return raise(error.RuntimeError, &this.err_msg, n_location, "Can only multiply a string by a non-negative integer but {} was the actual value.", .{n});
        } else if (n == 0) {
            // @TODO: Implement optimisation
        }

        const _n = @intCast(usize, n);
        const allocated_str = try this.allocator.alloc(u8, s.len * _n);

        var i: usize = 0;
        while (i < _n) : (i += 1) {
            const offset = s.len * i;
            var dest = allocated_str[offset..offset + s.len];
            std.mem.copy(u8, dest, s);
        }

        try this.gc.manageString(allocated_str);
        return Value{ .Str = allocated_str };
    }

    fn evaluateDivide(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Int = @divTrunc(lhs_value, rhs_value) },
                .Num => |rhs_value| Value{ .Num = @divTrunc(@intToFloat(f64, lhs_value), rhs_value) },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Num = @divTrunc(lhs_value, @intToFloat(f64, rhs_value)) },
                .Num => |rhs_value| Value{ .Num = @divTrunc(lhs_value, rhs_value) },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`/` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            else => raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`/` requires its first operand to be either an `Int` or a `Num`.", .{}),
        };
    }

    fn evaluateEqual(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.eql(rhs);
        return Value{ .Bool = equal };
    }

    fn evaluateNotEqual(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        const equal = lhs.eql(rhs);
        return Value{ .Bool = !equal };
    }

    fn evaluateLessThan(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value < rhs_value },
                .Num => |rhs_value| Value{ .Bool = @intToFloat(f64, lhs_value) < rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`<` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value < @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Bool = lhs_value < rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`<` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            else => raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`<` requires its first operand to be either an `Int` or a `Num`.", .{}),
        };
    }

    fn evaluateGreaterThan(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        const rhs = try this.evaluateNode(rhs_node);

        return switch (lhs) {
            .Int => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value > rhs_value },
                .Num => |rhs_value| Value{ .Bool = @intToFloat(f64, lhs_value) > rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`>` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            .Num => |lhs_value| switch (rhs) {
                .Int => |rhs_value| Value{ .Bool = lhs_value > @intToFloat(f64, rhs_value) },
                .Num => |rhs_value| Value{ .Bool = lhs_value > rhs_value },
                else => raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`>` requires its second operand to be either an `Int` or a `Num`.", .{}),
            },
            else => raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`>` requires its first operand to be either an `Int` or a `Num`.", .{}),
        };
    }

    fn evaluateOr(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        if (lhs != .Bool) {
            return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`or` requires its first operand to be a Bool value.", .{});
        }

        const lhs_value = switch (lhs) { .Bool => |value| value, else => unreachable };
        if (lhs_value) {
            return Value{ .Bool = true };
        }

        const rhs = try this.evaluateNode(rhs_node);
        if (rhs != .Bool) {
            return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`or` requires its second operand to be a Bool value.", .{});
        }

        return rhs;
    }

    fn evaluateAnd(this: *This, lhs_node: *Ast, rhs_node: *Ast) anyerror!Value {
        const lhs = try this.evaluateNode(lhs_node);
        if (lhs != .Bool) {
            return raise(error.RuntimeError, &this.err_msg, lhs_node.token.location, "`and` requires its first operand to be a Bool value.", .{});
        }

        const lhs_value = switch (lhs) { .Bool => |value| value, else => unreachable };
        if (!lhs_value) {
            return Value{ .Bool = false };
        }

        const rhs = try this.evaluateNode(rhs_node);
        if (rhs != .Bool) {
            return raise(error.RuntimeError, &this.err_msg, rhs_node.token.location, "`and` requires its second operand to be a Bool value.", .{});
        }

        return rhs;
    }

    fn evaluateIndex(this: *This, container_node: *Ast, index_node: *Ast) anyerror!Value {
        const container = try this.evaluateNode(container_node);

        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, &this.err_msg, index_node.token.location, "Cannot index a container with something other than an `Int`.", .{}),
        };

        return switch (container) {
            .Str => |value| this.evaluateIndexStr(value, index, index_node.token.location),
            .List => |value| this.evaluateIndexList(value, index, index_node.token.location),
            else => raise(error.RuntimeError, &this.err_msg, container_node.token.location, "`[` requires its first operand to be either a 'Str' or a 'List'.", .{}),
        };
    }

    fn evaluateIndexStr(this: *This, string: []const u8, index: i64, location: CodeLocation) anyerror!Value {
        var chars = (try Utf8View.init(string)).iterator();

        var i: usize = 0;
        while (i < index) : (i += 1) {
            _ = chars.nextCodepoint();
        }

        const opt_char = chars.nextCodepoint();
        if (opt_char == null) {
            return raise(error.RuntimeError, &this.err_msg, location, "Array bounds check failure!", .{});
        }

        const char = opt_char.?;

        return Value{ .Char = char };
    }

    fn evaluateIndexList(this: *This, list: *List, index: i64, index_location: CodeLocation) anyerror!Value {
        try arrayBoundsCheck(Value, list.items, @intCast(usize, index), index_location, &this.err_msg);
        return list.items[@intCast(usize, index)];
    }

    fn evaluateCall(this: *This, callable_node: *Ast, args_node: *AstBlock) anyerror!Value {
        const callable = try this.evaluateNode(callable_node);

        return switch (callable) {
            .Closure => |closure| this.evaluateCallClosure(closure, args_node),
            .Type => |typ| this.evaluateCallType(typ, args_node),
            else => raise(error.RuntimeError, &this.err_msg, callable_node.token.location, "Cannot call something that isn't a `Closure`.", .{}),
        };
    }

    fn setupScopeForClosureCall(this: *This, scope: *Scope, closure: *Closure, args: *AstBlock) anyerror!void {
        try this.injectClosedValuesIntoScope(closure, scope);

        if (args.nodes.len != closure.type_def.parameters.len) {
            return raise(error.RuntimeError, &this.err_msg, args.token.location, "Inccorect number of arguments! `{s}` expects {} arguments but was given {}.", .{ closure.name, closure.type_def.parameters.len, args.nodes.len });
        }

        var i: usize = 0;
        while (i < closure.type_def.parameters.len) : (i += 1) {
            const arg_name = closure.type_def.parameters[i].name;
            const arg_value = try this.evaluateNode(args.nodes[i]);
            try this.stack.pushVariable(this.allocator, scope, arg_name, arg_value);
        }
    }

    fn injectClosedValuesIntoScope(this: *This, closure: *Closure, scope: *Scope) anyerror!void {
        var i: usize = 0;
        while (i < closure.closed_values.len) : (i += 1) {
            const name = closure.closed_value_names[i];
            const value = closure.closed_values[i];

            try this.stack.pushVariable(this.allocator, scope, name, value);
        }
    }

    fn callClosure(this: *This, closure: *Closure) anyerror!Value {
        // @TODO:
        // Handle early return
        //
        return this.evaluateBlock(closure.code);
    }

    fn evaluateCallClosure(
        this: *This,
        closure: *Closure,
        args_node: *AstBlock,
    ) anyerror!Value {
        var closure_scope = Scope.init(this.globalScope());
        
        try this.pushCallFrame();
        defer this.popCallFrame();

        try this.stack.pushVariable(this.allocator, &closure_scope, closure.name, Value{ .Closure = closure });

        try this.setupScopeForClosureCall(&closure_scope, closure, args_node);

        try this.pushScope(closure_scope);
        defer this.endScope();

        return this.callClosure(closure);
    }

    fn evaluateCallType(
        this: *This,
        typ: Type,
        args: *AstBlock,
    ) anyerror!Value {
        switch (typ) {
            .Record => |record_index| {
                const record_type = this.interp.record_types.items[record_index];
                return this.evaluateCallRecordType(typ, record_type, args);
            },
            .Tag => |tag_index| {
                const tag_type = this.interp.tag_types.items[tag_index];
                return this.evaluateCallTagType(typ, tag_type, args);
            },
            else => unreachable,
        }
    }

    fn evaluateCallRecordType(this: *This, typ: Type, defn: RecordTypeDefinition, args: *AstBlock) anyerror!Value {
        if (args.nodes.len != defn.fields.len) {
            return raise(error.RuntimeError, &this.err_msg, args.token.location, "Incorrect number of arguments! Expected {} but found {}.", .{ defn.fields.len, args.nodes.len });
        }
        
        var fields = StringArrayHashMapUnmanaged(Value){};

        for (defn.fields) |field, i| {
            const field_value = try this.evaluateNode(args.nodes[i]);
            try fields.putNoClobber(this.allocator, field.name, field_value);
        }

        const record_value = Record{ .typ = typ, .fields = fields };
        const allocated_record = try this.gc.copyRecord(record_value);
        return Value{ .Record = allocated_record };
    }

    fn evaluateCallTagType(this: *This, typ: Type, defn: TagTypeDefinition, args: *AstBlock) anyerror!Value {
        _ = this;
        _ = typ;
        _ = defn;
        _ = args;
        todo("Implement evaluateCallTagType().");
    }

    fn evaluateDot(this: *This, instance_node: *Ast, field_ident_node: *AstIdent) anyerror!Value {
        const instance_value = try this.evaluateNode(instance_node);

        return switch (instance_value) {
            .List => |list| this.evaluateDotList(list, field_ident_node),
            .Type => |_type| this.evaluateDotType(_type, field_ident_node),
            .Record => |record| this.evaluateDotRecord(record, field_ident_node),
            else => raise(error.RuntimeError, &this.err_msg, instance_node.token.location, "`.` requires its first operand to be an instance of a struct.", .{}),
        };
    }

    fn evaluateDotList(this: *This, list: *List, field_ident_node: *AstIdent) anyerror!Value {
        const ident = field_ident_node.ident;

        if (std.mem.eql(u8, ident, "len")) {
            return Value{ .Int = @intCast(i64, list.items.len) };
        } else {
            return raise(error.RuntimeError, &this.err_msg, field_ident_node.token.location, "List does not have a field with this name.", .{});
        }
    }

    fn evaluateDotType(this: *This, typ: Type, field_idenet_node: *AstIdent) anyerror!Value {
        const defn = switch (typ) {
            .Record => |record_index| this.interp.record_types.items[record_index],
            else => unreachable,
        };

        _ = field_idenet_node;
        _ = defn;
        todo("reimplement dot operator for `structs` which would now be a Type");

        // const ident = field_ident_node.ident;

        // if (_struct.methods.get(ident)) |method| {
        //     const receiver = Value{ .Struct = _struct };
        //     const bound_closure = try method.makeBound(this.allocator, receiver, field_ident_node.token.location, &this.err_msg);
        //     const allocated_bound_closure = try this.gc.allocateClosure(bound_closure);
        //     return Value{ .Closure = allocated_bound_closure };
        // } else {
        //     return raise(error.RuntimeError, field_ident_node.token.location, "Struct does not have a method with this name.", &this.err_msg);
        // }
    }

    fn evaluateDotRecord(
        this: *This,
        record: *Record,
        field_ident_node: *AstIdent,
    ) anyerror!Value {
        const ident = field_ident_node.ident;

        if (record.fields.getPtr(ident)) |field_ptr| {
            return field_ptr.*;
        } 
        // else if (instance._struct.methods.get(ident)) |method| {
        //     const receiver = Value{ .Instance = instance };
        //     const bound_closure = try method.makeBound(this.allocator, receiver, field_ident_node.token.location, &this.err_msg);
        //     const allocated_bound_closure = try this.gc.allocateClosure(bound_closure);
        //     return Value{ .Closure = allocated_bound_closure };
        // } 
        else {
            return raise(error.RuntimeError, &this.err_msg, field_ident_node.token.location, "Record does not have a field with this name.", .{});
        }
    }

    fn evaluateExclusiveRange(this: *This, start_node: *Ast, end_node: *Ast) anyerror!Value {
        const start_value = try this.evaluateNode(start_node);
        const end_value   = try this.evaluateNode(end_node);

        const start = switch (start_value) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, &this.err_msg, start_node.token.location, "Can only make ranges of integers.", .{}),
        };
        const end = switch (end_value) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, &this.err_msg, end_node.token.location, "Can only make ranges of integers.", .{}),
        };

        return Value{ .Range = Range.init(start, end) };
    }

    fn evaluateNoneOr(this: *This, lhs: *Ast, rhs: *Ast) anyerror!Value {
        const lhs_value = try this.evaluateNode(lhs);

        if (lhs_value == .None) {
            return try this.evaluateNode(rhs);
        }

        return lhs_value;
    }

    fn evaluateFormat(this: *This, lhs: *Ast, rhs: *Ast) anyerror!Value {
        _ = this;
        _ = lhs;
        _ = rhs;
        todo("Implement evaluateFormat");
    }

    fn evaluatePartialCopy(this: *This, lhs: *Ast, rhs: *Ast) anyerror!Value {
        const source_value = try this.evaluateNode(lhs);
        const source = switch (source_value) {
            .Record => |record| record,
            else => return raise(error.InternalError, &this.err_msg, lhs.token.location, "Non-record value reached evaluation of partial copy node.", .{}),
        };

        var copied = Record{ .typ = lhs.typ.?, .fields = try source.fields.clone(this.allocator) };

        if (rhs.kind != .Comma) {
            return raise(error.InternalError, &this.err_msg, rhs.token.location, "rhs of partial copy node not a comma node and reached evaluation.", .{});
        }

        const field_nodes = rhs.downcast(AstBlock);
        for (field_nodes.nodes) |node| {
            if (node.kind != .Bind) {
                return raise(error.InternalError, &this.err_msg, node.token.location, "field node in partial copy not a bind node and reached evaluation.", .{});
            }

            const bind = node.downcast(AstBinary);

            if (bind.lhs.kind != .Ident) {
                return raise(error.InternalError, &this.err_msg, bind.lhs.token.location, "field node lhs in partial copy not an ident node and reached evaluation.", .{});
            }

            const field_ident = bind.lhs.downcast(AstIdent);

            try copied.fields.put(this.allocator, field_ident.ident, try this.evaluateNode(bind.rhs));
        }

        const allocated_copy = try this.gc.copyRecord(copied);
        return Value{ .Record = allocated_copy };
    }

    fn evaluateAssign(this: *This, assign: *AstBinary) anyerror!void {
        switch (assign.lhs.kind) {
            .GetGlobal, .GetVar => try this.evaluateAssignVariable(assign.lhs.downcast(AstGetVar), assign.rhs),
            .Index => try this.evaluateAssignIndex(assign.lhs.downcast(AstBinary), assign.rhs),
            .Dot => try this.evaluateAssignDot(assign.lhs.downcast(AstBinary), assign.rhs),
            else => {
                std.debug.print("{}", .{assign.lhs.kind});
                return raise(error.RuntimeError, &this.err_msg, assign.lhs.token.location, "Cannot assign to this kind of expression!", .{});
            },
        }
    }

    fn evaluateAssignVariable(this: *This, get: *AstGetVar, expr: *Ast) anyerror!void {
        const index = switch (get.kind) {
            .GetGlobal => get.index,
            .GetVar => (this.currentFrame() orelse return raise(error.InternalError, &this.err_msg, get.token.location, "Attempt to evaluate assignment to GetVar node without a call frame.", .{})).start + get.index,
            else => unreachable,
        };

        this.stack.values.items[index] = try this.evaluateNode(expr);
    }

    fn evaluateAssignIndex(this: *This, target: *AstBinary, expr: *Ast) anyerror!void {
        const container = try this.evaluateNode(target.lhs);

        switch (container) {
            .Str => todo("Implement `evaluateAssignIndex()` for strings."),
            .List => |value| try this.evaluateAssignIndexList(value, target.rhs, expr),
            else => return raise(error.RuntimeError, &this.err_msg, target.lhs.token.location, "Cannot index something that isn't a `Str` or a `List`.", .{}),
        }
    }

    fn evaluateAssignDot(this: *This, target: *AstBinary, expr: *Ast) anyerror!void {
        const record = try this.evaluateNode(target.lhs);

        switch (record) {
            .Record => |tp| try this.evaluateAssignDotRecord(tp, target.rhs.downcast(AstIdent), expr),
            else => return raise(error.RuntimeError, &this.err_msg, target.lhs.token.location, "`.` requires its first operand to be a record value.", .{}),
        }
    }

    fn evaluateAssignDotRecord(
        this: *This,
        record: *Record,
        field_ident_node: *AstIdent,
        expr: *Ast,
    ) anyerror!void {
        const field_ident = field_ident_node.ident;
        var field_ptr = record.fields.getPtr(field_ident) orelse return raise(error.RuntimeError, &this.err_msg, field_ident_node.token.location, "Record does not have a field with this name.", .{});

        field_ptr.* = try this.evaluateNode(expr);
    }

    fn evaluateAssignIndexList(this: *This, list: *List, index_node: *Ast, expr: *Ast) anyerror!void {
        const index = switch (try this.evaluateNode(index_node)) {
            .Int => |value| value,
            else => return raise(error.RuntimeError, &this.err_msg, index_node.token.location, "Cannot index a container with something other than an `Int`.", .{}),
        };

        const value_ptr = &list.items[@intCast(usize, index)];

        const new_value = try this.evaluateNode(expr);
        value_ptr.* = new_value;
    }

    fn evaluateBlock(this: *This, block: *AstBlock) anyerror!Value {
        var trval: Value = .None;

        if (block.kind == .Block) {
            try this.beginScope();
        }

        defer if (block.kind == .Block) {
            this.endScope();
        };

        for (block.nodes) |node| {
            trval = try this.evaluateNode(node);
        }

        return trval;
    }

    fn evaluateList(this: *This, list: *AstBlock) anyerror!Value {
        var items = try this.gc.allocateList();
        items.* = try List.initCapacity(this.allocator, list.nodes.len);

        for (list.nodes) |node| {
            try items.append(this.allocator, try this.evaluateNode(node));
        }

        return Value{ .List = items };
    }

    fn evaluateRecordLiteral(this: *This, record: *AstBlock) anyerror!Value {
        const record_index = switch (record.typ.?) {
            .Record => |ti| ti,
            else => unreachable,
        };

        const record_type = this.interp.record_types.items[record_index];
        return try this.evaluateCallRecordType(record.typ.?, record_type, record);
    }

    fn evaluateReturn(this: *This, ret: *AstReturn, signal: ReturnSignals) anyerror!void {
        if (ret.sub) |sub| {
            const return_value = try this.evaluateNode(sub);
            // @TODO: Set return value
            std.debug.print(">>> return_value = {}", .{return_value});
        }

        return signal;
    }

    fn evaluateIf(this: *This, _if: *AstIf) anyerror!Value {
        const cond = try this.evaluateNode(_if.condition);

        const rval = if (cond.isTrue())
            this.evaluateNode(_if.then_block.asAst())
        else if (_if.else_block) |else_block|
            this.evaluateNode(else_block)
        else
            @as(Value, Value.None);

        return rval;
    }

    fn evaluateWhile(this: *This, _while: *AstWhile) anyerror!Value {
        var rval: Value = .None;

        while (true) {
            const cond = try this.evaluateNode(_while.condition);

            if (!cond.isTrue()) {
                break;
            }

            rval = try this.evaluateNode(_while.block.asAst());
        }

        return rval;
    }

    fn evaluateFor(this: *This, _for: *AstFor) anyerror!Value {
        const container = try this.evaluateNode(_for.container);
        const block = _for.block;

        return switch (container) {
            .Str => |value| try this.evaluateForLoopForStr(value, _for.iterator, block),
            .Range => |value| try this.evaluateForLoopForRange(value, _for.iterator, block),
            .List => |value| try this.evaluateForLoopForList(value, _for.iterator, block),
            // .Instance => |value| try this.evaluateForLoopForInstance(value, _for.iterator, block),
            else => return raise(error.RuntimeError, &this.err_msg, _for.container.token.location, "Cannot iterate over a value of this type.", .{}),
        };
    }

    fn evaluateForLoopForStr(this: *This, string: []const u8, it_id: *AstIdent, block: *AstBlock) anyerror!Value {
        var rval: Value = .None;

        var chars = (try Utf8View.init(string)).iterator();
        while (chars.nextCodepoint()) |c| {
            try this.beginScope();
            defer this.endScope();

            try this.stack.pushVariable(this.allocator, this.currentScope(), it_id.ident, Value{ .Char = c });
            rval = try this.evaluateBlock(block);
        }

        return rval;
    }

    fn evaluateForLoopForRange(this: *This, range: Range, it_id: *AstIdent, block: *AstBlock) anyerror!Value {
        var rval: Value = .None;

        var i = range.start;
        while (i < range.end) : (i += 1) {
            try this.beginScope();
            defer this.endScope();

            try this.stack.pushVariable(this.allocator, this.currentScope(), it_id.ident, Value{ .Int = i });
            rval = try this.evaluateBlock(block);
        }

        return rval;
    }

    fn evaluateForLoopForList(this: *This, list: *List, it_id: *AstIdent, block: *AstBlock) anyerror!Value {
        var rval: Value = .None;
        
        for (list.items) |value| {
            try this.beginScope();
            defer this.endScope();

            try this.stack.pushVariable(this.allocator, this.currentScope(), it_id.ident, value);
            rval = try this.evaluateBlock(block);
        }

        return rval;
    }

    // fn evaluateForLoopForInstance(this: *This, instance: *Instance, it_id: *AstIdent, block: *AstBlock) anyerror!Value {
    //     var rval: Value = .None;

    //     if (instance._struct.methods.getPtr("iter")) |iter_method_ptr| {
    //         const iter_method = iter_method_ptr.*;

    //         if (iter_method.params.len != 1) {
    //             return raise(error.RuntimeError, it_id.token.location, "To iterate over an instance, its `iter()` method must only take a `self` parameter.", &this.err_msg);
    //         }

    //         const bound_iter_method = try this.gc.allocateClosure(try iter_method.makeBound(this.allocator, Value{ .Instance = instance }, it_id.token.location, &this.err_msg));

    //         // @TODO:
    //         // figure actual scope stuff
    //         //
    //         var method_scope = Scope.init(this.allocator, this.global_scope);
    //         try this.injectClosedValuesIntoScope(bound_iter_method, &method_scope, it_id.token.location);

    //         try this.pushScope(method_scope);
    //         defer this.endScope();

    //         const iter_instance = switch (try this.callClosure(bound_iter_method)) {
    //             .Instance => |value| value,
    //             else => return raise(error.RuntimeError, it_id.token.location, "`iter()` expected to return an instance.", &this.err_msg),
    //         };

    //         rval = try this.evaluateForLoopForIteratorInstance(iter_instance, it_id, block);
    //     } else {
    //         rval = try this.evaluateForLoopForIteratorInstance(instance, it_id, block);
    //     }

    //     return rval;
    // }

    // fn evaluateForLoopForIteratorInstance(this: *This, iterator: *Instance, it_id: *AstIdent, block: *AstBlock) anyerror!Value {
    //     const next_method = (iterator._struct.methods.getPtr("iternext") orelse {
    //         return raise(error.RuntimeError, it_id.token.location, "Cannot use something that doesn't have a `iternext()` method as an iterator.", &this.err_msg);
    //     }).*;
    //     const get_method = (iterator._struct.methods.getPtr("iterget") orelse {
    //         return raise(error.RuntimeError, it_id.token.location, "Cannot use something that doesn't have a `iterget()` method as an iterator.", &this.err_msg);
    //     }).*;

    //     if (next_method.params.len != 1) {
    //         return raise(error.RuntimeError, it_id.token.location, "To iterate over an instance, its `iternext()` method must only take a `self` parameter.", &this.err_msg);
    //     }
    //     if (get_method.params.len != 1) {
    //         return raise(error.RuntimeError, it_id.token.location, "To iterate over an instance, its `iterget()` method must only take a `self` parameter.", &this.err_msg);
    //     }

    //     const bound_next_method = try this.gc.allocateClosure(try next_method.makeBound(this.allocator, Value{ .Instance = iterator }, it_id.token.location, &this.err_msg));
    //     const bound_get_method = try this.gc.allocateClosure(try get_method.makeBound(this.allocator, Value{ .Instance = iterator }, it_id.token.location, &this.err_msg));

    //     var rval: Value = .None;

    //     while (true) {
    //         {
    //             var method_scope = Scope.init(this.allocator, this.global_scope);
    //             try this.injectClosedValuesIntoScope(bound_next_method, &method_scope, it_id.token.location);

    //             try this.pushScope(method_scope);
    //             defer this.endScope();

    //             const result = try this.callClosure(bound_next_method);

    //             if (!result.isTrue()) {
    //                 break;
    //             }
    //         }

    //         // call `get()` to evaluate the next value of the iteration
    //         const iteration_value = blk: {
    //             var method_scope = Scope.init(this.allocator, this.global_scope);
    //             try this.injectClosedValuesIntoScope(bound_get_method, &method_scope, it_id.token.location);

    //             try this.pushScope(method_scope);
    //             defer this.endScope();

    //             const result = try this.callClosure(bound_get_method);
    //             break :blk result;
    //         };

    //         // evaluate block with current iteration value
    //         {
    //             try this.beginScope();
    //             defer this.endScope();

    //             _ = try this.currentScope().addVariable(it_id.ident, iteration_value, it_id.token.location, &this.err_msg);
    //             rval = try this.evaluateBlock(block);
    //         }
    //     }

    //     return rval;
    // }

    // fn evaluateDef(this: *This, def: *AstDef) anyerror!void {
    //     const closure = try this.createDefClosure(def);
    //     const closure_value = Value{ .Closure = closure };
    //     try this.stack.pushVariable(this.allocator, this.currentScope(), def.ident.ident, closure_value);
    // }

    fn evaluateCase(this: *This, case: *AstCase) anyerror!Value {
        var rval: Value = .None;

        const cond_value = try this.evaluateNode(case.condition);

        for (case.branches) |branch| {
            const gate_value = try this.evaluateNode(branch.lhs);
            if (gate_value.eql(cond_value)) {
                rval = try this.evaluateNode(branch.rhs);
                break;
            }
        } else {
            if (case.default) |default| {
                rval = try this.evaluateNode(default);
            }
        }

        return rval;
    }

    fn evaluateInstantiateLambda(this: *This, lambda: *AstInstantiateLambda) anyerror!void {
        const def = this.interp.lambdas.items[lambda.lambda_index];

        const closure = try this.instantiateLambda(def);
        const closure_value = Value{ .Closure = closure };
        try this.stack.pushVariable(this.allocator, this.currentScope(), def.name, closure_value);
    }

    fn instantiateLambda(this: *This, lambda: LambdaDefinition) anyerror!*Closure {
        var closed_values = ArrayListUnmanaged(Value){};
        errdefer closed_values.deinit(this.allocator);

        for (lambda.closed_values) |cv| {
            var index = cv.index;
            if (!cv.global) {
                const frame = this.currentFrame() orelse return raise(error.InternalError, &this.err_msg, null, "Attempt to retrieve non-global closed value ({}) without a call frame", .{index});
                index += frame.start;
            }

            const value = this.stack.values.items[index];
            try closed_values.append(this.allocator, value);
        }

        const closure = Closure.init(lambda.name, lambda.type_def, lambda.code, closed_values.items, lambda.closed_value_names);
        return try this.gc.copyClosure(closure);
    }

    // fn createDefClosure(this: *This, def: *AstDef) anyerror!*Closure {
    //     // @TODO:
    //     // Find closure values and close them.
    //     //

    //     // var params = try ArrayListUnmanaged(Closure.Parameter).initCapacity(this.allocator, def.params.len);
    //     // for (def.params) |param_node| {
    //     //     const ident = param_node.ident;
    //     //     const param = Closure.Parameter{ .name = ident.ident };
    //     //     try params.append(this.allocator, param);
    //     // }

    //     const closed_values = StringArrayHashMapUnmanaged(Value){};

    //     const type_def = def.type_def orelse return raise(error.InternalError, &this.err_msg, def.token.location, "AstDef reached evaluation without a LambdaTypeDefinition.", .{});

    //     return try this.gc.copyClosure(Closure.init(def.ident.ident, type_def, def.body, closed_values));
    // }

    fn evaluateVar(this: *This, _var: *AstVar) anyerror!void {
        const var_ident = _var.ident.ident;
        const initial = try this.evaluateNode(_var.initializer);

        try this.stack.pushVariable(this.allocator, this.currentScope(), var_ident, initial);
    }

    // fn evaluateExtend(this: *This, extend: *AstExtend) anyerror!void {
    //     const struct_value = try this.evaluateNode(extend._struct);

    //     const _struct = switch (struct_value) {
    //         .Struct => |rc| rc,
    //         else => return raise(error.RuntimeError, extend._struct.token.location, "Can only extend structs.", &this.err_msg),
    //     };

    //     for (extend.body.nodes) |method_node| {
    //         switch (method_node.kind) {
    //             .Def => {
    //                 const def = method_node.downcast(AstDef);
    //                 const method = try this.createDefClosure(def);
    //                 try _struct.methods.put(this.allocator, def.name, method);
    //             },
    //             else => return raise(error.RuntimeError, method_node.token.location, "Can only extend structs with methods.", &this.err_msg),
    //         }
    //     }
    // }
};

fn arrayBoundsCheck(
    comptime T: type,
    array: []T,
    index: usize,
    index_location: CodeLocation,
    out_err_msg: *ErrMsg,
) !void {
    if (index < 0 or index >= array.len) {
        return raise(error.RuntimeError, out_err_msg, index_location, "Array bounds check failure! List has length {} but index was {}.", .{ array.len, index });
    }
}
