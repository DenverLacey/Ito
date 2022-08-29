const std = @import("std");
const Token = @import("parser.zig").Token;
const Char = @import("values.zig").Char;
const Type = @import("values.zig").Type;

pub const Ast = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,

    const This = @This();

    fn init(kind: AstKind, token: Token) This {
        return This{ .kind = kind, .token = token, .typ = null };
    }

    pub fn downcast(this: *This, comptime T: type) *T {
        return @ptrCast(*T, this);
    }

    pub fn downcastConst(this: *const This, comptime T: type) *const T {
        return @ptrCast(*const T, this);
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (this.kind) {
            // Literals
            .None, .Bool, .Char, .Int, .Num, .Str => try writer.print("{}", .{this.downcastConst(AstLiteral)}),
            .Ident => try writer.print("{}", .{this.downcastConst(AstIdent)}),

            // Unary
            .Negate,
            .Not,
            => try writer.print("{}", .{this.downcastConst(AstUnary)}),

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
            .Dot,
            .ExclusiveRange,
            => try writer.print("{}", .{this.downcastConst(AstBinary)}),

            // Blocks
            .Block, .Comma, .List => try writer.print("{}", .{this.downcastConst(AstBlock)}),

            .If => try writer.print("{}", .{this.downcastConst(AstIf)}),
            .While => try writer.print("{}", .{this.downcastConst(AstWhile)}),
            .For => try writer.print("{}", .{this.downcastConst(AstFor)}),
            .Def => try writer.print("{}", .{this.downcastConst(AstDef)}),
            .Param => try writer.print("{}", .{this.downcastConst(AstParam)}),
            .Var => try writer.print("{}", .{this.downcastConst(AstVar)}),
            .VarBlock => try writer.print("{}", .{this.downcastConst(AstVarBlock)}),
            .Type => try writer.print("{}", .{this.downcastConst(AstType)}),
            .TypeBlock => try writer.print("{}", .{this.downcastConst(AstTypeBlock)}),
            .TypeSignature => try writer.print("{}", .{this.downcastConst(AstTypeSignature)}),

            // Typechecked specific
            .GetGlobal => try writer.print("{}", .{this.downcastConst(AstGetVar)}),
            .GetVar => try writer.print("{}", .{this.downcastConst(AstGetVar)}),
            .GetType => try writer.print("{}", .{this.downcastConst(AstGetType)}),
            .GetTag => try writer.print("{}", .{this.downcastConst(AstGetTag)}),
        }
    }
};

pub const AstKind = enum {
    // Literals
    None,
    Bool,
    Char,
    Int,
    Num,
    Str,
    Ident,

    // Unary
    Negate,
    Not,

    // Binary
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Or,
    And,
    Index,
    Call,
    Dot,
    ExclusiveRange,

    // Blocks
    Block,
    Comma,
    List,

    If,
    While,
    For,
    Def,
    Param,
    Var,
    VarBlock,
    Type,
    TypeBlock,
    TypeSignature,

    // Typechecked specific
    GetGlobal,
    GetVar,
    GetType,
    GetTag,
};

pub const AstLiteral = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    literal: Literal,

    const This = @This();

    pub const Literal = union(enum) {
        None,
        Bool: bool,
        Char: Char,
        Int: i64,
        Num: f64,
        Str: []const u8,
    };

    pub fn init(kind: AstKind, token: Token, literal: Literal) This {
        const typ: Type = switch (literal) {
            .None => .None,
            .Bool => .Bool,
            .Char => .Char,
            .Int => .Int,
            .Num => .Num,
            .Str => .Str,
        };
        return This{ .kind = kind, .token = token, .typ = typ, .literal = literal };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstIdent = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    ident: []const u8,
    big: bool,

    const This = @This();

    pub fn init(token: Token, ident: []const u8, big: bool) This {
        return This{ .kind = .Ident, .token = token, .typ = null, .ident = ident, .big = big };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstGetVar = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    index: usize,

    const This = @This();

    pub fn init(token: Token, typ: Type, index: usize, global: bool) This {
        return This{ 
            .kind = if (global) AstKind.GetGlobal else AstKind.GetVar,
            .token = token,
            .typ = typ,
            .index = index,
        };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstGetType = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    typ_of: Type,

    const This = @This();

    pub fn init(token: Token, typ: Type) This {
        return This{ .kind = .GetType, .token = token, .typ = .Type, .typ_of = typ };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstGetTag = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    tag_name: []const u8,

    const This = @This();

    pub fn init(token: Token, typ: Type, tag_name: []const u8) This {
        return This{ .kind = .GetTag, .token = token, .typ = typ, .tag_name = tag_name };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstUnary = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    sub: *Ast,

    const This = @This();

    pub fn init(kind: AstKind, token: Token, sub: *Ast) This {
        return This{ .kind = kind, .token = token, .typ = null, .sub = sub };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstBinary = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    lhs: *Ast,
    rhs: *Ast,

    const This = @This();

    pub fn init(kind: AstKind, token: Token, lhs: *Ast, rhs: *Ast) This {
        return This{ .kind = kind, .token = token, .typ = null, .lhs = lhs, .rhs = rhs };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstBlock = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    nodes: []*Ast,

    const This = @This();

    pub fn init(kind: AstKind, token: Token, nodes: []*Ast) This {
        return This{ .kind = kind, .token = token, .typ = null, .nodes = nodes };
    }

    pub fn deinit(this: *This, allocator: std.mem.Allocator) void {
        allocator.free(this.nodes);
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstIf = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    condition: *Ast,
    then_block: *AstBlock,
    else_block: ?*Ast,

    const This = @This();

    pub fn init(token: Token, condition: *Ast, then_block: *AstBlock, else_block: ?*Ast) This {
        return This{ 
            .kind = .If,
            .token = token,
            .typ = null,
            .condition = condition,
            .then_block = then_block,
            .else_block = else_block
        };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstWhile = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    condition: *Ast,
    block: *AstBlock,

    const This = @This();

    pub fn init(token: Token, condition: *Ast, block: *AstBlock) This {
        return This{ .kind = .While, .token = token, .typ = null, .condition = condition, .block = block };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstFor = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    iterator: *AstIdent,
    container: *Ast,
    block: *AstBlock,

    const This = @This();

    pub fn init(token: Token, iterator: *AstIdent, container: *Ast, block: *AstBlock) This {
        return This{ .kind = .For,
            .token = token,
            .typ = null,
            .iterator = iterator,
            .container = container,
            .block = block
        };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstDef = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    name: []const u8,
    params: []*AstParam,
    ret_type: ?*AstTypeSignature,
    body: *AstBlock,

    const This = @This();

    pub fn init(token: Token, name: []const u8, params: []*AstParam, ret_type: ?*AstTypeSignature, body: *AstBlock) This {
        return This{ 
            .kind = .Def, 
            .token = token, 
            .typ = null, 
            .name = name, 
            .params = params, 
            .ret_type = ret_type, 
            .body = body
        };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstParam = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    ident: *AstIdent,
    sig: ?*AstTypeSignature,

    const This = @This();

    pub fn init(token: Token, ident: *AstIdent, sig: ?*AstTypeSignature) This {
        return This{ .kind = .Param, .token = token, .typ = null, .ident = ident, .sig = sig };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstVarBlock = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    declarations: []*AstVar,

    const This = @This();

    pub fn init(token: Token, declarations: []*AstVar) This {
        return This{ .kind = .VarBlock, .token = token, .typ = null, .declarations = declarations };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstVar = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    ident: *AstIdent,
    initializer: *Ast,

    const This = @This();

    pub fn init(token: Token, ident: *AstIdent, initializer: *Ast) This {
        return This{ .kind = .Var, .token = token, .typ = null, .ident = ident, .initializer = initializer };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstTypeBlock = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    declarations: []*AstType,

    const This = @This();

    pub fn init(token: Token, declarations: []*AstType) This {
        return This{ .kind = .TypeBlock, .token = token, .typ = null, .declarations = declarations };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstType = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    ident: *AstIdent,
    signature: *AstTypeSignature,

    const This = @This();

    pub fn init(token: Token, ident: *AstIdent, signature: *AstTypeSignature) This {
        return This{ .kind = .Type, .token = token, .typ = null, .ident = ident, .signature = signature };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }
};

pub const AstTypeSignature = struct {
    kind: AstKind,
    token: Token,
    typ: ?Type,
    data: Data,

    const This = @This();

    pub fn init(token: Token, data: Data) This {
        return This{ .kind = .TypeSignature, .token = token, .typ = null, .data = data };
    }

    pub fn asAst(this: *This) *Ast {
        return @ptrCast(*Ast, this);
    }

    pub const Data = union(enum) {
        Name: []const u8,
        Tuple: TupleData,
        Tag: TagData,
        Union: UnionData,
    };

    pub const TupleData = struct {
        field_names: []*AstIdent,
        field_types: []?*AstTypeSignature,
    };

    pub const TagData = struct {
        tag_names: []*AstIdent,
    };

    pub const UnionData = struct {
        variants: []*AstTypeSignature,
    };
};

// pub const AstExtend = struct {
//     kind: AstKind,
//     token: Token,
//     _struct: *Ast,
//     body: *AstBlock,

//     const This = @This();

//     pub fn init(token: Token, _struct: *Ast, body: *AstBlock) This {
//         return This{ .kind = .Extend, .token = token, ._struct = _struct, .body = body };
//     }

//     pub fn asAst(this: *This) *Ast {
//         return @ptrCast(*Ast, this);
//     }
// };
