const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Utf8View = std.unicode.Utf8View;
const Utf8Iterator = std.unicode.Utf8Iterator;

const errors = @import("errors.zig");
const ErrMsg = errors.ErrMsg;
const raise = errors.raise;
const todo = errors.todo;

const Char = @import("values.zig").Char;

const ast = @import("ast.zig");
const Ast = ast.Ast;
const AstKind = ast.AstKind;
const AstLiteral = ast.AstLiteral;
const AstIdent = ast.AstIdent;
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

const zg = @import("ziglyph");

pub const CodeLocation = struct {
    line: usize,
    col: usize,
    file: []const u8,

    const This = @This();

    pub fn init(line: usize, col: usize, file: []const u8) This {
        return This{ .line = line, .col = col, .file = file };
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s}:{}:{}", .{ this.file, this.line, this.col });
    }
};

pub const Token = struct {
    indentation: usize,
    data: TokenData,
    location: CodeLocation,

    const This = @This();

    pub fn init(indentation: usize, data: TokenData, location: CodeLocation) This {
        return This{ 
            .indentation = indentation,
            .data = data,
            .location = location
        };
    }

    fn precedence(this: *const Token) TokenPrecedence {
        return switch (this.data) {
            // Literals
            .None => .None,
            .Bool => .None,
            .Char => .None,
            .Int => .None,
            .Num => .None,
            .Str => .None,
            .Ident => .None,
            .BigIdent => .None,

            // Delimeters
            .Newline => .None,
            .Comma => .None,
            .Semicolon => .None,
            .Colon => .None,
            .LeftParen => .Call,
            .RightParen => .None,
            .LeftCurly => .None,
            .RightCurly => .None,
            .LeftSquare => .Call,
            .RightSquare => .None,
            .Pipe => .BitOr,

            // Operators
            .Bang => .Unary,
            .BangEqual => .Equality,
            .Or => .Or,
            .And => .And,
            .Plus => .Term,
            .Dash => .Term,
            .Star => .Factor,
            .Slash => .Factor,
            .Equal => .Assignment,
            .DoubleEqual => .Equality,
            .LeftAngle => .Comparison,
            .RightAngle => .Comparison,
            .Dot => .Call,
            .DoubleDot => .Range,
            .Space => .Call,
            .QuestionMark => .Unary,
            .DoubleQuestionMark => .NoneOr,
            .Arrow => .None, // @TODO: When we do annonymous lambdas maybe we want a particular precedence.

            // Keywords
            .Do => .None,
            .Pass => .None,
            .If => .None,
            .Else => .None,
            .Elif => .None,
            .While => .None,
            .For => .None,
            .In => .None,
            .Def => .None,
            .Var => .None,
            .Type => .None,
            .Module => .None,
        };
    }

    pub fn format(this: *const This, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (std.mem.eql(u8, fmt, "!")) {
            switch (this.data) {
                // Literals
                .None => _ = try writer.write("None"),
                .Bool => |value| {
                    const value_as_string: []const u8 = if (value) "True" else "False";
                    try writer.print("{s}", .{value_as_string});
                },
                .Char => |value| try writer.print("{u}", .{value}),
                .Int => |value| try writer.print("{}", .{value}),
                .Num => |value| try writer.print("{d}", .{value}),
                .Str => |value| try writer.print("{s}", .{value}),
                .Ident, .BigIdent => |ident| try writer.print("{s}", .{ident}),

                // Delimeters
                .Newline => try writer.print("newline", .{}),
                .Comma => try writer.print(",", .{}),
                .Semicolon => try writer.print(";", .{}),
                .Colon => try writer.print(":", .{}),
                .LeftParen => try writer.print("(", .{}),
                .RightParen => try writer.print(")", .{}),
                .LeftCurly => try writer.print("{{", .{}),
                .RightCurly => try writer.print("}}", .{}),
                .LeftSquare => try writer.print("[", .{}),
                .RightSquare => try writer.print("]", .{}),
                .Pipe => try writer.print("|", .{}),

                // Operators
                .Bang => try writer.print("!", .{}),
                .BangEqual => try writer.print("!=", .{}),
                .Or => try writer.print("or", .{}),
                .And => try writer.print("and", .{}),
                .Plus => try writer.print("+", .{}),
                .Dash => try writer.print("-", .{}),
                .Star => try writer.print("*", .{}),
                .Slash => try writer.print("/", .{}),
                .Equal => try writer.print("=", .{}),
                .DoubleEqual => try writer.print("==", .{}),
                .LeftAngle => try writer.print("<", .{}),
                .RightAngle => try writer.print(">", .{}),
                .Dot => try writer.print(".", .{}),
                .DoubleDot => try writer.print("..", .{}),
                .Space => try writer.print(" ", .{}),
                .QuestionMark => try writer.print("?", .{}),
                .DoubleQuestionMark => try writer.print("??", .{}),
                .Arrow => try writer.print("->", .{}),

                // Keywords
                .Do => try writer.print("do", .{}),
                .Pass => try writer.print("pass", .{}),
                .If => try writer.print("if", .{}),
                .Else => try writer.print("else", .{}),
                .Elif => try writer.print("elif", .{}),
                .While => try writer.print("while", .{}),
                .For => try writer.print("for", .{}),
                .In => try writer.print("in", .{}),
                .Def => try writer.print("def", .{}),
                .Var => try writer.print("var", .{}),
                .Type => try writer.print("type", .{}),
                .Module => try writer.print("module", .{}),
            }
        } else {
            try writer.print("Token {{ .indentation: {}, .data: {}, .location: {} }}", .{ this.indentation, this.data, this.location });
        }
    }
};

pub const TokenKind = enum {
    // Literals
    None,
    Bool,
    Char,
    Int,
    Num,
    Str,
    Ident,
    BigIdent,

    // Delimeters
    Newline,
    Comma,
    Semicolon,
    Colon,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,
    Pipe,

    // Operators
    Bang,
    BangEqual,
    Or,
    And,
    Plus,
    Dash,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    LeftAngle,
    RightAngle,
    Dot,
    DoubleDot,
    Space,
    QuestionMark,
    DoubleQuestionMark,
    Arrow,

    // Keywords
    Do,
    Pass,
    If,
    Else,
    Elif,
    While,
    For,
    In,
    Def,
    Var,
    Type,
    Module,
};

pub const TokenData = union(TokenKind) {
    // Literals
    None,
    Bool: bool,
    Char: Char,
    Int: i64,
    Num: f64,
    Str: []const u8,
    Ident: []const u8,
    BigIdent: []const u8,

    // Delimeters
    Newline,
    Comma,
    Semicolon,
    Colon,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    LeftSquare,
    RightSquare,
    Pipe,

    // Operators
    Bang,
    BangEqual,
    Or,
    And,
    Plus,
    Dash,
    Star,
    Slash,
    Equal,
    DoubleEqual,
    LeftAngle,
    RightAngle,
    Dot,
    DoubleDot,
    Space,
    QuestionMark,
    DoubleQuestionMark,
    Arrow,

    // Keywords
    Do,
    Pass,
    If,
    Else,
    Elif,
    While,
    For,
    In,
    Def,
    Var,
    Type,
    Module,

    const This = @This();

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("{s} {{ ", .{@typeName(This)});
        switch (this.*) {
            // Literals
            .None => _ = try writer.write(".None"),
            .Bool => |value| try writer.print(".Bool = {}", .{value}),
            .Char => |value| try writer.print(".Char = {}", .{value}),
            .Int => |value| try writer.print(".Int = {}", .{value}),
            .Num => |value| try writer.print(".Num = {}", .{value}),
            .Str => |str| try writer.print(".Str = {s}", .{str}),
            .Ident => |id| try writer.print(".Ident = {s}", .{id}),
            .BigIdent => |id| try writer.print(".BigIdent = {s}", .{id}),

            // Delimeters
            .Newline => _ = try writer.write(".Newline"),
            .Comma => _ = try writer.write(".Comma"),
            .Semicolon => _ = try writer.write(".Semicolon"),
            .Colon => _ = try writer.write(".Colon"),
            .LeftParen => _ = try writer.write(".LeftParen"),
            .RightParen => _ = try writer.write(".RightParen"),
            .LeftCurly => _ = try writer.write(".LeftCurly"),
            .RightCurly => _ = try writer.write(".RightCurly"),
            .LeftSquare => _ = try writer.write(".LeftSqaure"),
            .RightSquare => _ = try writer.write(".RightSquare"),
            .Pipe => _ = try writer.write(".Pipe"),

            // Operators
            .Bang => _ = try writer.write(".Bang"),
            .BangEqual => _ = try writer.write(".BangEqual"),
            .Or => _ = try writer.write(".Or"),
            .And => _ = try writer.write(".And"),
            .Plus => _ = try writer.write(".Plus"),
            .Dash => _ = try writer.write(".Dash"),
            .Star => _ = try writer.write(".Star"),
            .Slash => _ = try writer.write(".Slash"),
            .Equal => _ = try writer.write(".Equal"),
            .DoubleEqual => _ = try writer.write(".DoubleEqual"),
            .LeftAngle => _ = try writer.write(".LeftAngle"),
            .RightAngle => _ = try writer.write(".RightAngle"),
            .Dot => _ = try writer.write(".Dot"),
            .DoubleDot => _ = try writer.write(".DoubleDot"),
            .Space => _ = try writer.write(".Space"),
            .QuestionMark => _ = try writer.write(".QuestionMark"),
            .DoubleQuestionMark => _ = try writer.write(".DoubleQuestionMark"),
            .Arrow => _ = try writer.write(".Arrow"),

            // Keywords
            .Do => _ = try writer.write(".Do"),
            .Pass => _ = try writer.write(".Pass"),
            .If => _ = try writer.write(".If"),
            .Else => _ = try writer.write(".Else"),
            .Elif => _ = try writer.write(".Elif"),
            .While => _ = try writer.write(".While"),
            .For => _ = try writer.write(".For"),
            .In => _ = try writer.write(".In"),
            .Def => _ = try writer.write(".Def"),
            .Var => _ = try writer.write(".Var"),
            .Type => _ = try writer.write(".Type"),
            .Module => _ = try writer.write(".Module"),
        }
        _ = try writer.write(" }");
    }
};

pub const TokenPrecedence = enum {
    None,
    Assignment, // = += -= *= /= &= etc.
    Colon, // :
    NoneOr, // ??
    Cast, // as
    Range, // .. ...
    Or, // ||
    And, // &&
    BitOr, // |
    Xor, // ^
    BitAnd, // &
    Equality, // == !=
    Comparison, // < > <= >=
    Shift, // << >>
    Term, // + -
    Factor, // * / %
    Unary, // ! ~
    Call, // . () []
    Primary,

    fn next(this: TokenPrecedence) TokenPrecedence {
        const primary: u8 = @enumToInt(TokenPrecedence.Primary);
        const p = @enumToInt(this);
        return @intToEnum(TokenPrecedence, if (p + 1 > primary) primary else p + 1);
    }
};

pub const Tokenizer = struct {
    allocator: Allocator,
    source: Utf8Iterator,
    filename: []const u8,
    peeked_tokens: ArrayListUnmanaged(Token),
    previous_was_newline: bool,
    previous_token_kind: TokenKind,
    indentation: usize,

    line: usize,
    column: usize,

    err_msg: *ErrMsg,

    const This = @This();

    pub fn init(allocator: Allocator, err_msg: *ErrMsg, filename: []const u8, source: []const u8) !This {
        return This{
            .allocator = allocator,
            .source = (try Utf8View.init(source)).iterator(),
            .filename = filename,
            .peeked_tokens = ArrayListUnmanaged(Token){},
            .previous_was_newline = false,
            .previous_token_kind = .Newline,
            .indentation = 0,
            .line = 1,
            .column = 1,
            .err_msg = err_msg,
        };
    }

    fn bytesRemaining(this: *This) usize {
        return this.source.bytes.len - this.source.i;
    }

    fn currentLocation(this: *This) CodeLocation {
        return CodeLocation.init(this.line, this.column, this.filename);
    }

    fn isIdentBegin(c: Char) bool {
        return zg.isAlphabetic(c) or c == '_';
    }

    fn isIdentChar(c: Char) bool {
        return isIdentBegin(c) or zg.isAsciiDigit(c);
    }

    fn isStrBegin(c: Char) bool {
        return c == '"';
    }

    fn isCharBegin(c: Char) bool {
        return c == '\'';
    }

    fn isNumBegin(this: *This) bool {
        const reset_point = this.source.i;
        defer this.source.i = reset_point;

        const c1 = this.peekChar() orelse return false;
        if (zg.isAsciiDigit(c1))
            return true;

        if (!this.nextIfEq('-'))
            return false;
        
        const c2 = this.peekChar() orelse return false;
        return zg.isAsciiDigit(c2);
    }

    fn callIsBlocked(this: *This) bool {
        return this.previousIsCallBlocking() or this.nextIsCallBlocking();
    }

    fn nextIsCallBlocking(this: *This) bool {
        const reset_point = this.source.i;
        defer this.source.i = reset_point;

        const num_peeked = this.peeked_tokens.items.len;
        defer this.peeked_tokens.shrinkRetainingCapacity(num_peeked);

        const token = (this.peek(0) catch return false) orelse return false;
        switch (token.data) {
            // Delimeters
            .Newline,
            .Comma,
            .Semicolon,
            .Colon,
            .RightParen,
            .RightCurly,
            .RightSquare,
            .Pipe,

            // Operators
            .Bang,
            .BangEqual,
            .Or,
            .And,
            .Plus,
            .Dash,
            .Star,
            .Slash,
            .Equal,
            .DoubleEqual,
            .RightAngle,
            .Dot,
            .DoubleDot,
            .Space,
            .QuestionMark,
            .DoubleQuestionMark,
            .Arrow,

            // Keywords
            .In => return true,

            else => return false,
        }

        // const c = this.peekChar() orelse return false;
        // switch (c) {
        //     ',' => return true,
        //     ';' => return true,
        //     ':' => return true,
        //     ')' => return true,
        //     '}' => return true,
        //     ']' => return true,
        //     '!' => return true,
        //     '+' => return true,
        //     '-' => return true,
        //     '*' => return true,
        //     '/' => return true,
        //     '=' => return true,
        //     '<' => return true,
        //     '>' => return true,
        //     '.' => return true,
        //     '?' => return true,
        //     else => return isIdentBegin(c) and this.tokenizeIdentOrKeyword().data != .Ident,
        // }
    }

    fn previousIsCallBlocking(this: *This) bool {
        var previous = this.previous_token_kind;
        if (this.peeked_tokens.items.len != 0) {
            previous = this.peeked_tokens.items[this.peeked_tokens.items.len - 1].data;
        }

        return switch (previous) {
            // Literals
            .None,
            .Bool,
            .Char,
            .Int,
            .Num,
            .Str => true,

            // Delimeters
            .Newline,
            .Comma,
            .Semicolon,
            .Colon,
            .LeftParen,
            .LeftCurly,
            .LeftSquare,
            .Arrow => true,

            // Operators
            .Bang,
            .BangEqual,
            .Or,
            .And,
            .Plus,
            .Dash,
            .Star,
            .Slash,
            .Equal,
            .DoubleEqual,
            .LeftAngle,
            .RightAngle,
            .Dot,
            .DoubleDot,
            .Space,
            .DoubleQuestionMark => true,

            // Keywords
            .Do,
            .Pass,
            .If,
            .Else,
            .Elif,
            .While,
            .For,
            .In,
            .Def,
            .Var,
            .Type,
            .Module => true,

            else => false,
        };
    }

    fn peekChar(this: *This) ?Char {
        if (this.bytesRemaining() == 0) {
            return null;
        }

        const peeked = this.source.peek(1);
        return std.unicode.utf8Decode(peeked) catch |e| blk: {
            switch (e) {
                error.Utf8CodepointTooLarge => std.log.err("{}", .{e}),
                error.Utf8EncodesSurrogateHalf => std.log.err("{}", .{e}),
                error.Utf8ExpectedContinuation => std.log.err("{}", .{e}),
                error.Utf8OverlongEncoding => std.log.err("{}", .{e}),
            }
            break :blk null;
        };
    }

    fn nextChar(this: *This) ?Char {
        const maybe_next_char = this.source.nextCodepoint();
        if (maybe_next_char) |c| {
            if (c == '\n') {
                this.previous_was_newline = true;
                this.line += 1;
                this.column = 1;
            } else {
                this.previous_was_newline = false;
                this.column += 1;
            }
        }

        return maybe_next_char;
    }

    fn nextIf(this: *This, f: fn(Char) bool) ?Char {
        const c = this.peekChar() orelse return null;
        if (!f(c)) return null;
        return this.nextChar();
    }

    fn nextIfEq(this: *This, c: Char) bool {
        const pc = this.peekChar() orelse return false;
        if (c != pc) return false;
        return this.nextChar() != null;
    }

    fn peek(this: *This, n: usize) !?Token {
        while (this.peeked_tokens.items.len <= n) {
            if (try this.nextNoPeeking()) |t| {
                try this.peeked_tokens.append(this.allocator, t);
            } else {
                return null;
            }
        }
        return this.peeked_tokens.items[n];
    }

    fn next(this: *This) !?Token {
        if (this.peeked_tokens.items.len != 0) {
            const t = this.peeked_tokens.orderedRemove(0);
            std.debug.print(">>> {}\n", .{t});
            return t;
        }

        const t = try this.nextNoPeeking();
        std.debug.print(">>> {}\n", .{t});
        return t;
    }

    fn nextNoPeeking(this: *This) !?Token {
        try this.skipToBeginningOfNextToken();

        var token: ?Token = undefined;
        if (this.peekChar()) |c| {
            if (c == 0) {
                token = null;
            } else if (c == '\n') {
                token = Token.init(this.indentation, .Newline, this.currentLocation());
                _ = this.nextChar();
            } else if (this.isNumBegin()) {
                token = this.tokenizeNumber();
            } else if (isCharBegin(c)) {
                token = try this.tokenizeTextLiteral(true);
            } else if (isStrBegin(c)) {
                token = try this.tokenizeTextLiteral(false);
            } else if (isIdentBegin(c)) {
                token = this.tokenizeIdentOrKeyword();
            } else {
                token = try this.tokenizePunctuation();
            }

            this.previous_token_kind = token.?.data;
        } else {
            token = null;
        }

        return token;
    }

    fn skipToBeginningOfNextToken(this: *This) !void {
        const track_indentation = this.previous_was_newline;

        if (track_indentation) {
            this.indentation = 0;
        }

        while (this.bytesRemaining() != 0) {
            if (this.peekChar()) |c| {
                switch (c) {
                    '#' => while (true) {
                        if (this.nextChar()) |nc| {
                            if (nc == '\n') {
                                break;
                            }
                        } else {
                            break;
                        }
                    },
                    '\n' => {
                        if (!this.previous_was_newline) {
                            break;
                        }
                        _ = this.nextChar();
                    },
                    else => {
                        if (c != ' ') {
                            if (zg.isWhiteSpace(c)) {
                                return raise(error.ParseError, this.err_msg, this.currentLocation(), "Can only use spaces as a whitespace character.", .{});
                            } else {
                                break;
                            }
                        }

                        const reset_point = this.source.i;
                        _ = this.nextChar();
                        if (track_indentation) {
                            this.indentation += 1;
                        } else if (this.peekChar() != null) {
                            if (!this.callIsBlocked()) {
                                this.source.i = reset_point;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    fn tokenizeNumber(this: *This) Token {
        const location = this.currentLocation();

        const start_index = this.source.i;
        var end_index = start_index;

        if (this.nextIfEq('-'))
            end_index = this.source.i;

        while (this.nextIf(zg.isAsciiDigit) != null) {
            end_index = this.source.i;
        }

        const reset_index = this.source.i;
        var has_fractional_component = false;
        if (this.nextIfEq('.')) {
            if (this.peekChar()) |c| {
                if (!zg.isAsciiDigit(c)) {
                    this.source.i = reset_index;
                } else {
                    has_fractional_component = true;
                    while (this.nextIf(zg.isAsciiDigit) != null) {
                        end_index = this.source.i;
                    }
                }
            }
        }

        const word = this.source.bytes[start_index..end_index];

        if (has_fractional_component) {
            return Token.init(this.indentation, TokenData{ .Num = std.fmt.parseFloat(f64, word) catch unreachable }, location);
        } else {
            return Token.init(this.indentation, TokenData{ .Int = std.fmt.parseInt(i64, word, 10) catch unreachable }, location);
        }
    }

    fn tokenizeTextLiteral(this: *This, comptime char: bool) !Token {
        const location = this.currentLocation();
        const terminator = this.nextChar().?;

        const start_index = this.source.i;
        var end_index = start_index;

        while (true) {
            if (this.nextChar()) |c| {
                if (c == terminator) {
                    break;
                }
                end_index = this.source.i;
            } else {
                return raise(error.ParseError, this.err_msg, location, if (char) "Unended Char literal." else "Unended Str literal.", .{});
            }
        }

        const word = this.source.bytes[start_index..end_index];

        if (char) {
            if (word.len != 1) {
                return raise(error.ParseError, this.err_msg, location, "Char literals can only contain 1 character.", .{});
            } else {
                return Token.init(this.indentation, TokenData{ .Char = word[0] }, location);
            }
        } else {
            return Token.init(this.indentation, TokenData{ .Str = word }, location);
        }
    }

    fn tokenizeIdentOrKeyword(this: *This) Token {
        const location = this.currentLocation();

        const start_index = this.source.i;
        var end_index = start_index;
        while (this.nextIf(isIdentChar) != null) {
            end_index = this.source.i;
        }

        const word = this.source.bytes[start_index..end_index];

        const seq_length = std.unicode.utf8ByteSequenceLength(word[0]) catch unreachable;
        const first_char = std.unicode.utf8Decode(word[0..seq_length]) catch unreachable;
        const is_big = zg.isUpper(first_char);

        return if (std.mem.eql(u8, word, "None"))
            Token.init(this.indentation, .None, location)
        else if (std.mem.eql(u8, word, "True"))
            Token.init(this.indentation, TokenData{ .Bool = true }, location)
        else if (std.mem.eql(u8, word, "False"))
            Token.init(this.indentation, TokenData{ .Bool = false }, location)
        else if (std.mem.eql(u8, word, "or"))
            Token.init(this.indentation, .Or, location)
        else if (std.mem.eql(u8, word, "and"))
            Token.init(this.indentation, .And, location)
        else if (std.mem.eql(u8, word, "do"))
            Token.init(this.indentation, .Do, location)
        else if (std.mem.eql(u8, word, "pass"))
            Token.init(this.indentation, .Pass, location)
        else if (std.mem.eql(u8, word, "if"))
            Token.init(this.indentation, .If, location)
        else if (std.mem.eql(u8, word, "else"))
            Token.init(this.indentation, .Else, location)
        else if (std.mem.eql(u8, word, "elif"))
            Token.init(this.indentation, .Elif, location)
        else if (std.mem.eql(u8, word, "while"))
            Token.init(this.indentation, .While, location)
        else if (std.mem.eql(u8, word, "for"))
            Token.init(this.indentation, .For, location)
        else if (std.mem.eql(u8, word, "in"))
            Token.init(this.indentation, .In, location)
        else if (std.mem.eql(u8, word, "def"))
            Token.init(this.indentation, .Def, location)
        else if (std.mem.eql(u8, word, "var"))
            Token.init(this.indentation, .Var, location)
        else if (std.mem.eql(u8, word, "type"))
            Token.init(this.indentation, .Type, location)
        else if (is_big)
            Token.init(this.indentation, TokenData{ .BigIdent = word }, location)
        else
            Token.init(this.indentation, TokenData{ .Ident = word }, location);
    }

    fn tokenizePunctuation(this: *This) !Token {
        const location = this.currentLocation();
        return switch (this.nextChar().?) {
            ',' => Token.init(this.indentation, .Comma, location), 
            ';' => Token.init(this.indentation, .Semicolon, location),
            ':' => Token.init(this.indentation, .Colon, location),
            '(' => Token.init(this.indentation, .LeftParen, location),
            ')' => Token.init(this.indentation, .RightParen, location),
            '{' => Token.init(this.indentation, .LeftCurly, location),
            '}' => Token.init(this.indentation, .RightCurly, location),
            '[' => Token.init(this.indentation, .LeftSquare, location),
            ']' => Token.init(this.indentation, .RightSquare, location),
            '|' => Token.init(this.indentation, .Pipe, location),
            '!' => if (this.nextIfEq('='))
                Token.init(this.indentation, .BangEqual, location)
            else
                Token.init(this.indentation, .Bang, location),
            '+' => Token.init(this.indentation, .Plus, location),
            '-' => if (this.nextIfEq('>'))
                Token.init(this.indentation, .Arrow, location)
            else
                Token.init(this.indentation, .Dash, location),
            '*' => Token.init(this.indentation, .Star, location),
            '/' => Token.init(this.indentation, .Slash, location),
            '=' => if (this.nextIfEq('='))
                Token.init(this.indentation, .DoubleEqual, location)
            else
                Token.init(this.indentation, .Equal, location),
            '<' => Token.init(this.indentation, .LeftAngle, location),
            '>' => Token.init(this.indentation, .RightAngle, location),
            '.' => if (this.nextIfEq('.'))
                Token.init(this.indentation, .DoubleDot, location)
            else
                Token.init(this.indentation, .Dot, location),
            ' ' => Token.init(this.indentation, .Space, location),
            '?' => if (this.nextIfEq('?'))
                Token.init(this.indentation, .DoubleQuestionMark, location)
            else
                Token.init(this.indentation, .QuestionMark, location),
            else => |c| raise(error.ParseError, this.err_msg, location, "Invalid operator `{u}`.", .{c}),
        };
    }
};

pub const Parser = struct {
    allocator: Allocator,
    tokenizer: Tokenizer,
    err_msg: ErrMsg,
    parsing_comma_separated_expressions: bool,

    const This = @This();

    fn createNode(this: *This, comptime T: type, args: anytype) !*T {
        var node = try this.allocator.create(T);
        node.* = @call(.{ .modifier = .always_inline }, T.init, args);
        return node;
    }

    fn check(this: *This, kind: TokenKind) !bool {
        const next = try this.tokenizer.peek(0);
        return if (next) |n| n.data == kind else false;
    }

    fn skipCheck(this: *This, kind: TokenKind) !bool {
        try this.skipNewlines();
        return this.check(kind);
    }

    fn peekCheck(this: *This, kind: TokenKind) !?usize {
        const i = try this.peekNewlines();
        if (try this.tokenizer.peek(i)) |token| {
            if (token.data == kind) {
                return i;
            }
        }
        return null;
    }

    fn checkEof(this: *This) !bool {
        return (try this.tokenizer.peek(0)) == null;
    }

    fn skipCheckEof(this: *This) !bool {
        try this.skipNewlines();
        return this.checkEof();
    }

    fn peekCheckEof(this: *This) !bool {
        const i = try this.peekNewlines();
        return (try this.tokenizer.peek(i)) == null;
    }

    fn match(this: *This, kind: TokenKind) !?Token {
        if (try this.check(kind)) {
            return this.tokenizer.next();
        }
        return null;
    }

    fn skipMatch(this: *This, kind: TokenKind) !?Token {
        try this.skipNewlines();
        return this.match(kind);
    }

    fn peekMatch(this: *This, kind: TokenKind) !?Token {
        const i = try this.peekNewlines();
        if (try this.tokenizer.peek(i)) |t| {
            if (t.data == kind) {
                this.flushPeekedNewlines();
                return this.tokenizer.next();
            }
        }
        return null;
    }

    fn eat(this: *This, kind: TokenKind) !void {
        _ = try this.match(kind);
    }

    fn expect(this: *This, kind: TokenKind, comptime err_msg: []const u8) !Token {
        const next = try this.tokenizer.peek(0);

        if (next) |n| {
            if (n.data != kind) {
                return raise(error.ParseError, &this.err_msg, n.location, err_msg, .{});
            } else {
                _ = this.tokenizer.next() catch unreachable;
                return n;
            }
        }

        return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), err_msg, .{});
    }

    fn skipExpect(this: *This, kind: TokenKind, comptime err_msg: []const u8) !Token {
        try this.skipNewlines();
        return this.expect(kind, err_msg);
    }

    fn peekExpect(this: *This, kind: TokenKind, comptime err_msg: []const u8) !Token {
        const i = this.peekNewlines();
        if (try this.tokenizer.peek(i)) |t| {
            if (t.data == kind) {
                this.flushPeekedNewlines();
                return (this.tokenizer.next() catch unreachable).?;
            } else {
                return raise(error.ParseError, &this.err_msg, t.location, err_msg, .{});
            }
        }

        return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), err_msg, .{});
    }

    fn eatStatementTerminator(this: *This) !void {
        if (try this.tokenizer.peek(0)) |token| {
            return switch (token.data) {
                .Newline, .Semicolon => {
                    _ = this.tokenizer.next() catch unreachable;
                },
                .RightCurly => return,
                else => {},
            };
        }
    }

    fn skipNewlines(this: *This) !void {
        while (try this.tokenizer.peek(0)) |t| {
            if (t.data != .Newline) {
                break;
            }
            _ = this.tokenizer.next() catch unreachable;
        }
    }

    fn peekNewlines(this: *This) !usize {
        var i: usize = 0;
        while (try this.tokenizer.peek(i)) |t| {
            if (t.data != .Newline) {
                break;
            }
            i += 1;
        }
        return i;
    }

    fn flushPeekedNewlines(this: *This) void {
        while (true) {
            if (this.tokenizer.peeked_tokens.items.len == 0 or this.tokenizer.peeked_tokens.items[0].data != .Newline) {
                break;
            }
            _ = this.tokenizer.peeked_tokens.orderedRemove(0);
        }
    }

    fn parseDeclaration(this: *This) anyerror!*Ast {
        return if (try this.match(.Def)) |token|
            (try this.parseDef(token)).asAst()
        else if (try this.match(.Var)) |token|
            try this.parseVar(token)
        else if (try this.match(.Type)) |token|
            try this.parseType(token)
        else
            this.parseStatement();
    }

    fn parseStatement(this: *This) anyerror!*Ast {
        const node = try this.parseExpressionOrAssignment();
        try this.eatStatementTerminator();
        return node;
    }

    fn parseExpressionOrAssignment(this: *This) anyerror!*Ast {
        return this.parsePrecedence(.Assignment);
    }

    fn parseExpression(this: *This) anyerror!*Ast {
        const node = try this.parseExpressionOrAssignment();
        if (node.kind == .Assign) {
            return raise(error.ParseError, &this.err_msg, node.token.location, "Cannot assign in expression context.", .{});
        }

        return node;
    }

    fn parsePrecedence(this: *This, precedence: TokenPrecedence) anyerror!*Ast {
        try this.skipNewlines();

        const token = (try this.tokenizer.next()) orelse {
            return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Unexpected end of file.", .{});
        };

        var previous = try this.parsePrefix(token);
        while (true) {
            if (try this.tokenizer.peek(0)) |infix| {
                if (@enumToInt(precedence) > @enumToInt(infix.precedence())) {
                    break;
                }
                const next_token = (this.tokenizer.next() catch unreachable).?;
                previous = try this.parseInfix(next_token, previous);
            } else {
                break;
            }
        }

        return previous;
    }

    fn parsePrefix(this: *This, token: Token) anyerror!*Ast {
        switch (token.data) {
            // Literals
            .None => {
                return (try this.createLiteral(.None, token, AstLiteral.Literal.None)).asAst();
            },
            .Bool => |value| {
                return (try this.createLiteral(.Bool, token, AstLiteral.Literal{ .Bool = value })).asAst();
            },
            .Char => |value| {
                return (try this.createLiteral(.Char, token, AstLiteral.Literal{ .Char = value })).asAst();
            },
            .Int => |value| {
                return (try this.createLiteral(.Int, token, AstLiteral.Literal{ .Int = value })).asAst();
            },
            .Num => |value| {
                return (try this.createLiteral(.Num, token, AstLiteral.Literal{ .Num = value })).asAst();
            },
            .Str => |value| {
                return (try this.createLiteral(.Str, token, AstLiteral.Literal{ .Str = value })).asAst();
            },
            .Ident => |ident| {
                const node = try this.createNode(AstIdent, .{ token, ident, false });
                return node.asAst();
            },
            .BigIdent => |ident| {
                const node = try this.createNode(AstIdent, .{ token, ident, true });
                return node.asAst();
            },

            // Delimeters
            .LeftParen => {
                const old_pcse = this.parsing_comma_separated_expressions;
                this.parsing_comma_separated_expressions = false;
                defer this.parsing_comma_separated_expressions = old_pcse;

                const expr = try this.parseExpression();
                _ = try this.expect(.RightParen, "Expected `)` to terminate parenthesized expression.");
                return expr;
            },
            .LeftSquare => {
                const old_pcse = this.parsing_comma_separated_expressions;
                this.parsing_comma_separated_expressions = false;
                defer this.parsing_comma_separated_expressions = old_pcse;

                return (try this.parseList(token)).asAst();
            },

            // Operators
            .Bang => {
                return (try this.parseUnary(.Not, token)).asAst();
            },
            .Dash => {
                return (try this.parseUnary(.Negate, token)).asAst();
            },

            // Keywords
            .Do => {
                return (try this.parseBlock(token.indentation)).asAst();
            },
            .If => {
                return (try this.parseIf(token)).asAst();
            },
            .While => {
                return (try this.parseWhile(token)).asAst();
            },
            .For => {
                return (try this.parseFor(token)).asAst();
            },
            // .Var => {
            //     return (try this.parseVar(token)).asAst();
            // },
            // .Println => {
            //     return (try this.parsePrintln(token)).asAst();
            // },

            else => {
                return raise(error.ParseError, &this.err_msg, token.location, "`{!}` is not a prefix operation.", .{token});
            },
        }
    }

    fn parseInfix(this: *This, token: Token, previous: *Ast) anyerror!*Ast {
        const prec = token.precedence();
        switch (token.data) {
            .Plus => {
                return (try this.parseBinary(prec, .Add, token, previous)).asAst();
            },
            .Dash => {
                return (try this.parseBinary(prec, .Subtract, token, previous)).asAst();
            },
            .Star => {
                return (try this.parseBinary(prec, .Multiply, token, previous)).asAst();
            },
            .Slash => {
                return (try this.parseBinary(prec, .Divide, token, previous)).asAst();
            },
            .Equal => {
                return (try this.parseBinary(prec, .Assign, token, previous)).asAst();
            },
            .BangEqual => {
                return (try this.parseBinary(prec, .NotEqual, token, previous)).asAst();
            },
            .DoubleEqual => {
                return (try this.parseBinary(prec, .Equal, token, previous)).asAst();
            },
            .LeftAngle => {
                return (try this.parseBinary(prec, .LessThan, token, previous)).asAst();
            },
            .RightAngle => {
                return (try this.parseBinary(prec, .GreaterThan, token, previous)).asAst();
            },
            .Or => {
                return (try this.parseBinary(prec, .Or, token, previous)).asAst();
            },
            .And => {
                return (try this.parseBinary(prec, .And, token, previous)).asAst();
            },
            .Dot => {
                const ident = (try this.parseIdent()) orelse return raise(error.ParseError, &this.err_msg, token.location, "Expected an identifier after `.` operator.", .{}); 

                const node = try this.createNode(AstBinary, .{ .Dot, token, previous, ident.asAst() });
                return node.asAst();
            },
            .DoubleDot => {
                return (try this.parseBinary(prec, .ExclusiveRange, token, previous)).asAst();
            },
            .QuestionMark => {
                return (try this.createNode(AstUnary, .{ .Unwrap, token, previous })).asAst();
            },
            .DoubleQuestionMark => {
                return (try this.parseBinary(prec, .NoneOr, token, previous)).asAst();
            },

            .Space => {
                const terminator = if (this.parsing_comma_separated_expressions) TokenKind.Comma else TokenKind.Newline;
                const args = try this.parseCommaSeparatedExpressions(terminator, token);
                _ = try this.match(.Space);

                const call = try this.createNode(AstBinary, .{ .Call, token, previous, args.asAst() });
                return call.asAst();
            },
            .LeftParen => {
                const args = try this.parseCommaSeparatedExpressions(.RightParen, token);
                _ = try this.expect(.RightParen, "Expected `)` to terminate call operator.");

                const call = try this.createNode(AstBinary, .{ .Call, token, previous, args.asAst() });
                return call.asAst();
            },
            .LeftSquare => {
                const old_pcse = this.parsing_comma_separated_expressions;
                this.parsing_comma_separated_expressions = false;
                defer this.parsing_comma_separated_expressions = old_pcse;

                const expr = (try this.parseBinary(.Assignment, .Index, token, previous)).asAst();
                _ = try this.expect(.RightSquare, "Expected `]` to terminate index operator.");
                return expr;
            },

            else => {
                return raise(error.ParseError, &this.err_msg, token.location, "`{!}` is not an infix operation.", .{token});
            },
        }
    }

    fn createLiteral(this: *This, kind: AstKind, token: Token, literal: AstLiteral.Literal) !*AstLiteral {
        var node = try this.allocator.create(AstLiteral);
        node.* = AstLiteral.init(kind, token, literal);
        return node;
    }

    fn parseUnary(this: *This, kind: AstKind, token: Token) !*AstUnary {
        const sub = try this.parsePrecedence(.Unary);
        return this.createNode(AstUnary, .{ kind, token, sub });
    }

    fn parseBinary(this: *This, precedence: TokenPrecedence, kind: AstKind, token: Token, lhs: *Ast) !*AstBinary {
        const rhs = try this.parsePrecedence(precedence.next());
        return this.createNode(AstBinary, .{ kind, token, lhs, rhs });
    }

    fn parseBlock(this: *This, indentation: usize) !*AstBlock {
        var nodes = ArrayListUnmanaged(*Ast){};

        try this.skipNewlines();
        const first = (try this.tokenizer.peek(0)) orelse return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Unexpected end of file.", .{});
        if (first.indentation <= indentation) {
            return raise(error.ParseError, &this.err_msg, first.location, "Expressions inside a block must be at a higher indentation level than the block itself.", .{});
        }
        if (first.data == .Pass) {
            _ = try this.tokenizer.next();
            return this.createNode(AstBlock, .{ .Block, first, nodes.items });
        }

        while (true) {
            try this.skipNewlines();
            if (try this.tokenizer.peek(0)) |t| {
                if (t.indentation != first.indentation) {
                    if (t.indentation > first.indentation) {
                        return raise(error.ParseError, &this.err_msg, t.location, "Expression is at incorrect indentation level for block.", .{});
                    }
                    break;
                }

                try nodes.append(this.allocator, try this.parseDeclaration());
            } else {
                break;
            }
        }

        return this.createNode(AstBlock, .{ .Block, first, nodes.items });
    }

    fn parseCommaSeparatedExpressions(this: *This, terminator: TokenKind, token: Token) !*AstBlock {
        var nodes = ArrayListUnmanaged(*Ast){};

        const old_pcse = this.parsing_comma_separated_expressions;
        this.parsing_comma_separated_expressions = true;
        defer this.parsing_comma_separated_expressions = old_pcse;

        while (true) {
            if (terminator != .Newline) try this.skipNewlines();
            
            if (try this.checkEof()) {
                break;
            }

            if (try this.check(terminator)) {
                break;
            }

            try nodes.append(this.allocator, try this.parseExpression());

            if (terminator != .Newline and (try this.match(.Newline)) != null) {
                try this.skipNewlines();
                try this.eat(.Comma);
            } else if (terminator != .Comma and (try this.match(.Comma)) != null) {
                // carry on
            } else {
                break;
            }
        }

        return this.createNode(AstBlock, .{ .Comma, token, nodes.items });
    }

    fn parseList(this: *This, token: Token) !*AstBlock {
        const list = try this.parseCommaSeparatedExpressions(.RightSquare, token);
        _ = try this.expect(.RightSquare, "Expected `]` to terminate List literal.");

        list.kind = .List;
        return list;
    }

    fn parseIdent(this: *This) std.mem.Allocator.Error!?*AstIdent {
        const ident_token = this.expect(.Ident, "") catch return null;
        switch (ident_token.data) {
            .Ident => |ident| {
                return try this.createNode(AstIdent, .{ ident_token, ident, false });
            },
            else => unreachable,
        }
    }

    fn parseBigIdent(this: *This) std.mem.Allocator.Error!?*AstIdent {
        const ident_token = this.expect(.BigIdent, "") catch return null;
        switch (ident_token.data) {
            .BigIdent => |ident| {
                return try this.createNode(AstIdent, .{ ident_token, ident, true });
            },
            else => unreachable,
        }
    }

    fn parseIf(this: *This, token: Token) anyerror!*AstIf {
        const condition = try this.parseExpression();

        const then_block = try this.parseBlock(token.indentation);

        var else_block: ?*Ast = null;
        if (try this.peekCheck(.Else)) |else_index| {
            const else_token = (this.tokenizer.peek(else_index) catch unreachable).?;
            if (else_token.indentation == token.indentation) {
                _ = this.skipExpect(.Else, "") catch unreachable;
                else_block = (try this.parseBlock(token.indentation)).asAst();
            }
        } else if (try this.peekCheck(.Elif)) |elif_index| {
            const elif_token = (this.tokenizer.peek(elif_index) catch unreachable).?;
            if (elif_token.indentation == token.indentation) {
                _ = this.skipExpect(.Elif, "") catch unreachable;
                else_block = (try this.parseIf(elif_token)).asAst();
            }
        }

        return this.createNode(AstIf, .{ token, condition, then_block, else_block });
    }

    fn parseWhile(this: *This, token: Token) !*AstWhile {
        const condition = try this.parseExpression();
        const block = try this.parseBlock(token.indentation);

        return this.createNode(AstWhile, .{ token, condition, block });
    }

    fn parseFor(this: *This, token: Token) !*AstFor {
        const iterator = (try this.parseIdent()) orelse {
            return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected identifier after `for` keyword.", .{});
        };
        _ = try this.skipExpect(.In, "Expected `in` keyword in for loop.");
        const container = try this.parseExpression();
        const block = try this.parseBlock(token.indentation);

        return this.createNode(AstFor, .{ token, iterator, container, block });
    }

    fn parseDef(this: *This, token: Token) !*AstDef {
        try this.skipNewlines();
        const ident = (try this.parseIdent()) orelse return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `def` keyword.", .{});

        // _ = try this.skipExpect(.LeftParen, "Expected `(` to begin parameter list.");
        const parens = (try this.skipMatch(.LeftParen)) != null;

        if (!parens) {
            _ = try this.expect(.Space, "Expected either `(` or a space to begin parameter list.");
        }

        var params = ArrayListUnmanaged(*AstParam){};
        while (true) {
            if ((try this.check(.RightParen)) or (try this.check(.Equal)) or (try this.checkEof())) {
                break;
            }

            const param_name = (try this.parseIdent()) orelse {
                return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected a parameter name.", .{});
            };

            const param_ident = try this.createNode(AstIdent, .{ param_name.token, param_name.ident, false });

            var param_type: ?*AstTypeSignature = null;
            if ((try this.match(.Colon)) != null) {
                param_type = try this.parseTypeSignature(true);
            }

            const param = try this.createNode(AstParam, .{ param_name.token, param_ident, param_type });
            try params.append(this.allocator, param);

            if ((try this.match(.Newline)) != null) {
                try this.skipNewlines();
                try this.eat(.Comma);
            } else if ((try this.match(.Comma)) != null) {
                // carry on
            } else {
                break;
            }
        }

        if (parens) {
            _ = try this.expect(.RightParen, "Expected `)` to terminate parameter list.");
        }

        var ret_type: ?*AstTypeSignature = null;
        if ((try this.match(.Arrow)) != null) {
            ret_type = try this.parseTypeSignature(true);
        }

        _ = try this.expect(.Equal, "Expected `=` after function signature.");

        const body = try this.parseBlock(token.indentation);

        return this.createNode(AstDef, .{ token, ident, params.items, ret_type, body });
    }

    fn parseVar(this: *This, token: Token) !*Ast {
        var decls = ArrayListUnmanaged(*AstVar){};

        try this.skipNewlines();
        const first = (try this.tokenizer.peek(0)) orelse return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `var` keyword.", .{});
        if (first.indentation < token.indentation) {
            return raise(error.ParseError, &this.err_msg, first.location, "Expressions after `var` must be at a higher or equal indentation level.", .{});
        } else if (first.indentation == token.indentation) {
            const ident = (try this.parseIdent()) orelse {
                return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `var` keyword.", .{});
                };

            var specified_type: ?*AstTypeSignature = null;
            if ((try this.match(.Colon)) != null) {
                specified_type = try this.parseTypeSignature(true);
            }

            const eql_token = try this.expect(.Equal, "Expected `=` after identifier in variable declaration.");
            const initializer = try this.parseExpression();
            
            const decl = try this.createNode(AstVar, .{ eql_token, ident, initializer, specified_type });
            try decls.append(this.allocator, decl);
        }
        
        while (true) {
            try this.skipNewlines();
            if (try this.tokenizer.peek(0)) |t| {
                if (t.indentation != first.indentation or t.indentation == token.indentation) {
                    if (t.indentation > first.indentation) {
                        return raise(error.ParseError, &this.err_msg, first.location, "Expressions after `var` must be at a higher or equal indentation level.", .{});
                    }
                    break;
                }

                const ident = (try this.parseIdent()) orelse {
                    return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `var` keyword.", .{});
                };

                var specified_type: ?*AstTypeSignature = null;
                if ((try this.match(.Colon)) != null) {
                    specified_type = try this.parseTypeSignature(true);
                }

                const eql_token = try this.expect(.Equal, "Expected `=` after identifier in variable declaration.");
                const initializer = try this.parseExpression();

                const decl = try this.createNode(AstVar, .{ eql_token, ident, initializer, specified_type });
                try decls.append(this.allocator, decl);
            } else {
                break;
            }
        }

        if (decls.items.len == 1) {
            return decls.items[0].asAst();
        } else {
            return (try this.createNode(AstVarBlock, .{ token, decls.items })).asAst();
        }
    }

    fn parseType(this: *This, token: Token) !*Ast {
        var decls = ArrayListUnmanaged(*AstType){};

        try this.skipNewlines();
        const first = (try this.tokenizer.peek(0)) orelse return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `type` keyword.", .{});
        if (first.indentation < token.indentation) {
            return raise(error.ParseError, &this.err_msg, first.location, "Expressions after `type` must be at a higher or equal indentation level.", .{});
        } else if (first.indentation == token.indentation) {
            const ident = (try this.parseBigIdent()) orelse {
                return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `type` keyword.", .{});
            };
            const eql_token = try this.expect(.Equal, "Expected `=` after identifier in type declaration.");
            const signature = try this.parseTypeSignature(true);

            const decl = try this.createNode(AstType, .{ eql_token, ident, signature });
            try decls.append(this.allocator, decl);
        }
        
        while (true) {
            try this.skipNewlines();
            if (try this.tokenizer.peek(0)) |t| {
                if (t.indentation != first.indentation or t.indentation == token.indentation) {
                    if (t.indentation > first.indentation) {
                        return raise(error.ParseError, &this.err_msg, first.location, "Expressions after `type` must be at a higher or equal indentation level.", .{});
                    }
                    break;
                }

                const ident = (try this.parseBigIdent()) orelse {
                    return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected an identifier after `type` keyword.", .{});
                };
                const eql_token = try this.expect(.Equal, "Expected `=` after identifier in type declaration.");
                const signature = try this.parseTypeSignature(true);

                const decl = try this.createNode(AstType, .{ eql_token, ident, signature });
                try decls.append(this.allocator, decl);
            } else {
                break;
            }
        }

        if (decls.items.len == 1) {
            return decls.items[0].asAst();
        } else {
            return (try this.createNode(AstTypeBlock, .{ token, decls.items })).asAst();
        }
    }

    fn parseTypeSignature(this: *This, parse_unions: bool) anyerror!*AstTypeSignature {
        var sig: *AstTypeSignature = undefined;

        try this.skipNewlines();
        if (try this.match(.BigIdent)) |big_ident| {
            var ident = switch (big_ident.data) { .BigIdent => |id| id, else => unreachable };
            sig = try this.allocator.create(AstTypeSignature);
            sig.* = AstTypeSignature.init(big_ident, AstTypeSignature.Data{ .Name = ident });
        } else if (try this.match(.None)) |none| {
            sig = try this.allocator.create(AstTypeSignature);
            sig.* = AstTypeSignature.init(none, AstTypeSignature.Data{ .Name = "None" });
        } else if (try this.match(.LeftParen)) |paren| {
            sig = try this.parseTupleTypeSignature(paren);
        } else if (try this.match(.LeftSquare)) |square| {
            sig = try this.parseTagTypeSignature(square);
        } else if (try this.match(.QuestionMark)) |qmark| {
            sig = try this.parseOptionalTypeSignature(qmark);
        } else if (try this.match(.Pipe)) |pipe| {
            sig = try this.parseUnionTypeSignature(pipe, null);
        } else {
            return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected a type signature.", .{});
        }

        if (parse_unions and (try this.match(.Pipe)) != null) {
            sig = try this.parseUnionTypeSignature(sig.token, sig);
        }

        return sig;
    }

    fn parseTupleTypeSignature(this: *This, token: Token) !*AstTypeSignature {
        var field_names = ArrayListUnmanaged(*AstIdent){};
        var field_types = ArrayListUnmanaged(?*AstTypeSignature){};

        while (true) {
            try this.skipNewlines();
            if ((try this.check(.RightParen)) or (try this.checkEof())) {
                break;
            }

            const name = (try this.parseIdent()) orelse return raise(error.ParseError,  &this.err_msg, this.tokenizer.currentLocation(), "Expected field name in tuple type signature.", .{});
            try field_names.append(this.allocator, name);

            if ((try this.match(.Colon)) != null) {
                const sig = try this.parseTypeSignature(true);
                try field_types.append(this.allocator, sig);
            } else {
                try field_types.append(this.allocator, null);
            }

            if ((try this.match(.Newline)) != null) {
                try this.skipNewlines();
                try this.eat(.Comma);
            } else if ((try this.match(.Comma)) != null) {
                // carry on
            } else {
                break;
            }
        }

        _ = try this.expect(.RightParen, "Expected `)` to terminate tuple type signature.");

        var node = try this.allocator.create(AstTypeSignature);
        node.* = AstTypeSignature.init(token, AstTypeSignature.Data{ .Tuple = .{ .field_names = field_names.items, .field_types = field_types.items } });
        return node;
    }

    fn parseTagTypeSignature(this: *This, token: Token) !*AstTypeSignature {
        var tag_names = ArrayListUnmanaged(*AstIdent){};

        while (true) {
            try this.skipNewlines();
            if ((try this.check(.RightSquare)) or (try this.checkEof())) {
                break;
            }

            const name = (try this.parseBigIdent()) orelse return raise(error.ParseError, &this.err_msg, this.tokenizer.currentLocation(), "Expected a Tag name in Tag type signature.", .{});
            try tag_names.append(this.allocator, name);

            if ((try this.match(.Newline)) != null) {
                try this.skipNewlines();
                try this.eat(.Comma);
            } else if ((try this.match(.Comma)) != null) {
                // carry on
            } else {
                break;
            }
        }

        _ = try this.expect(.RightSquare, "Expected `]` to terminate tuple type signature.");

        var node = try this.allocator.create(AstTypeSignature);
        node.* = AstTypeSignature.init(token, AstTypeSignature.Data{ .Tag = .{ .tag_names = tag_names.items } });
        return node;
    }

    fn parseUnionTypeSignature(this: *This, token: Token, previous: ?*AstTypeSignature) !*AstTypeSignature {
        var variant_signautres = ArrayListUnmanaged(*AstTypeSignature){};
        errdefer variant_signautres.deinit(this.allocator);

        if (previous) |p| {
            try variant_signautres.append(this.allocator, p);
        }

        while (!try this.checkEof()) {
            const variant = try this.parseTypeSignature(false);
            try variant_signautres.append(this.allocator, variant);

            if ((try this.skipMatch(.Pipe)) == null) break;
        }     

        var node = try this.allocator.create(AstTypeSignature);
        node.* = AstTypeSignature.init(token, AstTypeSignature.Data{ .Union = .{ .variants = variant_signautres.items }});
        return node;
    }

    fn parseOptionalTypeSignature(this: *This, token: Token) !*AstTypeSignature {
        const some_variant = try this.parseTypeSignature(true);
        var node = try this.allocator.create(AstTypeSignature);
        node.* = AstTypeSignature.init(token, AstTypeSignature.Data{ .Optional = some_variant });
        return node;
    }

    pub fn parse(this: *This) !ArrayListUnmanaged(*Ast) {
        var nodes = ArrayListUnmanaged(*Ast){};

        // @TODO:
        // parse module statement and imports
        //

        while (true) {
            try this.skipNewlines();
            if ((try this.tokenizer.peek(0)) == null) {
                break;
            }

            const node = try this.parseDeclaration();
            try nodes.append(this.allocator, node);
        }

        return nodes;
    }
};
