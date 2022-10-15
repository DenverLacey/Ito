const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const StringArrayHashMapUnmanaged = std.StringArrayHashMapUnmanaged;

const AstBlock = @import("ast.zig").AstBlock;

const err = @import("errors.zig");
const ErrMsg = err.ErrMsg;
const raise = err.raise;
const todo = err.todo;

const CodeLocation = @import("parser.zig").CodeLocation;

const interpreter = @import("interpreter.zig");
const Interpreter = interpreter.Interpreter;

pub const Type = union(enum) {
    Any,
    None,
    Bool,
    Char,
    Int,
    Num,
    Str,
    Type,
    // RangeChar,
    RangeInt,
    List: u32,
    Tuple: u32,
    Tag: u32,
    Union: u32,
    Lambda: u32,

    const This = @This();

    pub fn eql(this: This, other: This) bool {
        switch (this) {
            .Any => switch (other) {
                .Any => return true,
                else => return false,
            },
            .None => switch (other) {
                .None => return true,
                else => return false,
            },
            .Bool => switch (other) {
                .Bool => return true,
                else => return false,
            },
            .Char => switch (other) {
                .Char => return true,
                else => return false,
            },
            .Int => switch (other) {
                .Int => return true,
                else => return false,
            },
            .Num => switch (other) {
                .Num => return true,
                else => return false,
            },
            .Str => switch (other) {
                .Str => return true,
                else => return false,
            },
            .Type => switch (other) {
                .Type => return true,
                else => return false,
            },
            // .RangeChar => switch (other) {
            //     .RangeChar => return true,
            //     else => return false,
            // },
            .RangeInt => switch (other) {
                .RangeInt => return true,
                else => return false,
            },
            .List => |this_index| switch (other) {
                .List => |other_index| return this_index == other_index,
                else => return false,
            },
            .Tuple => |this_index| switch (other) {
                .Tuple => |other_index| return this_index == other_index,
                else => return false,
            },
            .Tag => |this_index| switch (other) {
                .Tag => |other_index| return this_index == other_index,
                else => return false,
            },
            .Union => |this_index| switch (other) {
                .Union => |other_index| return this_index == other_index,
                else => return false,
            },
            .Lambda => |this_index| switch (other) {
                .Lambda => |other_index| return this_index == other_index,
                else => return false,
            },
        }
    }

    pub fn compat(this: This, with: This) bool {
        switch (this) {
            .Any => switch (with) {
                .Any => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .None => switch (with) {
                .Any, .None => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Bool => switch (with) {
                .Any, .Bool => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Char => switch (with) {
                .Any, .Char => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Int => switch (with) {
                .Any, .Int => return true,
                .Num => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Num => switch (with) {
                .Any, .Num => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Str => switch (with) {
                .Any, .Str => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Type => switch (with) {
                .Any, .Type => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            // .RangeChar => switch (with) {
            //     .RangeChar => return true,
            //     else => return false,
            // },
            .RangeInt => switch (with) {
                .RangeInt => return true,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .List => |index| switch (with) {
                .Any => return true,
                .List => |with_index| return index == with_index,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Tuple => |index| switch (with) {
                .Any => return true,
                .Tuple => |with_index| return index == with_index,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Tag => |index| switch (with) {
                .Any => return true,
                .Tag => |with_index| return index == with_index,
                .Union => |with_index| return this.compatWithUnion(with_index),
                else => return false,
            },
            .Union => |index| switch (with) {
                .Any => return true,
                .Union => |with_index| {
                    const interp = Interpreter.get();
                    const this_union = interp.union_types.items[index];
                    const with_union = interp.union_types.items[with_index];
                    const superset = with_union.isSuperset(this_union.variants);
                    return superset;
                },
                else => return false,
            },
            .Lambda => |index| switch (with) {
                .Any => return true,
                .Lambda => |with_index| return index == with_index,
                else => return false,
            },
        }
    }

    fn compatWithUnion(this: This, union_index: u32) bool {
        const interp = Interpreter.get();
        const with_union = interp.union_types.items[union_index];
        const superset = with_union.isSuperset(&[_]Type{this});
        return superset;
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        const interp = Interpreter.get();
        switch (this.*) {
            .Any => _ = try writer.write("Any"),
            .None => _ = try writer.write("None"),
            .Bool => _ = try writer.write("Bool"),
            .Char => _ = try writer.write("Char"),
            .Int => _ = try writer.write("Int"),
            .Num => _ = try writer.write("Num"),
            .Str => _ = try writer.write("Str"),
            .Type => _ = try writer.write("Type"),
            // .RangeChar => _ = try writer.write("Range[Char]"),
            .RangeInt => _ = try writer.write("Range[Int]"),
            .List => |index| {
                const list_type = interp.list_types.items[index];
                try writer.print("List[{}]", .{list_type.item_type});
            },
            .Tuple => |index| {
                const tuple_type = interp.tuple_types.items[index];

                _ = try writer.write("{ ");
                for (tuple_type.fields) |field, i| {
                    try writer.print("{s} : {}", .{field.name, field.typ});
                    if (i < tuple_type.fields.len - 1) {
                        _ = try writer.write(", ");
                    }
                }
                _ = try writer.write(" }");
            },
            .Tag => |index| {
                const tag_type = interp.tag_types.items[index];

                // @TODO: Payload stuff

                _ = try writer.write("[");
                for (tag_type.variants) |variant, i| {
                    try writer.print("{s}", .{variant.name});
                    if (i < tag_type.variants.len - 1) {
                        _ = try writer.write(", ");
                    }
                }
                _ = try writer.write("]");
            },
            .Union => |index| {
                const union_type = interp.union_types.items[index];

                for (union_type.variants) |variant, i| {
                    try writer.print("{}", .{variant});
                    if (i < union_type.variants.len - 1) {
                        _ = try writer.write("|");
                    }
                }
            },
            .Lambda => |index| {
                const lambda_type = interp.lambda_types.items[index];

                _ = try writer.write("(");

                for (lambda_type.parameters) |param, i| {
                    if (param.typ == .Any) {
                        try writer.print("{s}", .{param.name});
                    } else {
                        try writer.print("{}", .{param.typ});
                    }

                    if (i < lambda_type.parameters.len - 1) {
                        _ = try writer.write(", ");
                    }
                }

                try writer.print(") -> {}", .{lambda_type.returns});
            },
        }
    }
};

pub const NamedParam = struct {
    name: []const u8,
    typ: Type,
};

pub const ListTypeDefinition = struct {
    item_type: Type,
};

pub const TupleTypeDefinition = struct {
    fields: []Field,

    const This = @This();

    pub const Field = NamedParam;

    pub fn findFieldByName(this: *const This, name: []const u8) ?Field {
        for (this.fields) |field| {
            if (std.mem.eql(u8, field.name, name)) {
                return field;
            }
        }
        return null;
    }
};

pub const TagTypeDefinition = struct {
    variants: []Variant,

    pub const Variant = struct {
        name: []const u8,
        // @TODO: Add payload stuff
    };
};

pub const UnionTypeDefinition = struct {
    variants: []Type,

    const This = @This();

    pub fn isSuperset(this: *const This, other: []const Type) bool {
        var superset = true;
        for (other) |other_variant| {
            var found = false;
            for (this.variants) |variant| {
                if (variant.eql(other_variant)) {
                    found = true;
                    break;
                }
            }

            if (!found) {
                superset = false;
                break;
            }
        }
        return superset;
    }
};

pub const LambdaTypeDefinition = struct {
    parameters: []Parameter,
    returns: Type,

    pub const Parameter = NamedParam;
};

pub const Value = union(ValueKind) {
    None,
    Bool: bool,
    Char: Char,
    Int: i64,
    Num: f64,
    Str: []const u8,
    Range: Range,
    List: *List,
    Closure: *Closure,
    Type: Type,
    Tuple: *Tuple,
    Tag: *Tag,

    const This = @This();

    pub fn isTrue(this: This) bool {
        return switch (this) {
            .None => false,
            .Bool => |value| value,
            .Char => |value| value != 0,
            .Int => |value| value != 0,
            .Num => |value| value != 0.0,
            .Str => |value| value.len != 0,
            .Range => true,
            .List => |value| value.items.len != 0,
            .Closure => true,
            .Type => true,
            .Tuple => true,
            .Tag => true,
        };
    }

    pub fn eql(this: This, other: This) bool {
        return switch (this) {
            .None => switch (other) {
                .None => true,
                else => false,
            },
            .Bool => |value| switch (other) {
                .Bool => |other_value| value == other_value,
                else => false,
            },
            .Char => |value| switch (other) {
                .Char => |other_value| value == other_value,
                else => false,
            },
            .Int => |value| switch (other) {
                .Int => |other_value| value == other_value,
                else => false,
            },
            .Num => |value| switch (other) {
                .Num => |other_value| value == other_value,
                else => false,
            },
            .Str => |value| switch (other) {
                .Str => |other_value| std.mem.eql(u8, value, other_value),
                else => false,
            },
            .Range => |value| switch (other) {
                .Range => |other_value| value.start == other_value.start and value.end == other_value.end,
                else => false,
            },
            .List => |value| switch (other) {
                .List => |other_value| blk: {
                    if (value.items.len != other_value.items.len) {
                        break :blk false;
                    }

                    for (value.items) |item, i| {
                        const other_item = other_value.items[i];
                        if (!item.eql(other_item)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                else => false,
            },
            .Closure => |value| switch (other) {
                .Closure => |other_value| value.code == other_value.code,
                else => false,
            },
            .Type => |typ| switch (other) {
                .Type => |other_typ| typ.eql(other_typ),
                else => false,
            },
            .Tuple => |value| switch (other) {
                .Tuple => |other_value| blk: {
                    if (!value.typ.eql(other_value.typ))
                        break :blk false;
                    
                    // @NOTE:
                    // Using the values slice might be
                    // error prone depending on if the
                    // order of the values isn't 
                    // guarenteed but i think it is
                    // because if they have the same
                    // type then the hashes for the fields
                    // should be the same.
                    //

                    const fields = value.fields.values();
                    const other_fields = other_value.fields.values();
                    std.debug.assert(fields.len == other_fields.len);

                    var i: usize = 0;
                    while (i < fields.len) : (i += 1) {
                        const field = fields[i];
                        const other_field = other_fields[i];

                        if (!field.eql(other_field)) {
                            break :blk false;
                        }
                    }

                    break :blk true;
                },
                else => false,
            },
            .Tag => |value| switch (other) {
                .Tag => |other_value| value == other_value,
                else => false,
            }
        };
    }

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        switch (this.*) {
            .None => try writer.print("None", .{}),
            .Bool => |value| {
                if (value) {
                    _ = try writer.write("True");
                } else {
                    _ = try writer.write("False");
                }
            },
            .Char => |value| try writer.print("{u}", .{value}),
            .Int => |value| try writer.print("{}", .{value}),
            .Num => |value| try writer.print("{d}", .{value}),
            .Str => |value| try writer.print("{s}", .{value}),
            .Range => |value| try writer.print("{}", .{value}),
            .List => |value| {
                try writer.print("[", .{});
                var i: usize = 0;
                while (i < value.items.len) : (i += 1) {
                    try writer.print("{}", .{value.items[i]});
                    if (i + 1 < value.items.len) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print("]", .{});
            },
            .Closure => |value| {
                try writer.print("{}", .{value});
            },
            .Type => |value| {
                try writer.print("{}", .{value});
            },
            .Tuple => |value| {
                try writer.print("{}", .{value});
            },
            .Tag => |value| {
                try writer.print("{}", .{value});
            }
        }
    }
};

pub const ValueKind = enum {
    None,
    Bool,
    Char,
    Int,
    Num,
    Str,
    Range,
    List,
    Closure,
    Type,
    Tuple,
    Tag,
};

pub const Char = u21;

pub const Range = struct {
    start: i64,
    end: i64,

    const This = @This();

    pub fn init(start: i64, end: i64) This {
        return This{ .start = start, .end = end };
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{}..{}", .{this.start, this.end});
    }
};

pub const List = ArrayListUnmanaged(Value);

pub const Closure = struct {
    name: []const u8,
    type_def: LambdaTypeDefinition,
    code: *AstBlock,
    closed_values: []Value,
    closed_value_names: [][]const u8,

    const This = @This();

    pub fn init(
        name: []const u8,
        type_def: LambdaTypeDefinition,
        code: *AstBlock,
        closed_values: []Value,
        closed_value_names: [][]const u8,
    ) This {
        return This{
            .name = name,
            .type_def = type_def,
            .code = code,
            .closed_values = closed_values,
            .closed_value_names = closed_value_names,
        };
    }

    pub fn deinit(_: *This, _: Allocator) void {}

    // pub fn makeBound(
    //     this: *This,
    //     allocator: Allocator,
    //     receiver: Value,
    //     location: CodeLocation,
    //     out_err_msg: *ErrMsg,
    // ) !This {
    //     var bound: This = undefined;
    //     bound.name = this.name;
    //     bound.code = this.code;

    //     bound.params = try allocator.alloc(Parameter, this.params.len - 1);
    //     std.mem.copy(Parameter, bound.params, this.params[1..]);

    //     bound.closed_values = .{};
    //     bound.closed_values.putNoClobber(allocator, this.params[0].name, receiver) catch unreachable;

    //     var it = this.closed_values.iterator();
    //     while (it.next()) |entry| {
    //         if (bound.closed_values.contains(entry.key_ptr.*)) {
    //             return raise(error.RuntimeError, out_err_msg, location, "`{s}` appears in closure `{s}` more than once.", .{ entry.key_ptr.*, this.name });
    //         }

    //         try bound.closed_values.putNoClobber(allocator, entry.key_ptr.*, entry.value_ptr.*);
    //     }

    //     return bound;
    // }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}(", .{this.name});

        for (this.type_def.parameters) |param, i| {
            try writer.print("{s}", .{param.name});
            if (i + 1 < this.type_def.parameters.len) {
                try writer.print(", ", .{});
            }
        }

        try writer.print(")", .{});
    }
};

pub const Tuple = struct {
    typ: Type,
    fields: StringArrayHashMapUnmanaged(Value),

    const This = @This();

    pub fn deinit(this: *This, allocator: Allocator) void {
        this.fields.deinit(allocator);
    }

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        _ = try writer.write("{ ");

        var it = this.fields.iterator();
        var i: usize = 0;
        while (it.next()) |field| : (i += 1) {
            try writer.print("{s} = {}", .{ field.key_ptr.*, field.value_ptr.* });
            if (i + 1 < this.fields.keys().len) {
                _ = try writer.write(", ");
            }
        }

        _ = try writer.write(" }");
    }
};

pub const Tag = struct {
    tag: []const u8,
    // @TODO: Payload stuff

    const This = @This();

    pub fn deinit(_: *This, _: Allocator) void {}

    pub fn format(
        this: *const This,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.print("{s}", .{this.tag});
    }
};


// Import Cut-n-paste
// const values = @import("values.zig");
// const Type = values.Type;
// const TypeDefinition = values.TypeDefinition;
// const TupleTypeDefinition = values.TupleTypeDefinition;
// const TagTypeDefinition = values.TagTypeDefinition;
// const UnionTypeDefinition = values.UnionTypeDefinition;
// const LambdaTypeDefinition = values.LambdaTypeDefinition;
// const Value = values.Value;
// const ValueKind = values.ValueKind;
// const Char = values.Char;
// const Range = values.Range;
// const List = values.List;
// const Closure = values.Closure;
// const Tuple = values.Tuple;
// const Tag = values.Tag;
