const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const Parser = @import("parser.zig").Parser;
const Typer = @import("typer.zig").Typer;
const Evaluator = @import("evaluator.zig").Evaluator;

const values = @import("values.zig");
const Type = values.Type;
const TypeDefinition = values.TypeDefinition;
const ListTypeDefinition = values.ListTypeDefinition;
const TupleTypeDefinition = values.TupleTypeDefinition;
const TagTypeDefinition = values.TagTypeDefinition;
const UnionTypeDefinition = values.UnionTypeDefinition;
const LambdaTypeDefinition = values.LambdaTypeDefinition;

const ErrMsg = @import("errors.zig").ErrMsg;

var interpreter = Interpreter{};

pub const Interpreter = struct {
    allocator: Allocator = undefined,

    // Composite Types
    composite_types: ArrayListUnmanaged(TypeDefinition) = .{},
    list_types: ArrayListUnmanaged(ListTypeDefinition) = .{},
    tuple_types: ArrayListUnmanaged(TupleTypeDefinition) = .{},
    tag_types: ArrayListUnmanaged(TagTypeDefinition) = .{},
    union_types: ArrayListUnmanaged(UnionTypeDefinition) = .{},
    lambda_types: ArrayListUnmanaged(LambdaTypeDefinition) = .{},

    const This = @This();

    pub fn get() *This {
        return &interpreter;
    }

    pub fn findOrAddListType(this: *This, item_type: Type) !Type {
        var typ: ?u32 = null;
        for (this.composite_types.items) |composite_type, composite_index| {
            switch (composite_type) {
                .List => |list_index| {
                    const list_type = this.list_types.items[list_index];
                    if (!list_type.item_type.eql(item_type)) continue;

                    typ = @intCast(u32, composite_index);
                    break;
                },
                else => {},
            }
        }

        if (typ == null) {
            const list_def = ListTypeDefinition{ .item_type = item_type };

            try this.list_types.append(this.allocator, list_def);
            try this.composite_types.append(this.allocator, TypeDefinition{ .List = @intCast(u32, this.list_types.items.len - 1) });
            
            typ = @intCast(u32, this.composite_types.items.len - 1);
        }

        return Type{ .Composite = typ.? };
    }

    pub fn findOrAddTupleType(this: *This, fields: []TupleTypeDefinition.Field) !Type {
        var typ: ?u32 = null;
        for (this.composite_types.items) |composite_type, composite_index| {
            switch (composite_type) {
                .Tuple => |tuple_index| {
                    const tuple_type = this.tuple_types.items[tuple_index];
                    if (fields.len != tuple_type.fields.len) continue;

                    var matches: usize = 0;
                    for (fields) |needle| {
                        for (tuple_type.fields) |field| {
                            if (std.mem.eql(u8, needle.name, field.name)) {
                                // @TODO: Check field types
                                matches += 1;
                                break;
                            }
                        }
                    }

                    if (matches == fields.len) {
                        typ = @intCast(u32, composite_index);
                        break;
                    }
                },
                else => {},
            }
        }

        if (typ == null) {
            const tuple_def = TupleTypeDefinition{ .fields = fields };

            try this.tuple_types.append(this.allocator, tuple_def);
            try this.composite_types.append(this.allocator, TypeDefinition{ .Tuple = @intCast(u32, this.tuple_types.items.len - 1) });
            
            typ = @intCast(u32, this.composite_types.items.len - 1);
        } else {
            this.allocator.free(fields);
        }

        return Type{ .Composite = typ.? };
    }

    pub fn findOrAddTagType(this: *This, variants: []TagTypeDefinition.Variant) !Type {
        var typ: ?u32 = null;
        for (this.composite_types.items) |composite_type, composite_index| {
            switch (composite_type) {
                .Tag => |tag_index| {
                    const tag_type = this.tag_types.items[tag_index];
                    if (variants.len != tag_type.variants.len) continue;

                    var matches: usize = 0;
                    for (variants) |needle| {
                        for (tag_type.variants) |variant| {
                            if (std.mem.eql(u8, needle.name, variant.name)) {
                                // @TODO: Check variant payload
                                matches += 1;
                                break;
                            }
                        }
                    }

                    if (matches == variants.len) {
                        typ = @intCast(u32, composite_index);
                        break;
                    }
                },
                else => {},
            }
        }

        if (typ == null) {
            const tag_def = TagTypeDefinition{ .variants = variants };

            try this.tag_types.append(this.allocator, tag_def);
            try this.composite_types.append(this.allocator, TypeDefinition{ .Tag = @intCast(u32, this.tag_types.items.len - 1) });
            
            typ = @intCast(u32, this.composite_types.items.len - 1);
        } else {
            this.allocator.free(variants);
        }

        return Type{ .Composite = typ.? };
    }
};

pub fn interpret(allocator: Allocator, filename: []const u8) !void {
    interpreter.allocator = allocator;

    const source = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(source);

    var parser = try Parser.init(allocator, filename, source);
    const nodes = parser.parse() catch |err| {
        if (err == error.ParseError) {
            try reportError(parser.err_msg);
            return;
        }
        return err;
    };

    for (nodes.items) |node| {
        std.debug.print("{}\n\n", .{node});
    }

    std.debug.print("---------------------------\n", .{});

    var typer = try Typer.init(&interpreter);
    const t_nodes = typer.typecheck(nodes) catch |err| {
        if (err == error.TypeError) {
            try reportError(typer.err_msg);
            return;
        }
        return err;
    };

    for (t_nodes.items) |node| {
        std.debug.print("{}\n\n", .{node});
    }
    
    std.debug.print("---------------------------\n", .{});

    var evaluator = try Evaluator.init(&interpreter);
    defer evaluator.deinit();

    evaluator.evaluate(t_nodes.items) catch |err| {
        if (err == error.RuntimeError) {
            try reportError(evaluator.err_msg);
            return;
        }
        return err;
    };
}

fn reportError(err_msg: ErrMsg) !void {
    var stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{err_msg});
}
