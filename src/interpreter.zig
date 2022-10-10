const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const AutoArrayHashMapUnmanaged = std.AutoArrayHashMapUnmanaged;

const parser = @import("parser.zig");
const Parser = parser.Parser;
const Tokenizer = parser.Tokenizer;

const AstBlock = @import("ast.zig").AstBlock;

const Typer = @import("typer.zig").Typer;

const Evaluator = @import("evaluator.zig").Evaluator;

const values = @import("values.zig");
const Type = values.Type;
const ListTypeDefinition = values.ListTypeDefinition;
const TupleTypeDefinition = values.TupleTypeDefinition;
const TagTypeDefinition = values.TagTypeDefinition;
const UnionTypeDefinition = values.UnionTypeDefinition;
const LambdaTypeDefinition = values.LambdaTypeDefinition;

const ErrMsg = @import("errors.zig").ErrMsg;

var interpreter = Interpreter{};

pub const LambdaDefinition = struct {
    name: []const u8,
    type_def: LambdaTypeDefinition,
    code: *AstBlock,
    closed_values: []ClosedValueIndex,
    closed_value_names: [][]const u8,

    pub const ClosedValueIndex = struct {
        index: usize,
        global: bool,
    };
};

pub const Interpreter = struct {
    allocator: Allocator = undefined,

    tags: ArrayListUnmanaged([]const u8) = .{},
    lambdas: ArrayListUnmanaged(LambdaDefinition) = .{},

    // Composite Types
    list_types: ArrayListUnmanaged(ListTypeDefinition) = .{},
    tuple_types: ArrayListUnmanaged(TupleTypeDefinition) = .{},
    tag_types: ArrayListUnmanaged(TagTypeDefinition) = .{},
    union_types: ArrayListUnmanaged(UnionTypeDefinition) = .{},
    lambda_types: ArrayListUnmanaged(LambdaTypeDefinition) = .{},

    const This = @This();

    pub fn get() *This {
        return &interpreter;
    }

    pub fn findOrAddTag(this: *This, tag: []const u8) ![]const u8 {
        if (this.findTag(tag)) |t| {
            return t;
        }

        try this.tags.append(this.allocator, tag);
        return this.tags.items[this.tags.items.len - 1];
    }

    pub fn findTag(this: *This, tag: []const u8) ?[]const u8 {
        for (this.tags.items) |t| {
            if (std.mem.eql(u8, t, tag)) {
                return t;
            }
        }
        return null;
    }

    pub fn addLambda(this: *This, lambda: LambdaDefinition) !usize {
        const lambda_index = this.lambdas.items.len;

        try this.lambdas.append(this.allocator, lambda);

        return lambda_index;
    }

    pub fn findOrAddListType(this: *This, item_type: Type) !Type {
        var typ: ?u32 = null;
        for (this.list_types.items) |list_type, list_index| {
            if (!list_type.item_type.eql(item_type)) continue;
            typ = @intCast(u32, list_index);
            break;
        }

        if (typ == null) {
            const list_def = ListTypeDefinition{ .item_type = item_type };

            try this.list_types.append(this.allocator, list_def);
            typ = @intCast(u32, this.list_types.items.len - 1);
        }

        return Type{ .List = typ.? };
    }

    pub fn findOrAddTupleType(this: *This, fields: []TupleTypeDefinition.Field) !Type {
        var typ: ?u32 = null;
        for (this.tuple_types.items) |tuple_type, tuple_index| {
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
                typ = @intCast(u32, tuple_index);
                break;
            }
        }

        if (typ == null) {
            const tuple_def = TupleTypeDefinition{ .fields = fields };

            try this.tuple_types.append(this.allocator, tuple_def);            
            typ = @intCast(u32, this.tuple_types.items.len - 1);
        } else {
            this.allocator.free(fields);
        }

        return Type{ .Tuple = typ.? };
    }

    pub fn findOrAddTagType(this: *This, variants: []TagTypeDefinition.Variant) !Type {
        var typ: ?u32 = null;
        for (this.tag_types.items) |tag_type, tag_index| {
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
                typ = @intCast(u32, tag_index);
                break;
            }
        }

        if (typ == null) {
            const tag_def = TagTypeDefinition{ .variants = variants };

            try this.tag_types.append(this.allocator, tag_def);            
            typ = @intCast(u32, this.tag_types.items.len - 1);
        } else {
            this.allocator.free(variants);
        }

        return Type{ .Tag = typ.? };
    }

    pub fn findOrAddLambdaType(this: *This, parameters: []LambdaTypeDefinition.Parameter, returns: Type) !Type {

        var found_index: ?usize = null;
        for (this.lambda_types.items) |lambda_type, lambda_index| {
            if (!lambda_type.returns.eql(returns)) continue;
            if (lambda_type.parameters.len != parameters.len) continue;

            var not_eql = false;
            for (lambda_type.parameters) |param, i| {
                if (!param.typ.eql(parameters[i].typ)) {
                    not_eql = true;
                    break;
                }
            }
            if (not_eql) continue;

            found_index = lambda_index;
            break;
        }

        if (found_index) |index| {
            this.allocator.free(parameters);
            return Type{ .Lambda = @intCast(u32, index) };
        }

        const lambda_def = LambdaTypeDefinition{ .parameters = parameters, .returns = returns };
        try this.lambda_types.append(this.allocator, lambda_def);

        return Type{ .Lambda = @intCast(u32, this.lambda_types.items.len - 1) };
    }

    pub fn unionizeTypes(this: *This, types: []const Type) !Type {
        std.debug.assert(types.len > 0);

        var flattened = AutoArrayHashMapUnmanaged(Type, void){};
        defer flattened.deinit(this.allocator);
        
        try this.flattenUnionTypes(&flattened, types);

        const flattened_types = flattened.keys();
        
        if (flattened_types.len == 1) {
            return flattened_types[0];
        }

        // check for equality and Any
        {
            var i: usize = 0;
            while (i < flattened_types.len - 1) : (i += 1) {
                const ta = flattened_types[i];
                const tb = flattened_types[i + 1];

                if (ta == .Any or tb == .Any) {
                    return .Any;
                }

                if (!ta.eql(tb)) {
                    break;
                }
            } else {
                return flattened_types[0];
            }
        }

        var found_index: ?usize = null;
        for (this.union_types.items) |union_type, union_index| {
            if (union_type.variants.len != flattened_types.len)
                continue;
            
            for (union_type.variants) |union_variant| {
                var found = false;
                for (flattened_types) |types_variant| {
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

        const variants = try this.allocator.alloc(Type, flattened_types.len);
        for (flattened_types) |typ, i| {
            variants[i] = typ;
        }

        const defn = UnionTypeDefinition{ .variants = variants };

        try this.union_types.append(this.allocator, defn);
        return Type{ .Union = @intCast(u32, this.union_types.items.len - 1) };
    }

    fn flattenUnionTypes(this: *This, flattened: *AutoArrayHashMapUnmanaged(Type, void), types: []const Type) anyerror!void {
        for (types) |typ| {
            switch (typ) {
                .Union => |union_index| {
                    const union_type = this.union_types.items[union_index];
                    try this.flattenUnionTypes(flattened, union_type.variants);
                },
                else => try flattened.put(this.allocator, typ, {}),
            }
        }
    }

    pub fn unionTypeWithVariantsRemoved(this: *This, union_type: UnionTypeDefinition, to_remove: []const Type) !Type {
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
};

pub fn interpret(allocator: Allocator, filename: []const u8) !void {
    interpreter.allocator = allocator;

    const source = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));
    defer allocator.free(source);

    var p = Parser{
        .allocator = allocator,
        .tokenizer = undefined,
        .err_msg = ErrMsg{},
    };
    p.tokenizer = try Tokenizer.init(allocator, &p.err_msg, filename, source);
    const nodes = p.parse() catch |err| {
        if (err == error.ParseError) {
            try reportError(p.err_msg);
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
