const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayListUnmanaged = std.ArrayListUnmanaged;

const val = @import("values.zig");
const Value = val.Value;
const List = val.List;
const Closure = val.Closure;
const Tuple = val.Tuple;
const Tag = val.Tag;

const DEBUG_ALWAYS_COLLECT         = false;
const DEBUG_TRACE_ALLOCATIONS      = false;
const DEBUG_TRACE_DEALLOCATIONS    = false;
const DEBUG_LOG_NUM_COLLECTIONS    = false;
var debug_num_collections: usize   = 0;
var debug_num_allocations: usize   = 0;
var debug_num_deallocations: usize = 0;

fn debugReportGcInfo() void {
    if (DEBUG_LOG_NUM_COLLECTIONS)
        std.debug.print("::: No. GC Collections: {}\n", .{debug_num_collections});
    if (DEBUG_TRACE_ALLOCATIONS)
        std.debug.print("::: No. Allocations:    {}\n", .{debug_num_allocations});
    if (DEBUG_TRACE_DEALLOCATIONS)
        std.debug.print("::: No. Deallocations:  {}\n", .{debug_num_deallocations});
}

pub const GarbageCollector = struct {
    allocator: Allocator,

    strings: ArrayListUnmanaged(GCValue([]const u8)),
    lists: ArrayListUnmanaged(GCValue(*List)),
    closures: ArrayListUnmanaged(GCValue(*Closure)),
    tuples: ArrayListUnmanaged(GCValue(*Tuple)),
    tags: ArrayListUnmanaged(GCValue(*Tag)),

    const This = @This();
    const COLLECTION_THRESHOLD: usize = 128;

    pub fn init(allocator: Allocator) This {
        return This{
            .allocator = allocator,
            .strings = ArrayListUnmanaged(GCValue([]const u8)){},
            .lists = ArrayListUnmanaged(GCValue(*List)){},
            .closures = ArrayListUnmanaged(GCValue(*Closure)){},
            .tuples = ArrayListUnmanaged(GCValue(*Tuple)){},
            .tags = ArrayListUnmanaged(GCValue(*Tag)){},
        };
    }

    pub fn deinit(this: *This) void {
        debugReportGcInfo();

        for (this.strings.items) |string| {
            this.allocator.free(string.value);
        }
        this.strings.deinit(this.allocator);

        for (this.lists.items) |list| {
            list.value.deinit(this.allocator);
            this.allocator.destroy(list.value);
        }
        this.lists.deinit(this.allocator);

        for (this.closures.items) |closure| {
            closure.value.deinit(this.allocator);
            this.allocator.destroy(closure.value);
        }
        this.closures.deinit(this.allocator);

        for (this.tuples.items) |tuple| {
            tuple.value.deinit(this.allocator);
            this.allocator.destroy(tuple.value);
        }
        this.tuples.deinit(this.allocator);

        for (this.tags.items) |tag| {
            tag.value.deinit(this.allocator);
            this.allocator.destroy(tag.value);
        }
        this.tags.deinit(this.allocator);
    }

    pub fn copyString(this: *This, s: []const u8) ![]const u8 {
        const allocated = try this.allocator.dupe(u8, s);

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING \"{s}\"\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.strings.append(this.allocator, GCValue([]const u8){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn allocateList(this: *This) !*List {
        const list = try this.allocator.create(List);

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING LIST\n", .{});
            debug_num_allocations += 1;
        }

        try this.lists.append(this.allocator, GCValue(*List){ .marked = false, .value = list });
        return list;
    }

    pub fn copyClosure(this: *This, closure: Closure) !*Closure {
        const allocated = try this.allocator.create(Closure);
        allocated.* = closure;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.closures.append(this.allocator, GCValue(*Closure){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn copyTuple(this: *This, tuple: Tuple) !*Tuple {
        const allocated = try this.allocator.create(Tuple);
        allocated.* = tuple;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.tuples.append(this.allocator, GCValue(*Tuple){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn copyTag(this: *This, tag: Tag) !*Tag {
        const allocated = try this.allocator.create(Tag);
        allocated.* = tag;

        if (DEBUG_TRACE_ALLOCATIONS) {
            std.debug.print("::: ALLOCATING {}\n", .{allocated});
            debug_num_allocations += 1;
        }

        try this.tags.append(this.allocator, GCValue(*Tag){ .marked = false, .value = allocated });
        return allocated;
    }

    pub fn collectGarbage(this: *This, variables: []Value) void {
        if (!DEBUG_ALWAYS_COLLECT and this.notEnoughGarbage()) return;

        if (DEBUG_LOG_NUM_COLLECTIONS)
            debug_num_collections += 1;

        this.markVariables(variables);
        this.freeUnmarkedValues();
    }

    fn notEnoughGarbage(this: *This) bool {
        // @INCOMPLETE:
        // This should probably use the actual amount of memory used
        // to determine if a collection is reasonable.
        //

        var num_live_allocations =
            this.strings.items.len +
            this.lists.items.len +
            this.closures.items.len +
            this.tuples.items.len +
            this.tags.items.len;

        if (DEBUG_LOG_NUM_COLLECTIONS)
            std.debug.print("::: num_live_allocations = {}\n", .{num_live_allocations});

        return num_live_allocations < COLLECTION_THRESHOLD;
    }

    fn markVariables(this: *This, variables: []Value) void {
        for (variables) |variable| {
            this.markVariable(variable);
        }
    }

    fn markVariable(this: *This, variable: Value) void {
        switch (variable) {
            .None, .Bool, .Char, .Int, .Num, .Range, .Type => {},
            .Str => |value| {
                for (this.strings.items) |*string| {
                    if (&value[0] == &string.value[0]) {
                        string.marked = true;
                        break;
                    }
                }
            },
            .List => |value| {
                for (this.lists.items) |*list| {
                    if (value == list.value) {
                        if (!list.marked)
                            this.markVariables(list.value.items);
                        list.marked = true;
                        break;
                    }
                }
            },
            .Closure => |value| {
                for (this.closures.items) |*c| {
                    if (value == c.value) {
                        if (!c.marked)
                            this.markVariables(c.value.closed_values.values());
                        c.marked = true;
                        break;
                    }
                }
            },
            .Tuple => |value| {
                this.markVariables(value.fields.values());
            },
            .Tag => |value| {
                for (this.tags.items) |*tag| {
                    if (value == tag.value) {
                        tag.marked = true;
                        break;
                    }
                }
            },
        }
    }

    fn freeUnmarkedValues(this: *This) void {
        this.freeUnmarkedStrings(&this.strings);
        this.freeUnmarkedValuesInList(*List, &this.lists);
        this.freeUnmarkedValuesInList(*Closure, &this.closures);
        this.freeUnmarkedValuesInList(*Tuple, &this.tuples);
        this.freeUnmarkedValuesInList(*Tag, &this.tags);
    }

    fn freeUnmarkedStrings(this: *This, strings: *ArrayListUnmanaged(GCValue([]const u8))) void {
        var i: usize = 0;
        while (i < strings.items.len) {
            if (!strings.items[i].marked) {
                const value = strings.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS) {
                    std.debug.print("::: FREEING \"{s}\"\n", .{value.value});
                    debug_num_deallocations += 1;
                }

                this.allocator.free(value.value);
                continue;
            } else {
                strings.items[i].marked = false; // reset for next collection
            }
            i += 1;
        }
    }

    fn freeUnmarkedValuesInList(
        this: *This,
        comptime T: type,
        values: *ArrayListUnmanaged(GCValue(T)),
    ) void {
        var i: usize = 0;
        while (i < values.items.len) {
            if (!values.items[i].marked) {
                const value = values.swapRemove(i);

                if (DEBUG_TRACE_DEALLOCATIONS) {
                    std.debug.print("::: FREEING {}\n", .{T});
                    debug_num_deallocations += 1;
                }

                value.value.deinit(this.allocator);
                this.allocator.destroy(value.value);

                continue;
            } else {
                values.items[i].marked = false; // reset for next collection
            }
            i += 1;
        }
    }
};

fn GCValue(comptime T: type) type {
    return struct {
        marked: bool,
        value: T,
    };
}
