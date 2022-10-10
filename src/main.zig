// @TODO:
// - Check out Ada for language design inspiration
// - Add ranged integer types like this:
//      `type Byte = 0..256`
// - Fix Bug with GC that crashes when a value being returned a block gets GC'd
//   before it gets assigned. (Could just put last value on the stack in the
//   block onto the end of the stack of the parent to prevent it from begin
//   GC'd.)
//

const std = @import("std");
const interpreter = @import("interpreter.zig");


pub fn main() anyerror!void {
    const Gpa = std.heap.GeneralPurposeAllocator(.{});
    var gpa = Gpa{};
    var allocator = gpa.allocator();

    var args = std.process.args();
    allocator.free(try (args.next(allocator).?));
    var filename: [:0]const u8 = undefined;
    if (args.next(allocator)) |next| {
        filename = try next;
    } else {
        var stdout = std.io.getStdOut().writer();
        _ = try stdout.write("Error: No file given to compile.\n");
        return;
    }
    defer allocator.free(filename);
    
    try interpreter.interpret(allocator, filename);
}
