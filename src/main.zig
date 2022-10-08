// @TODO:
// - Check out Ada for language design inspiration
// - Add ranged integer types like this:
//      `type Byte = 0..256`

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
