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
        std.debug.print("Error: No file given to compile.\n", .{});
        return;
    }
    defer allocator.free(filename);
    
    try interpreter.interpret(allocator, filename);
}
