const std = @import("std");
const CodeLocation = @import("parser.zig").CodeLocation;

pub const ErrMsg = struct {
    loc: ?CodeLocation = null,
    msg: []const u8 = "",
    err: anyerror = error.None,

    const This = @This();

    pub fn format(this: *const This, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        const err_type = if (this.err == error.InternalError) "Internal Error" else "Error";
        if (this.loc) |loc| {
            try writer.print("{}: {s}: {s}", .{ loc, err_type, this.msg });
        } else {
            try writer.print("{s}: {s}", .{ err_type, this.msg });
        }
    }
};

pub fn raise(
    err: anytype,
    out: *ErrMsg,
    loc: ?CodeLocation,
    comptime fmt: []const u8,
    args: anytype
) (@TypeOf(err) || std.fmt.AllocPrintError) {
    var allocator = std.heap.c_allocator;
    const msg = std.fmt.allocPrint(allocator, fmt, args) catch |print_error| {
        out.* = ErrMsg{ .msg = "Failed to print error message in `raise()`.", .err = print_error };
        return print_error;
    };

    out.* = ErrMsg{ .loc = loc, .msg = msg, .err = err };
    return err;
}

pub fn todo(msg: []const u8) noreturn {
    std.debug.print("Todo: {s}\n", .{msg});
    unreachable;
}
