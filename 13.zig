const std = @import("std");

pub fn main() void {
    const content = @embedFile("./input/13.txt");
    std.debug.print("{s}", .{content});
}
