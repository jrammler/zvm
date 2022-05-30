const std = @import("std");
const parser = @import("parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const cwd = std.fs.cwd();
    const programFile = try cwd.openFile("example.prog", .{});
    const programText = try programFile.reader().readAllAlloc(allocator, 1 << 30);
    programFile.close();

    var ast = try parser.Ast.parse(allocator, programText);
    if (ast == null) {
        std.debug.print("Ast.parse did return null\n", .{});
        return;
    }
    std.debug.print("{s}\n", .{ast});
}
