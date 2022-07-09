const std = @import("std");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    if (std.os.argv.len < 2) {
        std.debug.print("Pass file name as argument\n", .{});
        return;
    }

    const filename = std.mem.span(std.os.argv[1]);
    const cwd = std.fs.cwd();
    const programFile = try cwd.openFile(filename, .{});
    const programText = try programFile.reader().readAllAlloc(allocator, 1 << 30);
    defer allocator.free(programText);
    programFile.close();

    var statements = parser.parseFile(allocator, filename, programText) catch {
        std.debug.print("Error during parsing\n", .{});
        return;
    };
    defer statements.deinit();

    var syscallList = std.StringHashMap(compiler.FunctionDescription).init(allocator);
    defer syscallList.deinit();
    try syscallList.put("print", .{ .signature = .{ .argcount = 1 }, .location = .{ .syscallId = 0 } });
    const syscalls = [_]vm.Syscall{vm.syscallPrint};
    const memsize: usize = 1 << 10;

    var instructions = compiler.compile(allocator, statements, &syscallList, memsize) catch {
        std.log.err("Error during compilation\n", .{});
        return;
    };
    defer allocator.free(instructions);

    var stackBuffer: [memsize]u8 = undefined;
    var stack = vm.Stack.init(stackBuffer[0..]);

    vm.evaluate(instructions, &stack, syscalls[0..]);
}
