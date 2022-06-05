const std = @import("std");
const parser = @import("parser.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const filename = "example.prog";
    const cwd = std.fs.cwd();
    const programFile = try cwd.openFile(filename, .{});
    const programText = try programFile.reader().readAllAlloc(allocator, 1 << 30);
    programFile.close();

    var ast = (try parser.Ast.parse(allocator, filename, programText)) orelse {
        std.debug.print("Ast.parse did return null\n", .{});
        return;
    };
    defer ast.deinit();

    const syscallList = [_][]const u8{"print"};
    const syscalls = [_]vm.Syscall{vm.syscallPrint};

    var instructions = compiler.compile(allocator, ast, syscallList[0..]) catch {
        std.log.err("Error during compilation\n", .{});
        return;
    };
    defer allocator.free(instructions);

    var stackBuffer: [1 << 10]u8 = undefined;
    var stackMemory = vm.Memory.init(stackBuffer[0..]);
    var stack = vm.Stack.init(stackMemory);
    var memoryBuffer: [1 << 10]u8 = undefined;
    var memory = vm.Memory.init(memoryBuffer[0..]);

    vm.evaluate(instructions, &stack, &memory, syscalls[0..]);
}
