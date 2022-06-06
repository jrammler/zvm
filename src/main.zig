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

    const syscallList = [_][]const u8{"print"};
    const syscalls = [_]vm.Syscall{vm.syscallPrint};

    var instructions = compiler.compile(allocator, statements, syscallList[0..]) catch {
        std.log.err("Error during compilation\n", .{});
        return;
    };
    defer allocator.free(instructions);

    std.debug.print("Number of instructions: {}\n\n", .{instructions.len});

    var stackBuffer: [1 << 10]u8 = undefined;
    var stackMemory = vm.Memory.init(stackBuffer[0..]);
    var stack = vm.Stack.init(stackMemory);
    var memoryBuffer: [1 << 10]u8 = undefined;
    var memory = vm.Memory.init(memoryBuffer[0..]);

    vm.evaluate(instructions, &stack, &memory, syscalls[0..]);
}
