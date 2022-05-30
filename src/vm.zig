const std = @import("std");

const Instruction = union(enum) {
    Push: i32,
    Add,
    Sub,
    Store,
    Load,
    Jump: usize,
    JumpGtZero: usize,
    JumpEqZero: usize,
    Syscall: usize,
};

const Memory = struct {
    buffer: []u8,

    fn init(buffer: []u8) Memory {
        return Memory{ .buffer = buffer };
    }

    fn storeInt(self: *Memory, address: usize, value: i32) void {
        var v = value;
        std.mem.copy(u8, self.buffer[address .. address + 4], @ptrCast(*[4]u8, &v));
    }

    fn loadInt(self: *Memory, address: usize) i32 {
        var value: i32 = undefined;
        std.mem.copy(u8, @ptrCast(*[4]u8, &value), self.buffer[address .. address + 4]);
        return value;
    }
};

const Stack = struct {
    memory: Memory,
    sp: usize,

    fn init(memory: Memory) Stack {
        return Stack{
            .memory = memory,
            .sp = 0,
        };
    }

    fn pushInt(self: *Stack, value: i32) void {
        self.memory.storeInt(self.sp, value);
        self.sp += 4;
    }

    fn popInt(self: *Stack) i32 {
        self.sp -= 4;
        return self.memory.loadInt(self.sp);
    }
};

const Syscall = fn (stack: *Stack, memory: *Memory) void;

fn syscallPrint(stack: *Stack, memory: *Memory) void {
    _ = memory;
    var value = stack.popInt();
    std.debug.print("{}\n", .{value});
}

fn evaluate(program: []const Instruction, stack: *Stack, memory: *Memory, syscalls: []const Syscall) void {
    var ip: usize = 0;

    while (ip < program.len) {
        switch (program[ip]) {
            .Push => |value| {
                stack.pushInt(value);
                ip += 1;
            },
            .Add => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(v1 + v2);
                ip += 1;
            },
            .Sub => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(v2 - v1);
                ip += 1;
            },
            .Store => {
                var addr = stack.popInt();
                var value = stack.popInt();
                memory.storeInt(@intCast(usize, addr), value);
                ip += 1;
            },
            .Load => {
                var addr = stack.popInt();
                var value = memory.loadInt(@intCast(usize, addr));
                stack.pushInt(value);
                ip += 1;
            },
            .Jump => |jmpIp| {
                ip = jmpIp;
            },
            .JumpGtZero => |jmpIp| {
                var v = stack.popInt();
                if (v > 0) {
                    ip = jmpIp;
                } else {
                    ip += 1;
                }
            },
            .JumpEqZero => |jmpIp| {
                var v = stack.popInt();
                if (v == 0) {
                    ip = jmpIp;
                } else {
                    ip += 1;
                }
            },
            .Syscall => |syscall| {
                syscalls[syscall](stack, memory);
                ip += 1;
            },
        }
    }
}

test "program evaluation" {
    const expectEqual = std.testing.expectEqual;

    // stores counter at address 0, sum at address 4
    const program = [_]Instruction{
        .{ .Push = 10 },
        .{ .Push = 0 },
        .Store,
        .{ .Push = 0 },
        .{ .Push = 4 },
        .Store,

        .{ .Push = 4 },
        .Load,
        .{ .Push = 0 },
        .Load,
        .Add,
        .{ .Push = 4 },
        .Store,

        .{ .Push = 0 },
        .Load,
        .{ .JumpEqZero = 23 }, // jump after .{ .Jump = 6 },

        .{ .Push = 0 },
        .Load,
        .{ .Push = 1 },
        .Sub,
        .{ .Push = 0 },
        .Store,

        .{ .Jump = 6 },
    };

    var stackBuffer: [1 << 10]u8 = undefined;
    var stackMemory = Memory.init(stackBuffer[0..]);
    var stack = Stack.init(stackMemory);
    var memoryBuffer: [1 << 10]u8 = undefined;
    var memory = Memory.init(memoryBuffer[0..]);

    const syscalls = [_]Syscall{};

    evaluate(program[0..], &stack, &memory, syscalls[0..]);
    try expectEqual(@as(usize, 0), stack.sp);
    try expectEqual(@as(i32, 0), memory.loadInt(0));
    try expectEqual(@as(i32, 55), memory.loadInt(4));
}
