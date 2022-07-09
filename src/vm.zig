const std = @import("std");

pub const Instruction = union(enum) {
    Push: i32,
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LT,
    GT,
    Eq,
    Store,
    Load,
    StoreLocal,
    LoadLocal,
    PushSP,
    Res: usize,
    Free: usize,
    Jump: usize,
    JumpGtZero: usize,
    JumpEqZero: usize,
    Syscall: usize,
    Call: usize,
    FnInit,
    Ret,
};

pub const Stack = struct {
    buffer: []u8,
    sp: usize,
    bp: usize,

    const Self = @This();

    pub fn init(buffer: []u8) Stack {
        return Stack{
            .buffer = buffer,
            .sp = buffer.len,
            .bp = 0,
        };
    }

    pub fn storeInt(self: *Self, address: usize, value: i32) void {
        var v = value;
        std.mem.copy(u8, self.buffer[address .. address + 4], @ptrCast(*[4]u8, &v));
    }

    pub fn storeIntLocal(self: *Self, offset: i32, value: i32) void {
        self.storeInt(@intCast(usize, @intCast(i32, self.bp) + offset), value);
    }

    pub fn loadInt(self: *const Self, address: usize) i32 {
        var value: i32 = undefined;
        std.mem.copy(u8, @ptrCast(*[4]u8, &value), self.buffer[address .. address + 4]);
        return value;
    }

    pub fn loadIntLocal(self: *const Self, offset: i32) i32 {
        return self.loadInt(@intCast(usize, @intCast(i32, self.bp) + offset));
    }

    pub fn pushInt(self: *Self, value: i32) void {
        self.sp -= 4;
        self.storeInt(self.sp, value);
    }

    pub fn popInt(self: *Self) i32 {
        var value = self.loadInt(self.sp);
        self.sp += 4;
        return value;
    }

    fn pushBase(self: *Self) void {
        self.pushInt(@intCast(i32, self.bp));
        self.bp = self.sp;
    }

    fn popBase(self: *Self) void {
        self.sp = self.bp;
        self.bp = @intCast(usize, self.popInt());
    }

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        try std.fmt.format(writer, "[ ", .{});
        var i: usize = self.buffer.len;
        while (i > self.sp) {
            i -= 4;
            try std.fmt.format(writer, "{}, ", .{self.loadInt(i)});
        }
        try std.fmt.format(writer, "]\n", .{});
    }
};

pub const Syscall = fn (stack: *Stack) void;

pub fn syscallPrint(stack: *Stack) void {
    var value = stack.loadIntLocal(4);
    std.debug.print("{}\n", .{value});
    stack.storeInt(@intCast(usize, stack.loadIntLocal(8)), 0);
}

pub fn evaluate(program: []const Instruction, stack: *Stack, syscalls: []const Syscall) void {
    var ip: usize = 0;
    var cnt: usize = 0;

    while (ip < program.len) {
        cnt += 1;
        switch (program[ip]) {
            .Push => |value| {
                stack.pushInt(value);
                ip += 1;
            },
            .Pop => {
                _ = stack.popInt();
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
                stack.pushInt(v1 - v2);
                ip += 1;
            },
            .Mul => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(v1 * v2);
                ip += 1;
            },
            .Div => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(@divFloor(v1, v2));
                ip += 1;
            },
            .Mod => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(@mod(v1, v2));
                ip += 1;
            },
            .LT => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(if (v1 < v2) 1 else 0);
                ip += 1;
            },
            .GT => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(if (v1 > v2) 1 else 0);
                ip += 1;
            },
            .Eq => {
                var v1 = stack.popInt();
                var v2 = stack.popInt();
                stack.pushInt(if (v1 == v2) 1 else 0);
                ip += 1;
            },
            .Store => {
                var addr = stack.popInt();
                var value = stack.popInt();
                stack.storeInt(@intCast(usize, addr), value);
                ip += 1;
            },
            .Load => {
                var addr = stack.popInt();
                var value = stack.loadInt(@intCast(usize, addr));
                stack.pushInt(value);
                ip += 1;
            },
            .StoreLocal => {
                var offset = stack.popInt();
                var value = stack.popInt();
                stack.storeIntLocal(offset, value);
                ip += 1;
            },
            .LoadLocal => {
                var offset = stack.popInt();
                var value = stack.loadIntLocal(offset);
                stack.pushInt(value);
                ip += 1;
            },
            .PushSP => {
                stack.pushInt(@intCast(i32, stack.sp));
                ip += 1;
            },
            .Res => |amount| {
                stack.sp -= amount;
                ip += 1;
            },
            .Free => |amount| {
                stack.sp += amount;
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
                stack.pushBase();
                syscalls[syscall](stack);
                stack.popBase();
                ip += 1;
            },
            .Call => |call| {
                stack.pushInt(@intCast(i32, ip) + 1);
                stack.pushBase();
                ip = call;
            },
            .FnInit => {
                // TODO: remove FnInit
                ip += 1;
            },
            .Ret => {
                stack.popBase();
                ip = @intCast(usize, stack.popInt());
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

        .{ .Push = 1 },
        .{ .Push = 0 },
        .Load,
        .Sub,
        .{ .Push = 0 },
        .Store,

        .{ .Jump = 6 },
    };

    const memsize: usize = 1 << 10;
    var stackBuffer: [memsize]u8 = undefined;
    var stack = Stack.init(stackBuffer[0..]);

    const syscalls = [_]Syscall{};

    evaluate(program[0..], &stack, syscalls[0..]);
    try expectEqual(memsize, stack.sp);
    try expectEqual(@as(i32, 0), stack.loadInt(0));
    try expectEqual(@as(i32, 55), stack.loadInt(4));
}
