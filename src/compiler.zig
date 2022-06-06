const std = @import("std");
const Allocator = std.mem.Allocator;

const parser = @import("parser.zig");
const Block = parser.Block;
const Statement = parser.Statement;
const Expression = parser.Expression;
const Declaration = parser.Declaration;
const vm = @import("vm.zig");

const Error = Allocator.Error || error{CompilationError};

const MemoryFrame = struct {
    variables: std.ArrayList([]const u8),

    const Self = @This();

    fn init(allocator: Allocator) Self {
        return .{
            .variables = std.ArrayList([]const u8).init(allocator),
        };
    }

    fn deinit(self: *Self) void {
        self.variables.deinit();
    }

    fn addVariable(self: *Self, varName: []const u8) !i32 {
        try self.variables.append(varName);
        return @intCast(i32, self.variables.items.len - 1) * 4;
    }

    fn variableAddr(self: *Self, varName: []const u8) ?i32 {
        var i: usize = self.variables.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, self.variables.items[i], varName)) {
                return @intCast(i32, i) * 4;
            }
        }
        return null;
    }
};

fn compileDeclaration(declaration: Declaration, instructions: *std.ArrayList(vm.Instruction), memoryFrame: *MemoryFrame, syscallList: []const []const u8) Error!void {
    var addr = try memoryFrame.addVariable(declaration.identifier);
    try compileExpression(declaration.value, instructions, memoryFrame, syscallList);
    try instructions.append(.{ .Push = addr });
    try instructions.append(.Store);
}

fn compileExpression(expression: Expression, instructions: *std.ArrayList(vm.Instruction), memoryFrame: *MemoryFrame, syscallList: []const []const u8) Error!void {
    _ = instructions;
    switch (expression) {
        .Number => |number| {
            try instructions.append(.{ .Push = number });
        },
        .Variable => |variable| {
            var addr = memoryFrame.variableAddr(variable) orelse {
                std.log.err("Variable \"{s}\" not found\n", .{variable});
                return error.CompilationError;
            };
            try instructions.append(.{ .Push = addr });
            try instructions.append(.Load);
        },
        .FunctionCall => |call| {
            var syscallNum = loop: for (syscallList) |syscallName, i| {
                if (std.mem.eql(u8, syscallName, call.name)) {
                    break :loop i;
                }
            } else {
                std.log.err("Function \"{s}\" not found\n", .{call.name});
                return error.CompilationError;
            };
            for (call.argList.arguments) |arg| {
                try compileExpression(arg, instructions, memoryFrame, syscallList);
            }
            try instructions.append(.{ .Syscall = syscallNum });
        },
        .BinaryOp => |op| {
            try compileExpression(op.operand2.*, instructions, memoryFrame, syscallList);
            if (op.operator == .Equals) {
                if (op.operand1.* != .Variable) {
                    std.log.err("Assignment is only possible to variable\n", .{});
                    return error.CompilationError;
                }
            } else {
                try compileExpression(op.operand1.*, instructions, memoryFrame, syscallList);
            }
            switch (op.operator) {
                .Plus => try instructions.append(.Add),
                .Minus => try instructions.append(.Sub),
                .Star => try instructions.append(.Mul),
                .Slash => try instructions.append(.Div),
                .Percent => try instructions.append(.Mod),
                .GreaterThan => try instructions.append(.GT),
                .LessThan => try instructions.append(.LT),
                .DoubleEquals => try instructions.append(.Eq),
                .Equals => {
                    var addr = memoryFrame.variableAddr(op.operand1.Variable) orelse {
                        std.log.err("Variable \"{s}\" not found\n", .{op.operand1.Variable});
                        return error.CompilationError;
                    };
                    try instructions.append(.{ .Push = addr });
                    try instructions.append(.Store);
                    try instructions.append(.{ .Push = 0 });
                },
                else => unreachable,
            }
        },
    }
}

fn compileStatement(statement: Statement, instructions: *std.ArrayList(vm.Instruction), memoryFrame: *MemoryFrame, syscallList: []const []const u8) !void {
    _ = instructions;
    switch (statement) {
        .Declaration => |declaration| {
            try compileDeclaration(declaration, instructions, memoryFrame, syscallList);
        },
        .Expression => |expression| {
            try compileExpression(expression, instructions, memoryFrame, syscallList);
            try instructions.append(.Pop);
        },
        .ConditionalBlock => |condBlock| {
            var beginCondAddr = instructions.items.len;
            try compileExpression(condBlock.condition, instructions, memoryFrame, syscallList);
            var jumpCondAddr = instructions.items.len;
            try instructions.append(.{ .JumpEqZero = 0 });
            try compileBlock(condBlock.body, instructions, memoryFrame, syscallList);
            if (condBlock.isLoop) {
                try instructions.append(.{ .Jump = beginCondAddr });
            }
            instructions.items[jumpCondAddr].JumpEqZero = instructions.items.len;
        },
    }
}

fn compileBlock(block: Block, instructions: *std.ArrayList(vm.Instruction), memoryFrame: *MemoryFrame, syscallList: []const []const u8) Error!void {
    for (block.statements) |statement| {
        try compileStatement(statement, instructions, memoryFrame, syscallList);
    }
}

pub fn compile(allocator: Allocator, block: Block, syscallList: []const []const u8) ![]vm.Instruction {
    var instructions = std.ArrayList(vm.Instruction).init(allocator);
    defer instructions.deinit();
    var memoryFrame = MemoryFrame.init(allocator);
    defer memoryFrame.deinit();

    try compileBlock(block, &instructions, &memoryFrame, syscallList);

    return instructions.toOwnedSlice();
}

var testError: bool = undefined;
fn syscallExpect69(stack: *vm.Stack, memory: *vm.Memory) void {
    _ = memory;
    var value = stack.popInt();
    if (value != 69) {
        testError = true;
    }
    stack.pushInt(0);
}

test "compilation" {
    const expectEqual = std.testing.expectEqual;

    const allocator = std.testing.allocator;

    var program = "var a = 17 \n var b = 2 \n expect69(a * b + 35)";

    var block = try parser.parseFile(allocator, "testfile", program);
    defer block.deinit();

    const syscallList = [_][]const u8{"expect69"};
    var instructions = try compile(allocator, block, syscallList[0..]);
    defer allocator.free(instructions);

    var stackBuffer: [1 << 10]u8 = undefined;
    var stackMemory = vm.Memory.init(stackBuffer[0..]);
    var stack = vm.Stack.init(stackMemory);
    var memoryBuffer: [1 << 10]u8 = undefined;
    var memory = vm.Memory.init(memoryBuffer[0..]);

    const syscalls = [_]vm.Syscall{syscallExpect69};

    testError = false;
    vm.evaluate(instructions, &stack, &memory, syscalls[0..]);
    try expectEqual(@as(usize, 0), stack.sp);
    try expectEqual(false, testError);
}
