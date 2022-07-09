const std = @import("std");
const Allocator = std.mem.Allocator;

const parser = @import("parser.zig");
const Block = parser.Block;
const Statement = parser.Statement;
const Expression = parser.Expression;
const Declaration = parser.Declaration;
const vm = @import("vm.zig");

const Error = Allocator.Error || error{CompilationError};

const CompilerContext = struct {
    // stores current function, syscall table, scope and namespace
    allocator: Allocator,
    syscalls: *std.StringHashMap(FunctionDescription),
    globals: ?Scope,
    functions: std.StringHashMap(FunctionDescription),
    locals: ?Scope,
    currentFunction: ?FunctionSignature,

    const Self = @This();

    fn init(allocator: Allocator, syscalls: *std.StringHashMap(FunctionDescription), memsize: usize) Self {
        var self = Self{
            .allocator = allocator,
            .syscalls = syscalls,
            .globals = Scope.init(allocator, null),
            .functions = std.StringHashMap(FunctionDescription).init(allocator),
            .locals = null,
            .currentFunction = null,
        };
        self.globals.?.offset = @intCast(i32, memsize);
        return self;
    }

    fn deinit(self: *Self) void {
        if (self.globals) |*scope| scope.deinit();
        self.functions.deinit();
        if (self.locals) |*scope| scope.deinit();
    }

    fn findFunction(self: *Self, name: []const u8) ?FunctionDescription {
        if (self.syscalls.get(name)) |syscall| return syscall;
        if (self.functions.get(name)) |function| return function;
        return null;
    }

    fn findVariable(self: *Self, name: []const u8) ?VariableLocation {
        if (self.locals) |*scope| {
            if (scope.variableAddr(name)) |addr| {
                return VariableLocation{ .local = addr };
            }
        }
        if (self.globals) |*scope| {
            if (scope.variableAddr(name)) |addr| {
                return VariableLocation{ .global = addr };
            }
        }
        return null;
    }

    fn beginScope(self: *Self) !*Scope {
        if (self.currentFunction == null) {
            var p = try self.allocator.create(Scope);
            p.* = self.globals.?;
            self.globals = Scope.init(self.allocator, p);
            return &self.globals.?;
        }

        var parent: ?*Scope = undefined;
        if (self.locals) |scope| {
            var p = try self.allocator.create(Scope);
            p.* = scope;
            parent = p;
        } else {
            parent = null;
        }
        self.locals = Scope.init(self.allocator, parent);
        return &self.locals.?;
    }

    fn endScope(self: *Self) !usize {
        var scope = if (self.locals != null) &self.locals else &self.globals;
        if (scope.* == null) {
            @panic("trying to end scope that is null");
        }
        var offset: i32 = scope.*.?.offset;
        if (scope.*.?.parent) |parent| {
            scope.*.?.deinit();
            scope.* = parent.*;
            self.allocator.destroy(parent);
            return @intCast(usize, scope.*.?.offset - offset);
        } else {
            scope.*.?.deinit();
            scope.* = null;
            return @intCast(usize, -offset);
        }
    }
};

const FunctionSignature = struct {
    // currently all functions return exactly one i32 and accept a number of i32 as arguments
    argcount: usize,
};

pub const FunctionDescription = struct {
    signature: FunctionSignature,
    location: union(enum) {
        syscallId: usize,
        insPointer: usize,
    },
};

const VariableLocation = union(enum) {
    global: i32,
    local: i32,
};

const Scope = struct {
    // stores all the local variables for the function
    // How should local scopes be handled? Biggest size of required local memory must be stored.
    // Or is it only allocated on the stack as soon as the scope is entered (eg. a while loop)?
    parent: ?*Scope,
    variables: std.StringHashMap(i32),
    offset: i32,

    const Self = @This();

    fn init(allocator: Allocator, parent: ?*Scope) Self {
        return .{
            .parent = parent,
            .variables = std.StringHashMap(i32).init(allocator),
            .offset = if (parent) |p| p.offset else 0,
        };
    }

    fn deinit(self: *Self) void {
        self.variables.deinit();
    }

    fn addVariable(self: *Self, varName: []const u8) !i32 {
        self.offset -= 4;
        try self.variables.put(varName, self.offset);
        return self.offset;
    }

    fn addVariableWithOffset(self: *Self, varName: []const u8, offset: i32) !void {
        try self.variables.put(varName, offset);
    }

    fn addArguments(self: *Self, arguments: []Expression) !void {
        for (arguments) |arg, index| {
            if (arg != .Variable) {
                std.log.err("Argument list in function definition must consist only of identifiers\n", .{});
                return error.CompilationError;
            }
            try self.addVariableWithOffset(arg.Variable, @intCast(i32, (arguments.len - index + 1) * 4));
        }
    }

    fn variableAddr(self: *Self, varName: []const u8) ?i32 {
        if (self.variables.get(varName)) |addr| {
            return addr;
        }
        if (self.parent) |parent| {
            if (parent.variableAddr(varName)) |addr| {
                return addr;
            }
        }
        return null;
    }
};

fn compileDeclaration(declaration: Declaration, instructions: *std.ArrayList(vm.Instruction), context: *CompilerContext) Error!void {
    var addr: i32 = undefined;
    var local: bool = undefined;
    if (context.locals) |*scope| {
        addr = try scope.addVariable(declaration.identifier);
        local = true;
    } else if (context.globals) |*scope| {
        addr = try scope.addVariable(declaration.identifier);
        local = false;
    } else {
        @panic("Compiling a declaration needs a scope");
    }
    try compileExpression(declaration.value, instructions, context);
    try instructions.append(.{ .Push = addr });
    if (local) {
        try instructions.append(.StoreLocal);
    } else {
        try instructions.append(.Store);
    }
}

fn compileExpression(expression: Expression, instructions: *std.ArrayList(vm.Instruction), context: *CompilerContext) Error!void {
    _ = instructions;
    switch (expression) {
        .Number => |number| {
            try instructions.append(.{ .Push = number });
        },
        .Variable => |variable| {
            var varLoc = context.findVariable(variable) orelse {
                std.log.err("Variable \"{s}\" not found\n", .{variable});
                return error.CompilationError;
            };
            switch (varLoc) {
                .global => |addr| {
                    try instructions.append(.{ .Push = addr });
                    try instructions.append(.Load);
                },
                .local => |addr| {
                    try instructions.append(.{ .Push = addr });
                    try instructions.append(.LoadLocal);
                },
            }
        },
        .FunctionCall => |call| {
            var function = if (context.findFunction(call.name)) |function| function else {
                std.log.err("Function \"{s}\" not found\n", .{call.name});
                return error.CompilationError;
            };
            // TODO: verify arguments
            try instructions.append(.{ .Res = 4 });
            try instructions.append(.PushSP);
            for (call.argList.arguments) |arg| {
                try compileExpression(arg, instructions, context);
            }
            switch (function.location) {
                .syscallId => |syscallId| {
                    try instructions.append(.{ .Syscall = syscallId });
                },
                .insPointer => |insPointer| {
                    try instructions.append(.{ .Call = insPointer });
                },
            }
            for (call.argList.arguments) |_| {
                try instructions.append(.Pop);
            }
            try instructions.append(.Pop);
        },
        .BinaryOp => |op| {
            try compileExpression(op.operand2.*, instructions, context);
            if (op.operator == .Equals) {
                if (op.operand1.* != .Variable) {
                    std.log.err("Assignment is only possible to variable\n", .{});
                    return error.CompilationError;
                }
            } else {
                try compileExpression(op.operand1.*, instructions, context);
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
                    var varLoc = context.findVariable(op.operand1.Variable) orelse {
                        std.log.err("Variable \"{s}\" not found\n", .{op.operand1.Variable});
                        return error.CompilationError;
                    };
                    switch (varLoc) {
                        .global => |addr| {
                            try instructions.append(.{ .Push = addr });
                            try instructions.append(.Store);
                        },
                        .local => |addr| {
                            try instructions.append(.{ .Push = addr });
                            try instructions.append(.StoreLocal);
                        },
                    }
                    try instructions.append(.{ .Push = 0 });
                },
                else => unreachable,
            }
        },
    }
}

fn compileStatement(statement: Statement, instructions: *std.ArrayList(vm.Instruction), context: *CompilerContext) !void {
    _ = instructions;
    switch (statement) {
        .Declaration => |declaration| {
            try compileDeclaration(declaration, instructions, context);
        },
        .Expression => |expression| {
            try compileExpression(expression, instructions, context);
            try instructions.append(.Pop);
        },
        .ConditionalBlock => |condBlock| {
            // TODO: create new scope
            var beginCondAddr = instructions.items.len;
            try compileExpression(condBlock.condition, instructions, context);
            var jumpCondAddr = instructions.items.len;
            try instructions.append(.{ .JumpEqZero = 0 });
            try compileBlock(condBlock.body, instructions, context);
            if (condBlock.isLoop) {
                try instructions.append(.{ .Jump = beginCondAddr });
            }
            instructions.items[jumpCondAddr].JumpEqZero = instructions.items.len;
        },
        .FunctionDefinition => |fun| {
            var skipJump = instructions.items.len;
            try instructions.append(.{ .Jump = 0 });
            // TODO make sure currentFunction is null
            context.currentFunction = .{ .argcount = fun.arguments.arguments.len };
            var scope = try context.beginScope();
            try scope.addArguments(fun.arguments.arguments);
            try instructions.append(.FnInit);
            try compileBlock(fun.body, instructions, context);
            try context.functions.put(fun.name, .{
                .signature = .{ .argcount = fun.arguments.arguments.len },
                .location = .{ .insPointer = skipJump + 1 },
            });
            if ((try context.endScope()) != 0) {
                @panic("Function scope should be empty and not reserve memory\n");
            }
            try instructions.append(.Ret);
            instructions.items[skipJump].Jump = instructions.items.len;
            context.currentFunction = null;
        },
        .ReturnStatement => |ret| {
            try compileExpression(ret.value, instructions, context);
            var args = if (context.currentFunction) |fun| fun.argcount else {
                std.log.err("Return statement is not allowed outside functions\n", .{});
                return error.CompilationError;
            };
            try instructions.append(.{ .Push = @intCast(i32, 2 + args) * 4 });
            try instructions.append(.LoadLocal);
            try instructions.append(.Store);
        },
    }
}

fn compileBlock(block: Block, instructions: *std.ArrayList(vm.Instruction), context: *CompilerContext) Error!void {
    _ = try context.beginScope();
    var resInstruction = instructions.items.len;
    try instructions.append(.{ .Res = 0 });
    for (block.statements) |statement| {
        try compileStatement(statement, instructions, context);
    }
    var localSize = try context.endScope();
    instructions.items[resInstruction].Res = localSize;
    try instructions.append(.{ .Free = localSize });
}

pub fn compile(allocator: Allocator, block: Block, syscallList: *std.StringHashMap(FunctionDescription), memsize: usize) ![]vm.Instruction {
    var instructions = std.ArrayList(vm.Instruction).init(allocator);
    defer instructions.deinit();

    var context = CompilerContext.init(allocator, syscallList, memsize);
    defer context.deinit();

    try compileBlock(block, &instructions, &context);

    return instructions.toOwnedSlice();
}

var testError: bool = undefined;
fn syscallExpect69(stack: *vm.Stack) void {
    var value = stack.loadIntLocal(4);
    if (value != 69) {
        testError = true;
    }
}

test "compilation" {
    const expectEqual = std.testing.expectEqual;

    const allocator = std.testing.allocator;

    var program = "var a = 17 \n var b = 2 \n expect69(a * b + 35)";

    var block = try parser.parseFile(allocator, "testfile", program);
    defer block.deinit();

    var syscallList = std.StringHashMap(FunctionDescription).init(allocator);
    defer syscallList.deinit();
    const memsize: usize = 1 << 10;
    try syscallList.put("expect69", .{ .signature = .{ .argcount = 1 }, .location = .{ .syscallId = 0 } });
    var instructions = try compile(allocator, block, &syscallList, memsize);
    defer allocator.free(instructions);

    var stackBuffer: [memsize]u8 = undefined;
    var stack = vm.Stack.init(stackBuffer[0..]);

    const syscalls = [_]vm.Syscall{syscallExpect69};

    testError = false;
    vm.evaluate(instructions, &stack, syscalls[0..]);
    try expectEqual(memsize, stack.sp);
    try expectEqual(false, testError);
}
