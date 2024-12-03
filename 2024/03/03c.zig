const std = @import("std");
const dprint = std.debug.print;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.print("{d}\n", .{try solve(stdin)});
}

fn Cregex(comptime captures_count: usize) type {
    return struct {
        preg: c.regex_t,

        const c = @cImport({
            @cInclude("regex.h");
        });

        const Self = @This();

        pub fn init(regex: [:0]const u8) !Self {
            var preg: c.regex_t = undefined;
            const errcode = c.regcomp(&preg, regex, c.REG_EXTENDED);
            if (!try errcodeToError(errcode)) unreachable;
            return .{
                .preg = preg,
            };
        }

        pub const Match = struct {
            pos: usize,
            val: []const u8,
        };

        pub fn match(self: *Self, str: [:0]const u8) !?[captures_count]?Match {
            var pmatch: [captures_count]c.regmatch_t = undefined;
            const errcode = c.regexec(&self.preg, str, pmatch.len, &pmatch, 0);
            if (!try errcodeToError(errcode)) return null;
            var matches: [captures_count]?Match = undefined;
            for (0..captures_count) |i| {
                if (pmatch[i].rm_so == -1) {
                    matches[i] = null;
                } else {
                    const so: usize = @intCast(pmatch[i].rm_so);
                    const eo: usize = @intCast(pmatch[i].rm_eo);
                    matches[i] = Match {
                        .pos = so,
                        .val = str[so..eo],
                    };
                }
            }
            return matches;
        }

        fn errcodeToError(errcode: c_int) !bool {
            if (errcode == 0) return true;
            switch (errcode) {
                c.REG_NOMATCH  => return false,             // "The regexec() function failed to match"
                c.REG_BADPAT   => return error.RegBadpat,   // "invalid regular expression"
                c.REG_ECOLLATE => return error.RegEcollate, // "invalid collating element"
                c.REG_ECTYPE   => return error.RegEctype,   // "invalid character class"
                c.REG_EESCAPE  => return error.RegEescape,  // "‘\\’ applied to unescapable character"
                c.REG_ESUBREG  => return error.RegEsubreg,  // "invalid backreference number"
                c.REG_EBRACK   => return error.RegEbrack,   // "brackets ‘[ ]’ not balanced"
                c.REG_EPAREN   => return error.RegEparen,   // "parentheses ‘( )’ not balanced"
                c.REG_EBRACE   => return error.RegEbrace,   // "braces ‘{ }’ not balanced"
                c.REG_BADBR    => return error.RegBadbr,    // "invalid repetition count(s) in ‘{ }’"
                c.REG_ERANGE   => return error.RegErange,   // "invalid character range in ‘[ ]’"
                c.REG_ESPACE   => return error.RegEspace,   // "ran out of memory"
                c.REG_BADRPT   => return error.RegBadrpt,   // "‘?’, ‘*’, or ‘+’ operand invalid"
                c.REG_EMPTY    => return error.RegEmpty,    // "empty (sub)expression"
                c.REG_ASSERT   => return error.RegAssert,   // "cannot happen - you found a bug"
                c.REG_INVARG   => return error.RegInvarg,   // "invalid argument, e.g. negative-length string"
                c.REG_ILLSEQ   => return error.RegIllseq,   // "illegal byte sequence (bad multibyte character)"
                else           => return error.RegUnknown,
            }
        }

    };
}

fn solve(reader: anytype) !u128 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const allocator = arena.allocator();
    var input_al = try std.ArrayList(u8 ).initCapacity(allocator, 1024*16);
    try reader.readAllArrayList(&input_al, std.math.maxInt(usize));
    try input_al.append(0);
    var input = input_al.items[0..input_al.items.len-1:0];
    var cregex = try Cregex(4).init("(don't\\(\\)|do\\(\\)|mul\\(([0-9]+),([0-9]+)\\))");
    var res: u128 = 0;
    var enable = true;
    while (try cregex.match(input)) |matches| {
        const k = matches[0].?.val[2];
        switch(k) {
            '(' => enable = true,  // do()
            'n' => enable = false, // don't()
            'l' => { // mul()
                if (enable) {
                    const l = try std.fmt.parseInt(u64, matches[2].?.val, 10);
                    const r = try std.fmt.parseInt(u64, matches[3].?.val, 10);
                    res += l*r;
                }
            },
            else => {
                dprint("Unknown char '{c}' in match: '{s}'\n", .{k, matches[0].?.val});
                return error.UnknownChar;
            }
        }
        input = input[matches[0].?.pos+matches[0].?.val.len..:0];
    }
    return res;
}

fn test_solve(expected: u128, input_file_path: []const u8) !void {
    const file = try std.fs.cwd().openFile(input_file_path, .{});
    defer file.close();
    try std.testing.expectEqual(expected, try solve(file.reader()));
}
test "03b example.b.txt" { try test_solve(48,       "./03/example.b.txt"); }
test "03b input.txt"     { try test_solve(75920122, "./03/input.txt"); }

