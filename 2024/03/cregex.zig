const Allocator = @import("std").mem.Allocator;
const c = @cImport({
    @cInclude("regex.h");
});

pub fn ismatch(regex: [:0]const u8, str: [:0]const u8) !bool {
    var preg: c.regex_t = undefined;
    var errcode = c.regcomp(&preg, regex, c.REG_EXTENDED);
    if (!try Cregex.errcodeToError(errcode)) unreachable;
    var pmatch: [1]c.regmatch_t = undefined;
    errcode = c.regexec(&preg, str, 1, &pmatch, 0);
    return try Cregex.errcodeToError(errcode);
}

pub const Cregex = struct {
    preg: c.regex_t,
    pmatch: []c.regmatch_t,
    matches: []?Match,

    pub const Match = struct {
        pos: usize,
        val: []const u8,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, regex: [:0]const u8) !Self {
        var preg: c.regex_t = undefined;
        const errcode = c.regcomp(&preg, regex, c.REG_EXTENDED);
        if (!try errcodeToError(errcode)) unreachable;
        // +1 for the entire capture resut (the one at [0] index)
        const matches_count: usize = @intCast(preg.re_nsub+1);
        const pmatch = try allocator.alloc(c.regmatch_t, matches_count);
        const matches = try allocator.alloc(?Match, matches_count);
        return .{
            .preg = preg,
            .pmatch = pmatch,
            .matches = matches,
        };
    }

    pub fn match(self: *Self, str: [:0]const u8) !?[]?Match {
        const errcode = c.regexec(&self.preg, str, self.pmatch.len, @ptrCast(self.pmatch), 0);
        if (!try errcodeToError(errcode)) return null;
        for (0..self.pmatch.len) |i| {
            if (self.pmatch[i].rm_so == -1) {
                self.matches[i] = null;
            } else {
                const so: usize = @intCast(self.pmatch[i].rm_so);
                const eo: usize = @intCast(self.pmatch[i].rm_eo);
                self.matches[i] = Match {
                    .pos = so,
                    .val = str[so..eo],
                };
            }
        }
        return self.matches;
    }

    pub fn errcodeToError(errcode: c_int) !bool {
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
