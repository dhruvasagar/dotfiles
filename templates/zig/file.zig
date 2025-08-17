const std = @import("std");
const time = std.time;
const print = std.debug.print;

pub fn main() !void {
    const s = time.microTimestamp();
    const stdin = std.io.getStdIn().reader();
    while (true) {
        var buf: [100]u8 = undefined;
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');
        if (line == null) break;
    }
    const e = time.microTimestamp();
    print("Time taken: {s}\n", .{std.fmt.fmtDurationSigned(e - s)});
}
