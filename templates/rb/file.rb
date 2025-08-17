# frozen_string_literal: true

def part1(lines) = 0
def part2(lines) = 0
def tdiff(s,e) = "#{(e - s) * 1000}ms"

lines = ARGF.readlines
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 lines
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 lines
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
