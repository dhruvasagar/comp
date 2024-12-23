# frozen_string_literal: true

@mem = {}
def valid?(pattern, towels)
  return true if towels[pattern]
  return @mem[pattern] if @mem.key?(pattern)

  @mem[pattern] = 1.upto(pattern.size-1).any? {|l|
    towels[pattern[0...l]] && valid?(pattern[l..], towels)
  }
end

@count_mem = {}
def valid_count(pattern, towels, count=0)
  return count if pattern.empty?
  return @count_mem[pattern] if @count_mem.key?(pattern)

  @count_mem[pattern] = 1.upto(pattern.size).filter_map {|l|
    valid_count(pattern[l..], towels, count) if towels[pattern[0...l]]
  }.sum
end

def part1(patterns, towels) = patterns.filter {|p| valid?(p, towels)}.size
def part2(patterns, towels) =
  patterns.map {|p|
    valid_count(p, towels, 1)
  }.sum
def tdiff(s,e) = "#{(e - s) * 1000}ms"

towelstr, patternstr = ARGF.read.split("\n\n")
towels = towelstr.split(", ").each_with_object({}) {|t, h| h[t]=true; h}
patterns = patternstr.split
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 patterns, towels
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 patterns, towels
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
