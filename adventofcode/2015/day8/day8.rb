# frozen_string_literal: true

def part1(strings)
  strings.reduce(0) { |r, s| r + s.size - s.undump.size }
end

def part2(strings)
  strings.reduce(0) { |r, s| r + s.dump.size - s.size }
end

def read_input
  ARGF.readlines
end

strings = read_input.map(&:chomp)
p part1(strings.dup)
p part2(strings.dup)
