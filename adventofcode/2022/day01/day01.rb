# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def part1 sums
  sums.max
end

def part2 sums
  sums.sort.reverse[0...3].sum
end

cals = read_input.map(&:to_i)
cal_sums = cals.chunk_while { |_, n2| n2 != 0 }.map(&:sum)

p part1 cal_sums
p part2 cal_sums
