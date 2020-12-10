# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def part1(joltages)
  jolts = [0] + joltages + [joltages.max + 3]
  jolts.each_cons(2)
       .map { |j1, j2| j2 - j1 }
       .group_by { |j| j }
       .values
       .map(&:length)
       .reduce(:*)
end

def part2(jolts)
  # tribonacci sequence (sum of previous 3) applied to only values that
  # exist in jolts
  h = { 0 => 1 }
  jolts.each do |j|
    h[j] = ((j - 3)...j).map { |jp| h[jp] }.compact.sum
  end
  h.values.last
end

joltages = read_input.map(&:to_i).sort
p part1(joltages)
p part2(joltages)
