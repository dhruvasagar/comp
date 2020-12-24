# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def part1(data)
  data.map { |d| d.max - d.min }.sum
end

def part2(data)
  data.map do |d|
    nums = d.combination(2).detect do |a, b|
      (a % b).zero? || (b % a).zero?
    end
    nums.max / nums.min
  end.sum
end

data = read_input.map(&:chomp).map { |r| r.split.map(&:to_i) }
p part1(data)
p part2(data)
