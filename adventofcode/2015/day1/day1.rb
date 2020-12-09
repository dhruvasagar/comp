# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def part1(directions)
  directions.chars.reduce(1) do |r, c|
    r + (c == '(' ? 1 : -1)
  end
end

def part2(directions)
  r = 0
  ans = -1
  directions.chars.each_with_index do |c, i|
    break if r == -1

    ans = i + 1
    r += (c == '(' ? 1 : -1)
  end
  ans
end

directions = read_input.first
p part1(directions)
p part2(directions)
