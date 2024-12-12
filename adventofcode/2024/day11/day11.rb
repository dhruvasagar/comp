# frozen_string_literal: true

@mem = {}
def solve stone, blinks
  return 1 if blinks == 0
  return @mem[[stone, blinks]] if @mem.key?([stone, blinks])

  @mem[[stone, blinks]] = if stone == 0
                            solve(1, blinks - 1)
                          elsif stone.to_s.size % 2 == 0
                            mid = (stone.to_s.size / 2)
                            solve(stone.to_s[0...mid].to_i, blinks - 1) + solve(stone.to_s[mid..].to_i, blinks - 1)
                          else
                            solve(stone * 2024, blinks - 1)
                          end
end

def part1(nums) = nums.map { |num| solve(num, 25)}.sum
def part2(nums) = nums.map { |num| solve(num, 75)}.sum

nums = ARGF.read.chomp.split.map(&:to_i)
p part1 nums
p part2 nums
