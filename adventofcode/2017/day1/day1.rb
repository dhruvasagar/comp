# frozen_string_literal: true

def part1(nums)
  sum = 0
  index = 0
  while index < nums.size
    nindex = (index + 1) % nums.size
    sum += nums[index] if nums[index] == nums[nindex]
    index += 1
  end
  sum
end

def part2(nums)
  hd = nums.size / 2
  sum = 0
  index = 0
  while index < nums.size
    nindex = (index + hd) % nums.size
    sum += nums[index] if nums[index] == nums[nindex]
    index += 1
  end
  sum
end

nums = gets.chomp.chars.map(&:to_i)
p part1(nums)
p part2(nums)
