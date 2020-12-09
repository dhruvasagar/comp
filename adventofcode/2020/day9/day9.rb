# frozen_string_literal: true

def read_input
  ARGF.readlines
end

PREAMBLE_SIZE = 25

def valid?(nums, num, index)
  sindex = index - PREAMBLE_SIZE
  nums[sindex...index].combination(2).any? { |n1, n2| n1 + n2 == num }
end

def part1(nums)
  index = PREAMBLE_SIZE
  while index < nums.size
    num = nums[index]
    return num unless valid?(nums, num, index)

    index += 1
  end
end

def find_subset(nums, num, index)
  rindex = index
  while rindex < nums.size
    num -= nums[rindex]

    return nil if num.negative?
    return rindex if num.zero?

    rindex += 1
  end
end

def part2(nums)
  num = part1(nums)
  index = 0
  while index < nums.size
    rindex = find_subset(nums, num, index)
    if rindex
      subset = nums[index..rindex]
      return subset.min + subset.max
    end
    index += 1
  end
end

nums = read_input.map(&:to_i)
p part1(nums)
p part2(nums)
