# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def mix nums
  nums.size.times do |ni|
    i = nums.find_index {|_, ii| ii == ni}
    v = nums.delete_at(i)
    nums.insert((i + v[0]) % nums.size, v)
  end
  nums
end

def groove_coordinates nums
  i = nums.find_index { |n, _| n == 0 }
  x = nums[(i + 1000) % nums.size][0]
  y = nums[(i + 2000) % nums.size][0]
  z = nums[(i + 3000) % nums.size][0]
  x + y + z
end

def part1 nums
  nums = mix nums.each_with_index.map { |n, i| [n, i] }
  groove_coordinates nums
end

def part2 nums
  nums = nums.each_with_index.map { |n, i| [n * 811589153, i] }
  10.times do
    nums = mix nums
  end
  groove_coordinates nums
end

nums = read_input.map(&:to_i)
p part1 nums
p part2 nums
