# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def reg?(num)
  num.is_a?(Integer)
end

def pure?(pair)
  return true if pair.is_a?(Integer)

  pair.all?(&method(:reg?))
end

def build_snail(num, depth = 1)
  return { value: num, depth: depth, regular: reg?(num) } if pure?(num)

  flat_snail(num, depth + 1)
end

def flat_snail(nums, depth = 1)
  # left, right = nums
  # [ build_snail(left), build_snail(right) ]

  list = []
  nums.each.with_index do |pair, index|
    if pure?(pair)
      list.push({ value: pair, depth: depth, regular: reg?(pair) })
    else
      list += flat_snail(pair, depth + 1)
    end
  end
  list
end

def explode(snail)
  index = snail.find_index { |s| s[:depth] == 4 && !s[:regular] }
  return false if index.negative?

  enum = snail[index]
  puts "exploding #{enum}"
  if index >= 0
    snum = snail[index - 1]
    if reg?(snum[:value])
      snum[:value] += enum[:value][0]
    else
      snum[:value][1] += enum[:value][0]
    end
    snail[index - 1] = snum
  end
  if index < snail.size
    snum = snail[index + 1]
    if reg?(snum[:value])
      snum[:value] += enum[:value][1]
    else
      snum[:value][0] += enum[:value][1]
    end
    snail[index + 1] = snum
  end
  snail[index][:value] = 0
  snail[index][:regular] = true
  true
end

def split(snail)
  index = snail.find_index do |s|
    (reg?(s) && s[:value] >= 10) || s[:value].any? { |v| v >= 10 }
  end
  return false if index.negative?

  snum = snail[index]
  half = snum[:value].to_f / 2
  new_val = [half.floor, half.ceil]

  if reg?(val)
    snail[index][:value] = new_val
    snail[index][:regular] = false
  elsif val[0] >= 10
    snail[index][:value] = val[1]
    snail[index][:regular] = true
    snail.insert(
      index - 1,
      {
        value: new_val,
        depth: snum[:depth] + 1,
        regular: false
      }
    )
  else # val[1] >= 10
    snail[index][:value] = val[0]
    snail[index][:regular] = true
    snail.insert(
      index,
      {
        value: new_val,
        depth: snum[:depth] + 1,
        regular: false
      }
    )
  end
  true
end

def reduce(snail)
  loop do
    prev_snail = snail
    explode(snail) || split(snail)
    break if prev_snail == snail
  end
  num
end

def add(snail1, snail2)
  [snail1, snail2]
end

def deep_array(depth)
  return [] if depth == 0

  [deep_array(depth - 1)]
end

def deep_push(arr, num, depth)
  if depth == 1
    if reg?(num)
      arr.push(num)
    else
      arr[-1] += num
    end
    return
  end
  # return arr.push(num) if depth == 1

  arr.push([]) if depth == 2 && arr[-1].size == 2

  deep_push(arr[-1], num, depth - 1)
end

def unflat(snail)
  max_depth = snail.map { |s| s[:depth] }.max
  r = deep_array(max_depth)
  snail.each do |num|

  end
end

def mag_pair(n1, n2)
  3 * n1 + 2 * n2
end

def magnitude(snail)
  snail.each_with_index.reduce(0) do |r, (num, index)|
    next r + num[:value] if num[:regular]

    n_of_left_higher = snail[0..index].select { |s| s[:depth] < num[:depth] }.size # right of
    n_of_right_higher = snail[(index + 1)..].select { |s| s[:depth] < num[:depth] }.size # left of

    c = 3 ** n_of_right_higher * 2 ** n_of_left_higher
    r + (3 * c * num[:value][0] + 2 * c * num[:value][1])
  end
end

def part1(snails)
  r = reduce(add(snails[0], snails[1]))
  snails[2..].each do |n|
    r = reduce(r.add(n))
  end
  puts r
  puts magnitude(r)
end

nums = read_input.map { |l| eval l }
p nums
puts
snails = nums.map(&method(:flat_snail)).flatten
p snails
puts

p magnitude(snails)
# explode(snails)
# p snails

# da = deep_array(4)
# p da
# deep_push(da, [4, 3], 4)
# p da
# deep_push(da, 4, 4)
# p da
# deep_push(da, 4, 3)
# p da
# deep_push(da, 7, 3)
# p da
# deep_push(da, [8, 4], 4)
# p da
