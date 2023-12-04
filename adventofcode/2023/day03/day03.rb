# frozen_string_literal: true

def read_input
  ARGF.readlines.map(&:chomp)
end

def is_digit?(c)
  c =~ /[0-9]/
end

def is_symbol?(c)
  c != '.' && c !~ /[0-9]/
end

def neighs(grid, pos)
  x, y = pos
  [
    [x, y + 1],
    [x, y - 1],
    [x + 1, y + 1],
    [x + 1, y],
    [x + 1, y - 1],
    [x - 1, y + 1],
    [x - 1, y],
    [x - 1, y - 1],
  ]
end

def get_number(grid, pos)
  x, y = pos
  c = grid[y][x]
  return unless is_digit?(c)

  numstr = c
  xi = x
  while is_digit?(grid[y][xi-1])
    numstr = "#{grid[y][xi-1]}#{numstr}"
    xi -= 1
  end
  xi = x
  while is_digit?(grid[y][xi+1])
    numstr = "#{numstr}#{grid[y][xi+1]}"
    xi += 1
  end
  numstr
end

def pad_grid(grid, ch)
  grid = grid.map {|y| "#{ch}#{y}#{ch}"}
  grid.unshift(ch * grid[0].size)
  grid.push(ch * grid[0].size)
end

grid = pad_grid(read_input, '.')
xmax, ymax = grid[0].size, grid.size

p2 = 0
part_numbers = []
(1...ymax).each { |y|
  numstr = ''
  is_num = false
  is_part_number = false
  (1...xmax).each { |x|
    c = grid[y][x]

    if is_digit?(c)
      is_num = true
      numstr = "#{numstr}#{c}"
    elsif is_num
      is_num = false
      if is_part_number
        part_numbers << numstr.to_i
        is_part_number = false
      end
      numstr = ''
    else
      numstr = ''
    end

    is_part_number ||= is_num && neighs(grid, [x, y]).any? { |xn, yn|
      is_symbol?(grid[yn][xn])
    }

    if c == '*'
      nums = neighs(grid, [x, y])
        .map {|p| get_number(grid, p)}
        .uniq
        .compact
        .map(&:to_i)
      p2 += nums[0] * nums[1] if nums.size == 2
    end
  }
}
# Part 1
p part_numbers.sum
# Part 2
p p2
