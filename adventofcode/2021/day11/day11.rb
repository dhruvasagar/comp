# frozen_string_literal: true

def read_input
  ARGF.readlines
end

ENERGY_THRESHOLD = 9

def display(grid)
  grid.each do |row|
    row.each(&method(:print))
    puts
  end
  puts
end

def neighs(point)
  x, y = point
  [
    [x - 1, y - 1], [x - 1, y], [x - 1, y + 1],
    [x, y - 1], [x, y + 1],
    [x + 1, y - 1], [x + 1, y], [x + 1, y + 1]
  ]
end

def inside?(grid, point)
  x, y = point
  my = grid.size
  mx = grid[0].size
  x >= 0 && x < mx && y >= 0 && y < my
end

def flash(grid, memo, point)
  x, y = point
  grid[y][x] = 0
  memo[[x, y]] = true

  fc = 1
  neighs(point).each do |i, j|
    next if memo.key?([i, j]) || !inside?(grid, [i, j])

    o = grid[j][i]
    o == 9 ? fc += flash(grid, memo, [i, j]) : grid[j][i] += 1
  end
  fc
end

def step(grid)
  fc = 0
  memo = {}
  grid.each.with_index do |row, y|
    row.each.with_index do |o, x|
      next if memo.key?([x, y])

      o == 9 ? fc += flash(grid, memo, [x, y]) : grid[y][x] += 1
    end
  end
  fc
end

def part1(grid)
  p 100.times.to_a.reduce(0) { |fc| fc + step(grid) }
end

def all_flashed?(grid)
  grid.map(&:sum).sum.zero?
end

def part2(grid)
  i = 100 # done in part1
  while step grid
    i += 1
    break if all_flashed?(grid)
  end
  p i
end

grid = read_input.map { |l| l.chomp.chars.map(&:to_i) }
part1 grid
part2 grid
