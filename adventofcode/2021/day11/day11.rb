# frozen_string_literal: true

def read_input
  ARGF.readlines
end

ENERGY_THRESHOLD = 9

def display(grid)
  grid.each { |row| puts row.join }
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
    next if memo.key?([i, j])
    next unless inside?(grid, [i, j])

    grid[j][i] == 9 ? fc += flash(grid, memo, [i, j]) : grid[j][i] += 1
  end
  fc
end

def step(grid)
  fc = 0
  memo = {}
  grid.each_index do |y|
    grid[y].each_index do |x|
      next if memo.key?([x, y])

      grid[y][x] == 9 ? fc += flash(grid, memo, [x, y]) : grid[y][x] += 1
    end
  end
  fc
end

def part1(grid)
  (1..100).reduce(0) { |fc| fc + step(grid) }
end

def all_flashed?(grid)
  grid.map(&:sum).sum.zero?
end

def part2(grid)
  (101..).each do |i|
    step grid
    return i if all_flashed?(grid)
  end
end

grid = read_input.map { |l| l.chomp.chars.map(&:to_i) }
p part1 grid
p part2 grid
