# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def is_visible?(grid, pos)
  x, y = pos
  h = grid[y][x]

  return true if x == grid.size - 1 || y == grid[0].size - 1

  l = (0...x).all? {|xi| grid[y][xi] < h}
  return true if l

  t = (0...y).all? {|yi| grid[yi][x] < h}
  return true if t

  r = ((x+1)...grid.size).all? {|xi| grid[y][xi] < h}
  return true if r

  b = ((y+1)...grid[0].size).all? {|yi| grid[yi][x] < h}
  return true if b

  return false
end

def part1 grid
  (0...grid.size).map do |xi|
    (0...grid[0].size).count do |yi|
      is_visible?(grid, [xi, yi])
    end
  end.sum
end

def scenic_score(grid, pos)
  x, y = pos
  h = grid[y][x]

  l = 0
  (0...x).to_a.reverse.each do |xi|
    l += 1
    break if grid[y][xi] >= h
  end

  t = 0
  (0...y).to_a.reverse.each do |yi|
    t += 1
    break if grid[yi][x] >= h
  end

  r = 0
  ((x+1)...grid.size).each do |xi|
    r += 1
    break if grid[y][xi] >= h
  end

  b = 0
  ((y+1)...grid[0].size).each do |yi|
    b += 1
    break if grid[yi][x] >= h
  end

  l * t * r * b
end

def part2 grid
  (0...grid.size).map do |xi|
    (0...grid[0].size).map do |yi|
      scenic_score(grid, [xi, yi])
    end.max
  end.max
end

lines = read_input
grid = lines.map(&:chomp).map(&:chars).map { |r| r.map(&:to_i) }
p part1 grid
p part2 grid
