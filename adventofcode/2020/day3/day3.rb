# frozen_string_literal: true

def read_input
  ARGF.readlines
end

TREE = '#'
OPEN = '.'
SLOPES = [3, 1]

def square(spot)
  if spot == TREE
    'X'
  else
    'O'
  end
end

def tree_count(terrain, slope)
  tree_count = 0
  row, col = 0, 0
  row_max, col_max = terrain.length-1, terrain[0].length-1
  while row < row_max
    if col + slope[0] > col_max # wrap around
      row += slope[1]
      col = slope[0] - (col_max - col) - 1
    else
      col += slope[0]
      row += slope[1]
    end
    break if row > row_max
    tree_count += 1 if terrain[row][col] == TREE
  end
  tree_count
end

def part1(terrain)
  tree_count(terrain, SLOPES)
end

ALL_SLOPES = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
].freeze

def part2(terrain)
  ALL_SLOPES.map do |slope|
    tree_count(terrain, slope)
  end.inject(:*)
end

terrain = read_input.map { |r| r.scan(/./) }
puts part1(terrain)
puts part2(terrain)
