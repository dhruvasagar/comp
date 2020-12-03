# frozen_string_literal: true

def read_input
  ARGF.readlines
end

TREE = '#'
OPEN = '.'
SLOPES = [3, 1].freeze

def tree_count(terrain, slope)
  tree_count = 0
  row = 0
  col = 0
  row_max = terrain.length - 1
  col_max = terrain[0].length - 1
  while row < row_max
    if col + slope[0] > col_max # wrap around
      col = slope[0] - (col_max - col) - 1
    else
      col += slope[0]
    end
    row += slope[1]
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
  [1, 2]
].freeze

def part2(terrain)
  ALL_SLOPES.map do |slope|
    tree_count(terrain, slope)
  end.inject(:*)
end

terrain = read_input.map { |r| r.scan(/./) }
puts part1(terrain)
puts part2(terrain)
