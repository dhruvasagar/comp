# frozen_string_literal: true

class Grid
  attr_reader :start, :map, :xmax, :ymax

  def initialize(lines)
    @ymax = lines.size
    @map = lines.each_index.reduce({}) {|h, y|
      line = lines[y].chomp
      @xmax ||= line.size
      line.chars.each_index {|x|
        pos = [x, y]
        h[pos] = lines[y][x]
        @start = pos if lines[y][x] == 'S'
      }
      h
    }
  end

  def is_rock?(pos) = @map[pos] == '#'
  def is_garden_plot?(pos) = @map[pos] == '.'
  def outside?(pos) = pos[0] < 0 || pos[1] < 0 || pos[0] >= @xmax || pos[1] >= @ymax

  def modpos(pos)
    x, y = pos
    x = x % @xmax if x < 0 || x >= @xmax
    y = y % @ymax if y < 0 || y >= @ymax
    [x, y]
  end

  def neighs(pos)
    x, y = pos
    [
      [x, y - 1],
      [x + 1, y],
      [x, y + 1],
      [x - 1, y]
    ]
  end

  def bfs(max)
    vis = {}
    count = 0
    parity = max % 2
    queue = neighs(@start).map {|n| [n, 1] }
    while !queue.empty?
      pos, steps = queue.shift
      break if steps > max
      next if vis[pos] || is_rock?(modpos(pos))

      vis[pos] = true
      count += 1 if steps % 2 == parity
      neighs(pos).each { |n|
        next if vis[n] || is_rock?(modpos(n))
        queue << [n, steps + 1]
      }
    end
    count
  end
end

def quad(a, b, c, n) = a * n * n + b * n + c

def part2(grid)
  grids = 26501365 / grid.ymax
  rem = 26501365 % grid.ymax

  seq = (0...3).map {|n| grid.bfs(n * grid.ymax + rem)}

  c = seq[0]
  aPlusB = seq[1] - c
  fourAPlusTwoB = seq[2] - c
  twoA = fourAPlusTwoB - (2 * aPlusB)
  a = twoA / 2
  b = aPlusB - a

  quad(a, b, c, grids)
end

lines = ARGF.readlines
grid = Grid.new(lines)
p grid.bfs(64)
p part2(grid)
