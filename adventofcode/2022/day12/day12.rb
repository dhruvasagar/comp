# frozen_string_literal: true

class Grid
  attr_accessor :grid, :start, :end

  def initialize(lines)
    @grid = lines.split.map(&:chars)

    (0...@grid.size).each do |y|
      (0...@grid[0].size).each do |x|
        @start = [x, y] if @grid[y][x] == 'S'
        @end = [x, y] if @grid[y][x] == 'E'
      end
    end
  end

  def get(point)
    return 'a' if point == @start
    return 'z' if point == @end

    x, y = point
    @grid[y][x]
  end

  def find_char(char)
    ps = []
    (0...@grid.size).each do |y|
      (0...@grid[0].size).each do |x|
        p = [x, y]
        ps.push(p) if self.get(p) == char
      end
    end
    ps
  end

  def inside?(point)
    x, y = point
    xsize = @grid[0].size
    ysize = @grid.size

    x >= 0 && x < xsize && y >= 0 && y < ysize
  end

  def dist(p1, p2)
    x1, y1 = p1
    x2, y2 = p2
    ord1 = p1 == self.start ? 'a'.ord : @grid[y1][x1].ord
    ord2 = p2 == self.end ? 'z'.ord : @grid[y2][x2].ord
    ord2 - ord1
  end

  def neighbours(point)
    x, y = point
    [
      [x, y - 1],
      [x - 1, y],
      [x + 1, y],
      [x, y + 1],
    ].filter {|p| self.inside?(p)}
  end

  def find_shortest_path(pstart, pend)
    q = [[pstart, 0]]
    vis = {}
    vis[pstart] = true
    while !q.empty?
      p, steps = q.shift
      return steps if p == pend

      self.neighbours(p).each do |n|
        if !vis[n] && self.dist(p, n) <= 1
          vis[n] = true
          q.push([n, steps + 1])
        end
      end
    end
    return (2**(0.size * 8 -2) -1)
  end
end

def read_input
  ARGF.read
end

def part1(grid)
  grid.find_shortest_path(grid.start, grid.end)
end

def part2(grid)
  grid.find_char('a').map do |s|
    grid.find_shortest_path(s, grid.end)
  end.min
end

grid = Grid.new(read_input)
p part1 grid
p part2 grid
