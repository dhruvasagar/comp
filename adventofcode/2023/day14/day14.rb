# frozen_string_literal: true

def tfmt(d) = "#{d/1_000_000}ms"

class Grid
  attr_accessor :grid, :ymax, :xmax

  def initialize(lines)
    @grid = lines
    @ymax, @xmax = lines.size, lines[0].size
  end

  def hash
    grid.map(&:join).join.hash
  end

  def tilt_step(dx, dy)
    new_grid = grid.map(&:dup)
    (0...xmax).each { |x|
      (0...ymax).each { |y|
        new_grid[y] ||= []
        nx, ny = x + dx, y + dy
        next if nx < 0 || ny < 0 || nx >= xmax || ny >= ymax

        if new_grid[y][x] == 'O' && new_grid[ny][nx] == '.'
          new_grid[y][x] = '.'
          new_grid[ny][nx] = 'O'
        end
      }
    }
    @grid = new_grid
  end

  def tilt(dx, dy)
    phash = hash
    while (true)
      tilt_step(dx, dy)
      break if phash == hash

      phash = hash
    end
  end

  def cycle
    [
      [0, -1],
      [-1, 0],
      [0, 1],
      [1, 0]
    ].each {|dx, dy|
        tilt(dx, dy)
    }
  end

  def total_load = (0...ymax).reduce(0) {|s, y| s + grid[y].count('O') * (ymax - y)}

  def to_s = grid.map(&:join).join("\n")
  alias :inspect :to_s
end

s = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
lines = ARGF.readlines.map(&:chomp).map(&:chars)
g = Grid.new(lines)

# Part 1
s1 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
g.tilt(0, -1)
puts g.total_load
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)

# Part 2
s2 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
g = Grid.new(lines)
hashes = [g.hash]
start = 0
again = false
final = (1..).each {|i|
  g.cycle
  start = hashes.index(g.hash)
  break i if hashes.include?(g.hash)

  hashes << g.hash
}
remaining = (1000000000 - final) % (final - start)
remaining.times { g.cycle }
puts g.total_load
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
e = Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)
puts "Time for part1: #{tfmt(e1-s1)}, part2: #{tfmt(e2-s2)}, total: #{tfmt(e-s)}"
