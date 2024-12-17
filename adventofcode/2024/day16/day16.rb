# frozen_string_literal: true

def next_pos((x, y), dirn)
  case dirn
  when :left
    [x - 1, y]
  when :top
    [x, y - 1]
  when :right
    [x + 1, y]
  when :down
    [x, y + 1]
  end
end

def neighs((x, y)) = [
  [[x - 1, y], :left],
  [[x, y - 1], :top],
  [[x + 1, y], :right],
  [[x, y + 1], :down],
]

class Maze
  attr_accessor :xmax, :ymax, :maze, :maze_map, :start_pos, :end_pos

  def initialize(maze_lines)
    @maze_map = {}
    @maze = maze_lines
    @ymax, @xmax = maze_lines.size, maze_lines[0].size
    @maze.each.with_index {|row, y|
      row.each.with_index {|c, x|
        @start_pos = [x, y] if c == 'S'
        @end_pos = [x, y] if c == 'E'
        @maze_map[[x, y]] = c
      }
    }
  end

  def debug(cpos)
    puts "Maze:"
    ymax.times {|y|
      puts xmax.times.map {|x|
        pos = [x, y]
        pos == cpos ? 'o' : @maze_map[pos]
      }.join
    }
    puts
    self
  end

  def inside?((x, y))
    x >= 0 && y >= 0 && x < xmax && y < ymax
  end

  def solve
    vis = {}
    cost = 0
    dirn = :right
    q = [[cost, dirn, @start_pos]]
    while !q.empty?
      cost, dirn, top = q.sort_by!(&:first).shift
      # p [cost, dirn, top, path, @end_pos, @maze_map[top]]
      # debug(top)
      return cost, vis if top == @end_pos
      next if vis[top]

      vis[top] = true
      q += neighs(top).filter_map {|(np, ndirn)|
        [dirn == ndirn ? cost + 1 : cost + 1001, ndirn, np] if !vis[np] && inside?(np) && @maze_map[np] != '#'
      }
    end
  end

  def solve_all best_cost
    cost = 0
    dirn = :right
    q = [[cost, dirn, @start_pos, [@start_pos]]]
    paths = []
    while !q.empty?
      cost, dirn, top, path = q.sort_by!(&:first).shift
      next if cost > best_cost
      # p [cost, dirn, top, path, @end_pos, @maze_map[top]]
      # debug(top)
      paths += path if top == @end_pos
      q += neighs(top).filter_map {|(np, ndirn)|
        [dirn == ndirn ? cost + 1 : cost + 1001, ndirn, np, path + [np]] if !path.include?(np) && inside?(np) && @maze_map[np] != '#'
      }
    end
    paths.uniq.size
  end
end

def part1(maze) = maze.solve.first
def part2(maze)
  cost, _ = maze.solve
  maze.solve_all cost
end
def tdiff(s,e) = "#{(e - s) * 1000}ms"

maze_lines = ARGF.readlines.map {|l| l.chomp.chars}
maze = Maze.new(maze_lines)
s = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part1 maze
e1 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
p part2 maze
e2 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
puts "Time taken part1: #{tdiff(s,e1)}, part2: #{tdiff(e1,e2)}, total: #{tdiff(s,e2)}"
