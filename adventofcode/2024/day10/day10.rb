# frozen_string_literal: true

def neighs((x,y)) = [[x-1, y], [x, y-1], [x+1, y], [x, y+1]]

class TopographicalMap
  attr_accessor :ymax, :xmax, :pmap, :trail_head_map

  def initialize(grid)
    @ymax, @xmax = grid.size, grid[0].size
    @pmap = {}
    @trail_head_map = {}
    grid.each.with_index {|row, y|
      row.each.with_index {|t, x|
        pos = [x, y]
        @pmap[pos] = t
        @trail_head_map[pos] = true if t.zero?
      }
    }
  end

  def inside? pos
    x, y = pos
    x >= 0 && y >= 0 && x < xmax && y < ymax
  end

  def score(rating: false)
    trail_head_map.keys.reduce(0) {|score, pos|
      q = [pos]
      vis = {}
      while !q.empty?
        top = q.pop
        next if !rating && vis[top]

        vis[top] = true
        if pmap[top] == 9
          score += 1
          next
        end
        q += neighs(top).filter {|np| (rating || !vis[np]) && inside?(np) && pmap[np] == pmap[top] + 1}
      end
      score
    }
  end
end

def part1(tmap) = tmap.score
def part2(tmap) = tmap.score(rating: true)

map = ARGF.readlines.map {|l| l.chomp.chars.map(&:to_i)}
tmap = TopographicalMap.new(map)
p part1 tmap
p part2 tmap
