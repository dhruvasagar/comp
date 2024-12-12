# frozen_string_literal: true

class Garden
  attr_accessor :ymax, :xmax, :grid, :grid_map, :garden_region_map

  def initialize(lines)
    @grid = []
    @grid_map = {}
    @garden_region_map = {}
    @ymax, @xmax = lines.size, lines.first.size
    lines.each.with_index {|row, y|
      @grid << row.chars
      row.chars.each_with_index {|c, x|
        pos = [x, y]
        @grid_map[pos] = c
        # @garden_region_map[c] ||= []
        # if @garden_region_map[c].empty?
        #   @garden_region_map[c] << pos
        # else
        #   if @garden_region_map[c].any? {|gpos| is_neigh?(gpos, pos)}
        #     @garden_region_map[c] << pos
        #   else
        #     (@garden_region_map["#{c}#{@garden_region_map.keys.size}"] ||= []) << pos
        #   end
        # end
      }
    }
    build_region_map
  end

  def build_region_map
    vis = {}
    ymax.times {|y|
      xmax.times {|x|
        pos = [x,y]
        next if vis[pos]

        c = @grid_map[pos]
        key = @garden_region_map.key?(c) ? "#{c}#{@garden_region_map.keys.size}" : c
        @garden_region_map[key] ||= []

        q = [pos]
        while !q.empty?
          top = q.pop
          next if vis[top]

          vis[top] = true
          @garden_region_map[key] << top
          neighs(top)
            .filter {|np| !vis[np] && c == @grid_map[np]}
            .each {|np| q << np}
        end
      }
    }
  end

  def to_s
    p @grid
    p @grid_map
    p @garden_region_map
  end
  alias_method :to_s, :inspect

  def inside?((x,y))
    x >= 0 && y >= 0 && x < xmax && y < ymax
  end

  def neighs((x,y), all=false)
    [[x - 1, y], [x, y - 1], [x + 1, y], [x, y + 1]].select {|p| all || inside?(p)}
  end

  def is_neigh?(pos, npos)
    neighs(pos).include?(npos)
  end

  def area(region)
    region.size
  end

  def perimeter(region)
    region.reduce(0) {|per, pos|
      per + neighs(pos, true).count {|np| @grid_map[pos] != @grid_map[np] }
    }
  end

  def perimeter2(region)
    region.reduce(0) {|per, (x, y)|
      c = @grid_map[[x, y]]
      per += 1 if @grid_map[[x - 1, y]] != c && @grid_map[[x, y - 1]] != c
      per += 1 if @grid_map[[x + 1, y]] != c && @grid_map[[x, y - 1]] != c
      per += 1 if @grid_map[[x - 1, y]] != c && @grid_map[[x, y + 1]] != c
      per += 1 if @grid_map[[x + 1, y]] != c && @grid_map[[x, y + 1]] != c
      per += 1 if @grid_map[[x - 1, y]] == c && @grid_map[[x, y - 1]] == c && @grid_map[[x - 1, y - 1]] != c
      per += 1 if @grid_map[[x + 1, y]] == c && @grid_map[[x, y - 1]] == c && @grid_map[[x + 1, y - 1]] != c
      per += 1 if @grid_map[[x - 1, y]] == c && @grid_map[[x, y + 1]] == c && @grid_map[[x - 1, y + 1]] != c
      per += 1 if @grid_map[[x + 1, y]] == c && @grid_map[[x, y + 1]] == c && @grid_map[[x + 1, y + 1]] != c
      per
    }
  end

  def price
    garden_region_map.keys.map {|r| area(garden_region_map[r]) * perimeter(garden_region_map[r])}.sum
  end

  def price2
    garden_region_map.keys.map {|r| area(garden_region_map[r]) * perimeter2(garden_region_map[r])}.sum
  end
end

def part1(garden) = garden.price
def part2(garden) = garden.price2

lines = ARGF.readlines
garden = Garden.new(lines.map(&:chomp))
p part1 garden
p part2 garden
