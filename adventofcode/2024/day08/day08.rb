class FMap
  attr_accessor :fmap, :pmap, :amap, :xmax, :ymax

  def parse_input(lines)
    @ymax, @xmax = lines.size, lines[0].chomp.size
    @fmap = {}
    @pmap = {}
    @amap = {}
    lines.each.with_index { |line, y|
      line.chomp.chars.each.with_index {|c, x|
        pos = [x, y]
        (@fmap[c] ||= []) << pos if c != '.'
        @pmap[pos] = c
      }
    }
  end

  def inside?(pos)
    x, y = pos
    !x.nil? && !y.nil? && x >= 0 && y >= 0 && x < xmax && y < ymax
  end

  def find_antinodes
    @amap = {}
    fmap.each { |f, flocs|
      flocs.combination(2).each {|(x1, y1), (x2, y2)|
        apos1 = [
          x1 - (x2 - x1),
          y1 - (y2 - y1)
        ]
        apos2 = [
          x2 + (x2 - x1),
          y2 + (y2 - y1)
        ]
        @amap[apos1] = '#' if inside?(apos1)
        @amap[apos2] = '#' if inside?(apos2)
      }
    }
    @amap
  end

  def find_all_antinodes
    @amap = {}
    fmap.each { |f, flocs|
      flocs.combination(2).each {|(x1, y1), (x2, y2)|
        lposes = [[x1 - (x2 - x1), y1 - (y2 - y1)]]
        rposes = [[x2 + (x2 - x1), y2 + (y2 - y1)]]
        loop {
          nlpos = [lposes[-1][0] - (x2 - x1), lposes[-1][1] - (y2 - y1)]
          break if !inside?(nlpos)
          lposes << nlpos
        }
        loop {
          nrpos = [rposes[-1][0] + (x2 - x1), rposes[-1][1] + (y2 - y1)]
          break if !inside?(nrpos)
          rposes << nrpos
        }
        lposes.each {|lpos| @amap[lpos] = '#' if inside?(lpos)}
        rposes.each {|rpos| @amap[rpos] = '#' if inside?(rpos)}
      }
      flocs.each {|apos| @amap[apos] = '#'}
    }
    @amap
  end

  def to_s
    ymax.times.map {|y|
      xmax.times.map {|x|
        pos = [x, y]
        amap[pos] || pmap[pos]
      }.join
    }.join("\n")
  end
  alias_method :to_s, :inspect
end

def part1 fmap
  fmap.find_antinodes.size
end

def part2 fmap
  fmap.find_all_antinodes.size
end

lines = ARGF.readlines
fmap = FMap.new
fmap.parse_input(lines)
p part1(fmap)
p part2(fmap)
