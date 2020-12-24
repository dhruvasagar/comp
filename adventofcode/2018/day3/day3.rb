# frozen_string_literal: true

def read_input
  ARGF.readlines
end

CLAIM_REGEX = /^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/.freeze

class Rectangle
  attr_accessor :id, :x1, :y1, :x2, :y2

  def initialize(id:, x1:, y1:, x2:, y2:)
    @id, @x1, @y1, @x2, @y2 = id, x1, y1, x2, y2
  end

  def overlap?(rect)
    return false if @x2 < rect.x1 || @x1 > rect.x2
    return false if @y2 < rect.y1 || @y1 > rect.y2

    true
  end

  def overlap(rect)
    # id = "o#{@id},#{rect.id}"
    rx1 = [@x1, rect.x1].max
    ry1 = [@y1, rect.y1].max
    rx2 = [@x2, rect.x2].min
    ry2 = [@y2, rect.y2].min
    (rx1..rx2).to_a.product((ry1..ry2).to_a)
    # Rectangle.new(
    #   id: id,
    #   x1: [@x1, rect.x1].max,
    #   y1: [@y1, rect.y1].max,
    #   x2: [@x2, rect.x2].min,
    #   y2: [@y2, rect.y2].min
    # )
  end

  def area
    (x2 - x1 + 1) * (y2 - y1 + 1)
  end

  def to_s
    "Rectangle[#{@x1},#{@x2}:#{@x2},#{@y2}]"
  end
end

def parse_input(lines)
  lines.map do |line|
    m = CLAIM_REGEX.match(line)
    x1, y1, w, h = m[2..5].map(&:to_i)
    Rectangle.new(id: m[1], x1: x1, y1: y1, x2: x1 + w - 1, y2: y1 + h - 1)
  end
end

def part1(rectangles)
  rectangles
    .combination(2)
    .select {|r1, r2| r1.overlap?(r2)}
    .map {|r1, r2| r1.overlap(r2)}
    .flatten(1).uniq.size
end

def part2(rectangles)
  overlap = {}
  rectangles.size.times do |i|
    r1 = rectangles[i]
    next if overlap.key?(r1.id)

    bad = ((i + 1)...rectangles.size).any? do |j|
      r2 = rectangles[j]
      next true if overlap.key?(r2.id)

      b = r1.overlap?(r2)
      if b
        overlap[r1.id] = true
        overlap[r2.id] = true
      end
      b
    end
    return r1.id unless bad
  end
  p overlap
end

lines = read_input.map(&:chomp)
rectangles = parse_input(lines)
# p part1(rectangles)
p part2(rectangles)
