# frozen_string_literal: true

def read_input
  ARGF.read
end

def display(coords)
  xmax = coords.map(&:first).max + 1
  ymax = coords.map(&:last).max + 1
  cmap = coords.each_with_object({}) { |c, r| r[c] = true }
  ymax.times do |y|
    xmax.times do |x|
      print cmap[[x, y]] ? '#' : ' '
    end
    puts
  end
end

def fold(coords, point)
  x, y = point
  coords.map do |cx, cy|
    cx = x.zero? ? cx : x - (x - cx).abs
    cy = y.zero? ? cy : y - (y - cy).abs
    [cx, cy]
  end
end

def part1(coords, insts)
  coords = fold(coords, insts[0])
  coords.uniq.size
end

def part2(coords, insts)
  insts.each { |inst| coords = fold(coords, inst) }
  display coords
end

coords, insts = read_input.chomp.split("\n\n")
coords = coords.split("\n").map { |c| c.split(',').map(&:to_i) }
insts = insts.split("\n").map { |i| i.split(' ').last.split('=') }.map { |c, v| c == 'x' ? [v.to_i, 0] : [0, v.to_i] }
p part1(coords.map(&:dup), insts)
part2(coords.map(&:dup), insts)
