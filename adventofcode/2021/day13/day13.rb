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
      print cmap[[x, y]] ? '#' : '.'
    end
    puts
  end
  puts
end

def fold_hor(coords, row)
  # fold up
  coords.map do |x, y|
    next [x, y] if y <= row

    [x, 2 * row - y]
  end
end

def fold_ver(coords, col)
  # fold left
  coords.map do |x, y|
    next [x, y] if x <= col

    [2 * col - x, y]
  end
end

def part1(coords, insts)
  c, val = insts[0] # only first
  coords = if c == 'x'
             fold_ver(coords, val.to_i)
           else
             fold_hor(coords, val.to_i)
           end
  coords.uniq.size
end

def part2(coords, insts)
  insts.each do |inst|
    c, val = inst
    coords = if c == 'x'
               fold_ver(coords, val.to_i)
             else
               fold_hor(coords, val.to_i)
             end
  end
  display coords
end

coords, insts = read_input.chomp.split("\n\n")
coords = coords.split("\n").map { |c| c.split(',').map(&:to_i) }
insts = insts.split("\n").map { |i| i.split(' ').last.split('=') }

p part1(coords.map(&:dup), insts)
part2(coords.map(&:dup), insts)
