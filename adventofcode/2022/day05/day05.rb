# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def parse_stacks(lines, count)
  stacks = []
  count.times do |i|
    stack = ""
    lines.each do |line|
      index = i * 4 + 1
      stack = line[index] + stack unless line[index]&.empty?
    end
    stacks.push(stack.strip)
  end
  stacks
end

def move(stacks, count, from, to)
  fstr = stacks[from]
  stacks[to] += fstr[-count..].reverse
  stacks[from] = fstr[0...(fstr.size - count)]
end

def apply_moves(lines, stacks)
  lines.each do |line|
    if /move (\d+) from (\d+) to (\d+)/ =~ line
      move(stacks, $1.to_i, $2.to_i - 1, $3.to_i - 1)
    end
  end
end

def part1(stacks, moves)
  apply_moves(moves, stacks)
  stacks.map {|s| s[-1]}.join
end

def move2(stacks, count, from, to)
  fstr = stacks[from]
  stacks[to] += fstr[-count..]
  stacks[from] = fstr[0...(fstr.size - count)]
end

def apply_moves2(lines, stacks)
  lines.each do |line|
    if /move (\d+) from (\d+) to (\d+)/ =~ line
      move2(stacks, $1.to_i, $2.to_i - 1, $3.to_i - 1)
    end
  end
end

def part2(stacks, moves)
  apply_moves2(moves, stacks)
  stacks.map {|s| s[-1]}.join
end

lines = read_input
sindex = lines.find_index { |line| line.strip.start_with?("1") }

scount = lines[sindex].split.map(&:strip).size
stacks = parse_stacks(lines[0...sindex], scount)

puts part1 stacks.dup, lines[sindex..]
puts part2 stacks.dup, lines[sindex..]
