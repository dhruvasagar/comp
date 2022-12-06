# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def sub(line, len)
  return len if line[len].chars.uniq.size == len

  idx = 1;
  while idx < line.size
    return (idx + len) if line[idx...(idx+len)].chars.uniq.size == len

    idx += 1
  end
end

def part1 line
  sub line, 4
end

def part2 line
  sub line, 14
end

line = read_input[0]
p part1 line
p part2 line
