def read_input
  ARGF.readlines
end

def parse_policy(policy)
  limits, char = policy.split
  limits = limits.split('-').map(&:to_i)
  [limits, char]
end

def pass_valid?(policy, pass)
  limits, char = parse_policy(policy)
  char_stats = Hash.new(0)
  pass.scan(/./).each do |p|
    char_stats[p] += 1
  end
  char_stats[char] >= limits[0] && char_stats[char] <= limits[1]
end

def at_pos(string, pos)
  string[pos - 1]
end

def pass_valid2?(policy, pass)
  positions, char = parse_policy(policy)
  (at_pos(pass, positions[0]) == char && at_pos(pass, positions[1]) != char) ||
    (at_pos(pass, positions[0]) != char && at_pos(pass, positions[1]) == char)
end

def part1(inputs)
  inputs.reduce(0) do |s, input|
    policy, pass = input.split(':')
    s += 1 if pass_valid?(policy, pass.strip)
    s
  end
end

def part2(inputs)
  inputs.reduce(0) do |s, input|
    policy, pass = input.split(':')
    s += 1 if pass_valid2?(policy, pass.strip)
    s
  end
end

inputs = read_input
puts part1(inputs)
puts part2(inputs)
