# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def small?(str)
  /[[:lower:]]/.match(str)
end

def all_paths(graph, start, ends, path, paths, &block)
  path += [start]

  if start == ends
    paths.push(path)
    return
  end

  graph[start].each do |np|
    all_paths(graph, np, ends, path, paths, &block) if !small?(np) || block.call(path, np)
  end
end

def part1(graph)
  paths = []
  all_paths(graph, 'start', 'end', [], paths) { |path, np| !path.include?(np) }
  paths.size
end

def valid_path?(path, node)
  return true unless path.include?(node)

  hc = Hash.new { |h, k| h[k] = 0 }
  path.select(&method(:small?)).each { |p| hc[p] += 1 }

  return false if hc[node] > 1

  vals = hc.values
  diff = vals.sum - vals.size
  diff.zero?
end

def part2(graph)
  paths = []
  all_paths(graph, 'start', 'end', [], paths) { |path, np| valid_path?(path, np) }
  paths.size
end

graph = Hash.new { |h, k| h[k] = [] }
read_input.each do |l|
  s, e = l.chomp.split('-')
  graph[s] << e unless e == 'start' || s == 'end'
  graph[e] << s unless s == 'start' || e == 'end'
end
p part1(graph)
p part2(graph)
