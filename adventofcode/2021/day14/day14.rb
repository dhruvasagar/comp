# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def expand(poly, rules)
  cs = poly.chars
  cs.each.map.with_index do |c, i|
    next [cs[i]] if i == cs.size - 1

    [c, rules[c + cs[i + 1]]].join
  end.join
end

def part1(poly, rules)
  np = 10.times.to_a.reduce(poly) { |p| expand(p, rules) }
  hc = Hash.new { |h, k| h[k] = 0 }
  np.chars.each_with_object(hc) { |c, h| h[c] += 1 }
  vals = hc.values
  vals.max - vals.min
end

lines = read_input
poly = lines[0].chomp
rules = lines[2...lines.size].each_with_object({}) do |l, m|
  pair, ins = l.chomp.split(' -> ')
  m[pair] = ins
end

# p part1(poly, rules)

def pair_freq(hcs, rules, count)
  count.times do
    nhc = Hash.new { |h, k| h[k] = 0 }
    hcs.each_with_object(nhc) do |(p, c), h|
      h["#{p[0]}#{rules[p]}"] += c
      h["#{rules[p]}#{p[1]}"] += c
    end
    hcs = nhc
  end
  hcs
end

def letter_freq(hcs, last)
  lc = Hash.new { |h, k| h[k] = 0 }
  hcs.each_with_object(lc) do |(p, c), h|
    h[p[0]] += c #hcs[p]
  end
  lc[last] += 1
  lc
end

def part2(poly, rules, count)
  hc = Hash.new { |h, k| h[k] = 0 }
  (0...(poly.size - 1)).each_with_object(hc) { |i, h| h[poly[i..(i + 1)]] += 1 }
  lc = letter_freq(pair_freq(hc, rules, count), poly[-1])
  vals = lc.values
  vals.max - vals.min
end

p part2(poly, rules, 10)
p part2(poly, rules, 40)
