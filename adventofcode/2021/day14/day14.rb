# frozen_string_literal: true

def read_input
  ARGF.readlines
end

lines = read_input
poly = lines[0].chomp
rules = lines[2...lines.size].each_with_object({}) do |l, m|
  pair, ins = l.chomp.split(' -> ')
  m[pair] = ins
end

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

def letter_freq(hcs)
  lc = Hash.new { |h, k| h[k] = 0 }
  hcs.each_with_object(lc) { |(p, c), h| h[p[0]] += c }
end

def step(poly, rules, count)
  hc = Hash.new { |h, k| h[k] = 0 }
  (0...(poly.size - 1)).each_with_object(hc) { |i, h| h[poly[i..(i + 1)]] += 1 }
  lc = letter_freq(pair_freq(hc, rules, count))
  lc[poly[-1]] += 1
  vals = lc.values
  vals.max - vals.min
end

p step(poly, rules, 10)
p step(poly, rules, 40)
