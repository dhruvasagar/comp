# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def revh(hash)
  hh = Hash.new { |h, k| h[k] = [] }
  hash.each do |k, v|
    hh[v] <<= k
  end
  hh
end

def ccount(word)
  ch = Hash.new(0)
  word.chars.each do |c|
    ch[c] += 1
  end
  revh(ch)
end

def part1(words)
  cnt2 = 0
  cnt3 = 0
  words.map do |w|
    cnt = ccount(w)
    cnt2 += 1 unless cnt[2].empty?
    cnt3 += 1 unless cnt[3].empty?
  end
  cnt2 * cnt3
end

def part2(words)
  a, b = words.combination(2).detect do |a, b|
    aa = a.chars.each.with_index.to_a
    bb = b.chars.each.with_index.to_a
    (aa - bb).size == 1 && (bb - aa).size == 1
  end
  (a.chars - (a.chars - b.chars)).join
end

words = read_input.map(&:chomp)
p part1(words)
p part2(words)
