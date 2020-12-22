# frozen_string_literal: true

def score(cards)
  size = cards.size
  cards.map.with_index.reduce(0) do |r, (c, i)|
    r + c * (size - i)
  end
end

def part1(players)
  p1cards = players[0][:cards].dup
  p2cards = players[1][:cards].dup
  while !p1cards.empty? && !p2cards.empty?
    p1, p2 = p1cards.shift, p2cards.shift
    p1 > p2 ? p1cards += [p1, p2] : p2cards += [p2, p1]
  end
  p1cards.empty? ? score(p2cards) : score(p1cards)
end

def recur_combat(p1cards, p2cards, visited, depth)
  loop do
    key = "#{p1cards.join(',')}:#{p2cards.join(',')}"
    return depth.zero? ? score(p1cards) : 0 if visited.key?(key)

    visited[key] = true
    p1, p2 = p1cards.shift, p2cards.shift

    if p1cards.size >= p1 && p2cards.size >= p2
      r = recur_combat(p1cards.dup[0...p1], p2cards.dup[0...p2], {}, depth + 1)
      r.zero? ? p1cards += [p1, p2] : p2cards += [p2, p1]
    else
      p1 > p2 ? p1cards += [p1, p2] : p2cards += [p2, p1]
    end

    if depth.zero?
      return score(p1cards) if p2cards.empty?
      return score(p2cards) if p1cards.empty?
    end

    return 0 if p2cards.empty?
    return 1 if p1cards.empty?
  end
end

def part2(players)
  p1cards = players[0][:cards].dup
  p2cards = players[1][:cards].dup
  visited = {}
  recur_combat(p1cards, p2cards, visited, 0)
end

def parse_player_cards(lines)
  {
    id: lines[0],
    cards: lines[1..-1].map(&:to_i)
  }
end

players = ARGF.read.split("\n\n").map { |l| parse_player_cards(l.split("\n")) }
p part1(players)
p part2(players)
