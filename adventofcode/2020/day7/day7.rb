# frozen_string_literal: true

def read_input
  ARGF.readlines
end

SHINY_BAG = 'shiny gold'
LEAF_BAG = 'no other'

def parse_bag(bag)
  color = bag.split(/bag/).first.strip
  m = color.match(/^(\d+)?\s*(.*)$/)
  count = m[1].to_i
  color = m[2]
  { color: color, count: count }
end

def bag_rules(bag_data)
  bag_data.each_with_object({}) do |bag, rules|
    outter_bag, inner_bags = bag.split(/contain/)
    outter_bag = parse_bag(outter_bag)
    if inner_bags.match(LEAF_BAG)
      rules[outter_bag[:color]] = nil
    else
      inner_bag_colors = inner_bags.split(',').map { |b| parse_bag(b) }
      rules[outter_bag[:color]] = inner_bag_colors
    end
  end
end

def can_contain?(rules, big, small)
  return false unless rules[big]
  return true if rules[big].any? { |b| b[:color] == small }

  rules[big].any? { |b| can_contain?(rules, b[:color], small) }
end

def part1(bag_rules)
  bag_rules.keys.select do |color|
    can_contain?(bag_rules, color, SHINY_BAG)
  end.size
end

def contains(rules, big_color)
  return 0 unless rules[big_color]

  rules[big_color].reduce(0) do |count, bag|
    count + (1 + contains(rules, bag[:color])) * bag[:count]
  end
end

def part2(bag_rules)
  contains(bag_rules, SHINY_BAG)
end

data = read_input
rules = bag_rules(data)
p part1(rules)
p part2(rules)
