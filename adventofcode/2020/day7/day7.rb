# frozen_string_literal: true

def read_input
  ARGF.readlines
end

def parse_bag_color(bag)
  color = bag.split(/bag/).first.strip
  color = color[2..-1] if color =~ /^\d/
  color
end

def bag_rules(bag_data)
  rules = {}
  bag_data.each do |bag|
    outter_bag, inner_bags = bag.split(/contain/)
    outter_bag_color = parse_bag_color(outter_bag)
    inner_bag_colors = inner_bags.split(',').map { |b| parse_bag_color(b) }
    rules[outter_bag_color] = inner_bag_colors
  end
  rules
end

SHINY_BAG = 'shiny gold'
LEAF_BAG = 'no other'

def can_contain?(rules, big, small)
  return false unless rules[big]
  return true if rules[big].include?(small)

  rules[big].any? { |b| can_contain?(rules, b, small) }
end

def part1(bag_rules)
  bag_rules.keys.select do |color|
    can_contain?(bag_rules, color, SHINY_BAG)
  end.size
end

def parse_bag(bag)
  color = bag.split(/bag/).first.strip
  count = 1
  if color =~ /^\d/
    count = color.split.first.to_i
    color = color.split[1..-1].join(' ')
  end
  { color: color, count: count }
end

def bag_rules2(bag_data)
  rules = {}
  bag_data.each do |bag|
    outter_bag, inner_bags = bag.split(/contain/)
    outter_bag_color = parse_bag(outter_bag)
    if inner_bags.match(LEAF_BAG)
      rules[outter_bag_color[:color]] = nil
    else
      inner_bag_colors = inner_bags.split(',').map { |b| parse_bag(b) }
      rules[outter_bag_color[:color]] = inner_bag_colors
    end
  end
  rules
end

def contains(rules, big_color)
  return 0 unless rules[big_color]

  count = 0
  rules[big_color].each do |bag|
    count += (1 + contains(rules, bag[:color])) * bag[:count]
  end
  count
end

def part2(bag_rules)
  contains(bag_rules, SHINY_BAG)
end

bag_data = read_input
p part1(bag_rules(bag_data))
p part2(bag_rules2(bag_data))
