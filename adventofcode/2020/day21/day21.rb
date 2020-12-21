# frozen_string_literal: true

def read_input
  ARGF.readlines
end

REGEX = /^(.+?) \(contains (.+)\)$/.freeze

def part1(ingredients, allergen_map)
  non_allergic = ingredients.flatten.uniq - allergen_map.values.flatten.uniq
  ingredients.reduce(0) do |s, is|
    s + (is & non_allergic).size
  end
end

def part2(allergen_map)
  rmap = {}
  allsortedarray = allergen_map.to_a
  until allsortedarray.empty?
    allsortedarray.sort_by! { |_, v| v.size }
    ingredient, allergen = allsortedarray.shift
    rmap[allergen.first] = ingredient
    allsortedarray = allsortedarray.map { |k, v| [k, v - [allergen.first]] }
  end
  rmap.keys.sort_by { |i| rmap[i] }.join(',')
end

def parse_line(line, allergen_map)
  m = REGEX.match(line.chomp)
  ingredients = m[1].split.map(&:strip)
  allergens = m[2].split(',').map(&:strip)
  allergens.each do |allergen|
    allergen_map[allergen] ||= ingredients
    allergen_map[allergen] &= ingredients
  end
  ingredients
end

def parse_input(lines)
  ingredients = []
  allergen_map = {}
  lines.each do |line|
    ingredients << parse_line(line, allergen_map)
  end
  [
    ingredients,
    allergen_map
  ]
end

ingredients, allergen_map = parse_input(read_input)
p part1(ingredients, allergen_map)
p part2(allergen_map)
