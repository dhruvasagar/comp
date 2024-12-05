# frozen_string_literal: true

def parse_rules(rulestr) =
  rulestr.split.map {|r| r.split("|").map(&:to_i)}
    .each_with_object({}) { |(l, r), h| (h[l] ||= []) << r }

def parse_updates(updatestr) = updatestr.split.map {|l| l.split(",").map(&:to_i)}

def is_valid? update, rules
  update.each.with_index do |num, idx|
    return false if idx == 0 && !rules.has_key?(num)
    return false if idx != 0 && update[0..(idx-1)].any? {|pnum| !rules[pnum]&.include?(num)}
  end
  true
end

def mid(update) = update[update.size / 2]

def part1(updates, rules) =
  updates.filter_map {|update| mid(update) if is_valid?(update, rules)}.sum

def fix_order(update, rules) =
  update.sort {|num1, num2| rules[num1]&.include?(num2) ? 1 : rules[num2]&.include?(num1) ? -1 : 0 }

def part2(updates, rules) =
  updates.filter_map {|update| mid(fix_order(update, rules)) if !is_valid?(update, rules)}.sum

input = ARGF.read
rulestr, updatestr = input.split("\n\n")
rules, updates = parse_rules(rulestr), parse_updates(updatestr)
p part1 updates, rules
p part2 updates, rules
