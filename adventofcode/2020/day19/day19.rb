# frozen_string_literal: true

def read_input
  ARGF.read
end

def parse_rule(rule)
  if rule =~ /"/
    rule.gsub('"', '')
  elsif rule =~ /^[\d\s]+$/
    rule.split.map(&:to_i)
  elsif rule =~ /\|/
    rule.split(' | ').map { |r| parse_rule(r) }
  end
end

def recur_regex(id, rule, rules)
  @recnt += 1
  "(?<e#{@recnt}>#{rule.map do |r|
    if r == id
      "\\g<e#{@recnt}>*"
    else
      rule_regex(r, rules)
    end
  end.join})"
end

def group_regex(rule, rules)
  "(?:#{rule.map do |r|
    r.map { |rr| rule_regex(rr, rules) }.join
  end.join('|')})"
end

def array_regex(id, rule, rules)
  recur = rule.any? { |r| r.include?(id) }
  if recur
    recur_regex(id, rule[1], rules)
  else
    group_regex(rule, rules)
  end
end

def rule_regex(id, rules)
  return rules[id] if rules[id].is_a?(String)

  rule = rules[id]
  if rule.is_a?(Array) && rule[0].is_a?(Array)
    array_regex(id, rule, rules)
  else
    rule.map { |r| rule_regex(r, rules) }.join
  end
end

def rules_regex(id, rules)
  @recnt = 0
  Regexp.new("^#{rule_regex(id, rules)}$")
end

def parse_rules(lines)
  lines.split("\n").each_with_object({}) do |rule, hash|
    k, v = rule.split(': ')
    v = parse_rule(v)
    hash[k.to_i] = v
  end
end

def part1(values, regexp)
  values.select { |v| regexp.match(v) }.size
end

rules, values = read_input.split("\n\n")
values = values.split("\n")
rules = parse_rules(rules)

puts part1(values, rules_regex(0, rules))

rules[8] = [[42], [42, 8]]
rules[11] = [[42, 31], [42, 11, 31]]
puts part1(values, rules_regex(0, rules))
