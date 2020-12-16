# frozen_string_literal: true

def read_input
  ARGF.read
end

def rule_ranges(rule)
  rule.split(' or ').map { |r| r.split('-').map(&:to_i) }.map { |r| r[0]..r[1] }
end

def valid?(field_rules, value)
  field_rules.values.any? do |rule|
    rule.any? { |r| r.include?(value) }
  end
end

def invalid_sum(field_rules, nearby_tickets)
  nearby_tickets.reduce(0) do |sum, nv|
    vs = nv.reject { |nvi| valid?(field_rules, nvi) }
    sum + vs.sum
  end
end

def part1(field_rules, nearby_tickets)
  invalid_sum(field_rules, nearby_tickets)
end

def reduce_maps(maps)
  maps.size.times.each_with_object({}) do |_, mv|
    index = maps.index { |m| m.size == 1 }
    mv[maps[index][0]] = index
    maps = maps.map { |m| m - maps[index] }
  end
end

def map_fields(field_rules, valid_tickets)
  maps = valid_tickets.transpose.map do |nv|
    nv.map do |nvi|
      field_rules.map do |name, ranges|
        name if ranges
                .select { |r| r.include?(nvi) }
                .size == 1
      end
    end.reduce(:&)
  end
  reduce_maps(maps)
end

def part2(field_rules, your_ticket, nearby_tickets)
  valid_tickets = nearby_tickets.reject do |nv|
    nv.any? { |nvi| !valid?(field_rules, nvi) }
  end
  fields = map_fields(field_rules, valid_tickets)
  fields
    .keys
    .select { |k| k =~ /departure/ }
    .map { |k| your_ticket[fields[k]] }
    .reduce(:*)
end

sections = read_input.split("\n\n")

field_rules = sections[0]
              .split("\n")
              .map { |r| r.split(':').map(&:strip) }
              .to_h
field_rules.each { |k, v| field_rules[k] = rule_ranges(v) }

your_ticket = sections[1]
              .split(":\n")[1]
              .split(',')
              .map(&:to_i)

nearby_tickets = sections[2]
                 .split(":\n")[1]
                 .split
                 .map { |r| r.split(',').map(&:to_i) }

p part1(field_rules, nearby_tickets)
p part2(field_rules, your_ticket, nearby_tickets)
