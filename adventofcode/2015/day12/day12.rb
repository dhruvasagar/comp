# frozen_string_literal: true

require 'json'

def part1(json)
  json.scan(/-?\d+/).map(&:to_i).sum
end

def sum_non_red(jobj)
  return 0 if jobj.is_a?(String)
  return jobj if jobj.is_a?(Integer)
  return jobj.map { |j| sum_non_red(j) }.sum if jobj.is_a?(Array)
  return 0 if jobj.is_a?(Hash) && jobj.values.any? { |v| v =~ /red/ }

  sum_non_red(jobj.values)
end

def part2(json)
  jobj = JSON.parse(json)
  sum_non_red(jobj)
end

json = gets.chomp
p part1(json)
p part2(json)
