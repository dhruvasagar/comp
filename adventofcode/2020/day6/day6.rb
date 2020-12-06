# frozen_string_literal: true

def read_input
  ARGF.read
end

def parse_input(answers)
  answers.split("\n\n")
end

def part1(answers)
  answers.map do |answer|
    answer.split.join.scan(/./).uniq.size
  end.reduce(:+)
end

def part2(answers)
  answers.map do |answer|
    as = answer.split
    comm_char = as.first
    as.each do |a|
      comm_char = (comm_char.scan(/./) & a.scan(/./)).join
    end
    comm_char.size
  end.reduce(:+)
end

answers = parse_input(read_input)
p part1(answers)
p part2(answers)
