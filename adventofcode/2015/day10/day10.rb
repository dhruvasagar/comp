# frozen_string_literal: true

class String
  def look_and_say
    gsub(/(.)\1*/) { |s| s.size.to_s + s[0] }
  end
end

def part1(digits)
  40.times { digits = digits.look_and_say }
  digits.size
end

def part2(digits)
  50.times { digits = digits.look_and_say }
  digits.size
end

digits = gets.chomp
p part1(digits)
p part2(digits)
