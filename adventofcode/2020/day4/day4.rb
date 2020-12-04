# frozen_string_literal: true

def read_input
  ARGF.read
end

FIELDS = {
  byr: lambda { |val|
    val = val.to_i
    val > 999 && val >= 1920 && val <= 2002
  },
  cid: ->(_) { true },
  ecl: ->(val) { %w[amb blu brn gry grn hzl oth].include?(val) },
  eyr: lambda { |val|
    val = val.to_i
    val > 999 && val >= 2020 && val <= 2030
  },
  hcl: ->(val) { val.match(/#([\da-f]{6})/) },
  hgt: lambda { |val|
    m = val.match(/^(\d+)(cm|in)/)
    return false unless m

    hgt = m[1].to_i
    dim = m[2]
    if dim == 'cm'
      hgt >= 150 && hgt <= 193
    else
      hgt >= 59 && hgt <= 76
    end
  },
  iyr: lambda { |val|
    val = val.to_i
    val > 999 && val >= 2010 && val <= 2020
  },
  pid: ->(val) { val.size == 9 }
}.freeze

EXCEPTIONS = [
  'cid'
].freeze

def parse_input(passports)
  passports.split("\n\n")
end

def parse_passport(passport)
  passport.split.map { |p| p.split(':').map(&:strip) }.to_h
end

def valid_passport?(passport)
  missing = FIELDS.keys.map(&:to_s) - passport.keys.sort
  missing.empty? || missing == EXCEPTIONS
end

def valid_passport2?(passport)
  valid_passport?(passport) && passport.all? do |k, v|
    FIELDS[k.to_sym].call(v)
  end
end

def part1(passports)
  parse_input(passports).select do |passport|
    valid_passport?(parse_passport(passport))
  end.size
end

def part2(passports)
  parse_input(passports).select do |passport|
    valid_passport2?(parse_passport(passport))
  end.size
end

passports = read_input
p part1(passports)
p part2(passports)
