# frozen_string_literal: true

def read_input
  ARGF.read
end

FIELDS = {
  byr: ->(val) { (1920..2002).include?(val.to_i) },
  cid: ->(_) { true },
  ecl: ->(val) { %w[amb blu brn gry grn hzl oth].include?(val) },
  eyr: ->(val) { (2020..2030).include?(val.to_i) },
  hcl: ->(val) { val.match(/^#([\da-f]{6})$/) },
  hgt: lambda { |val|
    m = val.match(/^(\d+)(cm|in)$/)
    return false unless m

    hgt = m[1].to_i
    dim = m[2]
    if dim == 'cm'
      (150..193).include?(hgt)
    else
      (59..76).include?(hgt)
    end
  },
  iyr: ->(val) { (2010..2020).include?(val.to_i) },
  pid: ->(val) { val.match(/^\d{9}$/) }
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
  missing.empty? || (missing - EXCEPTIONS).empty?
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
