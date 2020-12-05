# frozen_string_literal: true

def read_input
  ARGF.readlines
end

ROW_MULTIPLIER = 8

def bpart(str, min, max, fstr, lstr)
  str.scan(/./).each do |s|
    if s == fstr
      max = (min + max) / 2
    else
      min = (min + max) / 2 + 1
    end
  end
  min
end

def row(rstr)
  bpart(rstr, 0, 127, 'F', 'B')
end

def col(cstr)
  bpart(cstr, 0, 7, 'L', 'R')
end

def seat_id(seat)
  rstr = seat[0..6]
  cstr = seat[7..-1]
  row(rstr) * ROW_MULTIPLIER + col(cstr)
end

def part1(seats)
  seats.map { |seat| seat_id(seat.chomp) }.max
end

def part2(seats)
  all_seats = seats.map { |seat| seat_id(seat) }
  ((all_seats.min..all_seats.max).to_a - all_seats).first
end

seats = read_input
p part1(seats)
p part2(seats)
