# frozen_string_literal: true

def part1(earliest, bus_ids)
  max = bus_ids.max
  max * ((earliest.to_f / max).ceil * max - earliest)
end

def extended_gcd(a, b)
  last_remainder, remainder = a.abs, b.abs
  x, last_x, y, last_y = 0, 1, 1, 0
  until remainder.zero?
    last_remainder, (quotient, remainder) = remainder, last_remainder.divmod(remainder)
    x, last_x = last_x - quotient * x, x
    y, last_y = last_y - quotient * y, y
  end
  return last_remainder, last_x * (a < 0 ? -1 : 1)
end

def invmod(e, et)
  g, x = extended_gcd(e, et)
  raise 'Multiplicative inverse modulo does not exist!' if g != 1

  x % et
end

def chinese_remainder(mods, remainders)
  max = mods.inject( :* )  # product of all moduli
  series = remainders.zip(mods).map{ |r,m| (r * max * invmod(max/m, m) / m) }
  series.inject( :+ ) % max
end

def part2(earliest, bus_ids_raw)
  bus_ids = bus_ids_raw
            .map.with_index { |bid, idx| [bid.to_i, idx] }
            .reject { |bid, _| bid == 0 }
            .map { |bid, idx| [bid, (bid - idx) % bid] }
  chinese_remainder(bus_ids.map(&:first), bus_ids.map(&:last))
end

earliest_timestamp = gets.to_i
bus_ids_raw = gets.chomp.split(',')
bus_ids = bus_ids_raw.reject {|c| c == 'x'}.map(&:to_i)
p part1(earliest_timestamp, bus_ids)
p part2(earliest_timestamp, bus_ids_raw)
