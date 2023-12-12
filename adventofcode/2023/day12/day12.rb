# frozen_string_literal: true

def read_input
  ARGF.readlines
end

@cache = {}
def count(line, records, cur_size, idx, ridx)
  key = [line, records, cur_size, idx, ridx].hash
  return @cache[key] if @cache.key?(key)

  lsize = line.size
  rsize = records.size
  @cache[key] = if idx == line.size
    rsize == ridx ? 1 : 0
  elsif line[idx] == '#'
    count(line, records, cur_size + 1, idx+1, ridx)
  elsif line[idx] == '.' || ridx == rsize
    if ridx < rsize && cur_size == records[ridx]
      count(line, records, 0, idx + 1, ridx + 1)
    elsif cur_size == 0
      count(line, records, 0, idx + 1, ridx)
    else
      0
    end
  else
    damaged_count = count(line, records, cur_size + 1, idx + 1, ridx)
    operational_count = if cur_size == records[ridx]
      count(line, records, 0, idx + 1, ridx + 1)
    elsif cur_size == 0
      count(line, records, 0, idx + 1, ridx)
    else
      0
    end
    damaged_count + operational_count
  end
  return @cache[key]
end

lines = read_input.map(&:chomp)
p lines.reduce(0) { |s, line|
  springs, record = line.split
  records = record.split(',').map(&:to_i)
  s + count("#{springs}.", records, 0, 0, 0)
}

p lines.reduce(0) {|s, line|
  springs, record = line.split
  records = record.split(',').map(&:to_i) * 5
  s + count("#{([springs] * 5).join("?")}.", records, 0, 0, 0)
}
