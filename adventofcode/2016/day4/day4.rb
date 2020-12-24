# frozen_string_literal: true

ROOM_REGEX = /^([\w\-]+?)(\d+)\[(\w+)\]$/.freeze

def valid?(rn)
  lch = {}
  rn[:ename].chars.each do |r|
    lch[r] ||= { cnt: 0 }
    lch[r][:cnt] += 1
    lch[r][:ord] = ['z', r].map(&:ord).reduce(:-)
  end
  rn[:ename].chars.uniq.sort_by { |c| [lch[c][:cnt], lch[c][:ord]] }
            .reverse.take(5).join == rn[:checksum]
end

def part1(room_names)
  room_names
    .select { |rn| valid?(rn) }
    .map { |rn| rn[:sectorID] }
    .sum
end

def decrypt(rn)
  a = 'a'.ord
  shift = rn[:sectorID] % 26
  rn[:oname].chars.map(&:ord).map do |c|
    next c.chr if c == ' '.ord

    (c + shift - a) >= 26 ? (a + (c + shift - a) % 26).chr : (c + shift).chr
  end.join.strip
end

def part2(room_names)
  rn = room_names.detect { |rn| decrypt(rn) =~ /north/ }
  rn && rn[:sectorID]
end

def read_input
  ARGF.readlines
end

def parse_input(lines)
  lines.map do |l|
    m = ROOM_REGEX.match(l.chomp)
    {
      ename: m[1].gsub('-', ''),
      oname: m[1].gsub('-', ' '),
      sectorID: m[2].to_i,
      checksum: m[3]
    }
  end
end

room_names = parse_input(read_input)
p part1(room_names)
p part2(room_names)
