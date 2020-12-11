# frozen_string_literal: true

def read_input
  ARGF.readlines
end

FLOOR = '.'
EMPTY = 'L'
OCCUPIED = '#'
DIRECTIONS = [
  [-1, 0],
  [1, 0],
  [0, -1],
  [0, 1],
  [-1, -1],
  [1,  1],
  [-1, 1],
  [1, -1]
].freeze

def floor?(seat)
  seat == FLOOR
end

def empty?(seat)
  seat == EMPTY
end

def occupied?(seat)
  seat == OCCUPIED
end

def inside?(seat_map, i, j)
  ymax = seat_map.size
  xmax = seat_map[0].size
  i >= 0 && i < ymax && j >= 0 && j < xmax
end

def adjacent(seat_map, i, j)
  [
    [i-1,j-1],
    [i,j-1],
    [i+1,j-1],
    [i-1,j],
    [i+1,j],
    [i-1,j+1],
    [i,j+1],
    [i+1,j+1]
  ].select { |ai, aj| inside?(seat_map, ai, aj) }
end

def empty_adjacent?(seat_map, i, j)
  return false unless empty?(seat_map[i][j])

  adjacent(seat_map, i, j)
    .map { |ai, aj| seat_map[ai][aj] }
    .all? { |s| empty?(s) || floor?(s) }
end

def empty_adjacent2?(seat_map, i, j)
  return false unless empty?(seat_map[i][j])

  DIRECTIONS.all? do |d|
    si = i
    sj = j
    bool = true
    loop do
      si += d[0]
      sj += d[1]
      break unless inside?(seat_map, si, sj)

      seat = seat_map[si][sj]
      break if empty?(seat)

      if occupied?(seat)
        bool = false
        break
      end
    end
    bool
  end
end

def should_die?(seat_map, i, j)
  return false unless occupied?(seat_map[i][j])

  adjacent(seat_map, i, j)
    .map { |ai, aj| seat_map[ai][aj] }
    .select { |s| occupied?(s) }
    .size >= 4
end

def should_die2?(seat_map, i, j)
  return false unless occupied?(seat_map[i][j])

  DIRECTIONS.map do |d|
    si = i
    sj = j
    seat = nil
    loop do
      si += d[0]
      sj += d[1]
      break unless inside?(seat_map, si, sj)

      seat = seat_map[si][sj]
      break unless floor?(seat)
    end
    seat
  end
    .compact
    .select {|s| occupied?(s)}
    .size >= 5
end

def count_occupied(seat_map)
  seat_map.flatten.select { |s| occupied?(s) }.size
end


def apply_rules(seat_map)
  nsm = seat_map.map(&:dup)

  nsm.each.with_index do |sr, i|
    sr.each_index do |j|
      seat = nsm[i][j]

      seat_map[i][j] = OCCUPIED if empty?(seat) && empty_adjacent?(nsm, i, j)
      seat_map[i][j] = EMPTY if occupied?(seat) && should_die?(nsm, i, j)
    end
  end
end

def apply_rules2(seat_map)
  nsm = seat_map.map(&:dup)

  nsm.each.with_index do |sr, i|
    sr.each_index do |j|
      seat = nsm[i][j]

      seat_map[i][j] = OCCUPIED if empty?(seat) && empty_adjacent2?(nsm, i, j)
      seat_map[i][j] = EMPTY if occupied?(seat) && should_die2?(nsm, i, j)
    end
  end
end

def show_seat_map(seat_map)
  seat_map.each do |sr|
    sr.each do |s|
      print s
    end
    puts
  end
  puts
end

def part1(seat_map)
  nsm = seat_map.map(&:dup)
  last_count = -1
  while true
    apply_rules(nsm)
    new_count = count_occupied(nsm)
    break if last_count == new_count

    last_count = new_count
  end
  last_count
end

def part2(seat_map)
  nsm = seat_map.map(&:dup)
  last_count = -1
  while true
    apply_rules2(nsm)
    new_count = count_occupied(nsm)
    break if last_count == new_count

    last_count = new_count
  end
  last_count
end

def parse_input(seat_data)
  seat_data.map { |s| s.chomp.chars }
end

seat_data = read_input
seat_map = parse_input(seat_data)
p part1(seat_map)
p part2(seat_map)
