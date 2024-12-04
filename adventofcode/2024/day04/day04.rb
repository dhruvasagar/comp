def count_word grid, word, pos
  x, y = pos
  wsize = word.size
  ymax, xmax = grid.size, grid[0].size

  return 0 if grid[y][x] != word[0]

  count = 0
  # horizontal forward search
  count += 1 if x + wsize - 1 < xmax && grid[y][x..(x+wsize-1)] == word
  # horizontal backward search
  count += 1 if x - wsize + 1 >= 0 && grid[y][(x-wsize+1)..x].reverse == word
  # vertical forward search
  count += 1 if y + wsize - 1 < ymax && grid[y..(y+wsize-1)].map {|row| row[x]} == word
  # vertical backward search
  count += 1 if y - wsize + 1 >= 0 && grid[(y-wsize+1)..y].map {|row| row[x]}.reverse == word
  # diagonal forward search \
  count += 1 if (x + wsize - 1 < xmax && y + wsize - 1 < ymax) &&
                grid[y..(y+wsize-1)].map.with_index {|row, idx| row[x+idx]} == word
  # diagonal forward mirror search /
  count += 1 if (x - wsize + 1 >= 0 && y + wsize - 1 < ymax) &&
                grid[y..(y+wsize-1)].map.with_index {|row, idx| row[x-idx]} == word
  # diagonal backward search \
  count += 1 if (x - wsize + 1 >= 0 && y - wsize + 1 >= 0) &&
                grid[(y-wsize+1)..y].map.with_index {|row, idx| row[x-wsize+1+idx]}.reverse == word
  # diagonal backward mirror search /
  count += 1 if (x + wsize - 1 < xmax && y - wsize + 1 >= 0) &&
                grid[(y-wsize+1)..y].map.with_index {|row, idx| row[x+wsize-1-idx]}.reverse == word

  count
end

def part1 grid, word
  ymax, xmax = grid.size, grid[0].size

  res = 0
  ymax.times do |y|
    xmax.times do |x|
      res += count_word grid, word, [x, y]
    end
  end
  res
end

def match_xword grid, apos, word
  x, y = apos
  ymax, xmax = grid.size, grid[0].size
  return false if x == 0 || y == 0 || x == xmax-1 || y == ymax-1
  diag1 = grid[(y-1)..(y+1)].map.with_index {|row, idx| row[x-1+idx]}
  diag2 = grid[(y-1)..(y+1)].map.with_index {|row, idx| row[x+1-idx]}
  return (diag1 == word || diag1.reverse == word) &&
         (diag2 == word || diag2.reverse == word)
end

def part2 grid, word
  res = 0
  grid.each_with_index do |row, y|
    row.each_with_index do |char, x|
      next if char != 'A'
      res += 1 if match_xword grid, [x, y], word
    end
  end
  res
end

grid = readlines.map {|line| line.chomp.chars}
p part1 grid, "XMAS".chars
p part2 grid, "MAS".chars
