def part1 ls, rs
  ls.each.with_index.reduce(0) {|r, (l, i)| r + (l - rs[i]).abs}
end

def part2 ls, rs
  ls.reduce(0) {|r, l| r + l * rs.count(l)}
end

nums = readlines.map(&:split)
ls, rs, *_ = nums.transpose.map {|x| x.map(&:to_i)}
p part1 ls.sort, rs.sort
p part2 ls, rs
