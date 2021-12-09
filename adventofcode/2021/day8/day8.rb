def read_input
  ARGF.readlines
end

def nmap(sigmap)
  one = sigmap[2][0]
  seven = sigmap[3][0]
  four = sigmap[4][0]
  eight = sigmap[7][0]
  map = {
    one => 1,
    seven => 7,
    four => 4,
    eight => 8
  }

  three = sigmap[5].find { |s| (s.chars & one.chars) == one.chars } # 3
  map[three] = 3

  sigmap[5] = sigmap[5] - [three]
  two = sigmap[5].find { |s| (s.chars & four.chars).size == 2 }
  map[two] = 2

  five = (sigmap[5] - [two])[0]
  map[five] = 5

  six = sigmap[6].find { |s| (s.chars & one.chars) != one.chars } # 6
  map[six] = 6
  sigmap[6] = sigmap[6] - [six]

  nine = sigmap[6].find { |s| (s.chars & three.chars) == three.chars }
  map[nine] = 9

  zero = (sigmap[6] - [nine])[0]
  map[zero] = 0
  map
end

r = read_input.map do |line|
  sigs, digs = line.split(' | ').map { |s| s.split.map { |ss| ss.chars.sort.join } }
  sigmap = sigs.group_by(&:size)
  nh = nmap(sigmap)
  digs.map { |ds| nh[ds] }.join.to_i
end.reduce(:+)
p r
