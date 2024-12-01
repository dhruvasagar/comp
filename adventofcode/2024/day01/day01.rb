nums = readlines.map(&:split)
lnums, rnums = nums.map(&:first).map(&:to_i).sort, nums.map(&:last).map(&:to_i).sort
p lnums.map.with_index {|lnum, i| (lnum - rnums[i]).abs}.sum
p lnums.map {|lnum| lnum * rnums.count(lnum)}.sum
