def lastStoneWeight(stones)
  return 0 if stones.length == 0
  return stones[0] if stones.length == 1

  stones = stones.sort.reverse
  if stones[0] == stones[1]
    stones.slice!(0..1)
  else
    stones[0] = stones[0] - stones[1]
    stones = stones[0...1] + stones[2..-1] 
  end
  p stones
  lastStoneWeight(stones)
end

p lastStoneWeight([2, 2])
p lastStoneWeight([2, 7, 4, 1, 8, 1])
