package main

import (
	"fmt"
	"sort"
)

func lastStoneWeight(stones []int) int {
	if len(stones) == 0 {
		return 0
	}
	if len(stones) == 1 {
		return stones[0]
	}

	sort.Slice(stones, func(i, j int) bool { return stones[i] > stones[j] })
	if stones[0] == stones[1] {
		stones = stones[2:]
	} else {
		stones[0] = stones[0] - stones[1]
		stones = append(stones[:1], stones[2:]...)
	}

	return lastStoneWeight(stones)
}

func main() {
	fmt.Println(lastStoneWeight([]int{2, 2}))
	fmt.Println(lastStoneWeight([]int{2, 7, 4, 1, 8, 1}))
}
