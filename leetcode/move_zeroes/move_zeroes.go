package main

import "fmt"

func moveZeroes(nums []int) {
	ptr := 0
	for _, n := range nums {
		if n != 0 {
			nums[ptr] = n
			ptr++
		}
	}
	for i := ptr; i < len(nums); i++ {
		nums[i] = 0
	}
	fmt.Println(nums)
}

func main() {
	moveZeroes([]int{0, 0, 1})
	moveZeroes([]int{0, 1, 0, 3, 12})
}
