package main

import (
	"fmt"
	"sort"
)

func countElements(arr []int) int {
	sort.Ints(arr)
	memory := make(map[int]bool)
	for _, a := range arr {
		memory[a] = true
	}
	res := 0
	for _, a := range arr {
		if _, ok := memory[a+1]; ok {
			res++
		}
	}
	return res
}

func main() {
	fmt.Println(countElements([]int{1, 2, 3}))
	fmt.Println(countElements([]int{1, 1, 3, 3, 5, 5, 7, 7}))
	fmt.Println(countElements([]int{1, 3, 2, 3, 5, 0}))
	fmt.Println(countElements([]int{1, 1, 2, 2}))
}
