package main

import (
	"fmt"
)

const (
	left = iota
	right
)

func stringShift(str string, shift [][]int) string {
	for i := 0; i < len(shift); i++ {
		s := shift[i]
		d, a := s[0], s[1]
		if d == left {
			for j := 0; j < a; j++ {
				str = str[1:] + str[0:1]
			}
		} else {
			for j := 0; j < a; j++ {
				str = str[len(str)-1:] + str[:len(str)-1]
			}
		}
	}
	return str
}

func main() {
	fmt.Println(stringShift("abc", [][]int{[]int{0, 1}, []int{1, 2}}))
	fmt.Println(stringShift("abcdefg", [][]int{[]int{1, 1}, []int{1, 1}, []int{0, 2}, []int{1, 3}}))
}
