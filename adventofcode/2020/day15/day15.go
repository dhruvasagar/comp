package main

import (
	"fmt"
	"strconv"
	"strings"
)

const (
	part1Limit = 2020
	part2Limit = 30000000
)

func parseInput(in string) []int32 {
	r := []int32{}
	ints := strings.Split(in, ",")
	for _, ini := range ints {
		i, _ := strconv.Atoi(ini)
		r = append(r, int32(i))
	}
	return r
}

func nextNumber(num, index int, numbers []int, ph, lh map[int]int) int {
	if index < len(numbers) {
		num = numbers[index]
		ph[num] = index
		return num
	}

	if n, ok := lh[num]; ok {
		nnum := n - ph[num]
		if _, ok = ph[nnum]; ok {
			lh[nnum] = index
		} else {
			ph[nnum] = index
		}
		ph[num] = index - 1
		num = nnum
	} else {
		num = 0
		lh[num] = index
	}
	return num
}

func part(nums []int, limit int) int {
	index := 0
	lh := make(map[int]int)
	ph := make(map[int]int)
	num := nums[0]
	for {
		num = nextNumber(num, index, nums, ph, lh)
		if index+1 == limit {
			return num
		}

		index++
	}
}

func fastPart(nums []int32, limit int32) string {
	var turn int32 = 0
	var num int32 = 0
	var lomemSize int32 = 10000000
	mem := map[int32]int32{}
	lomem := make([]int32, lomemSize)

	for ; turn < int32(len(nums)); turn++ {
		num = nums[turn]
		lomem[num] = turn + 1
	}

	for ; turn != limit; turn++ {
		var m int32
		var ok bool
		if num < lomemSize {
			m = lomem[num]
			ok = m != 0
			lomem[num] = turn
		} else {
			m, ok = mem[num]
			mem[num] = turn
		}

		if !ok {
			num = 0
		} else {
			num = turn - m
		}
	}

	return fmt.Sprint(num)
}

func main() {
	var input string
	fmt.Scanln(&input)
	nums := parseInput(input)
	fmt.Println(fastPart(nums, part1Limit))
	fmt.Println(fastPart(nums, part2Limit))
}
