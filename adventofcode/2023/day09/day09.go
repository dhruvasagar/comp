package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func readInput() [][]int {
	allnums := [][]int{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		line := scanner.Text()
		numss := strings.Split(line, " ")
		nums := []int{}
		for _, ns := range numss {
			n, _ := strconv.Atoi(ns)
			nums = append(nums, n)
		}
		allnums = append(allnums, nums)
	}
	return allnums
}

func diffs(nums []int) []int {
	res := []int{}
	for i := 1; i < len(nums); i++ {
		res = append(res, nums[i]-nums[i-1])
	}
	return res
}

func next(nums []int) int {
	ndiffs := diffs(nums)
	allZeroes := true
	for _, nd := range ndiffs {
		if nd != 0 {
			allZeroes = false
			break
		}
	}
	if allZeroes {
		return nums[0]
	}

	return nums[len(nums)-1] + next(ndiffs)
}

func part1(allnums [][]int) int {
	res := 0
	for _, nums := range allnums {
		res += next(nums)
	}
	return res
}

func part2(allnums [][]int) int {
	res := 0
	for _, nums := range allnums {
		for i, j := 0, len(nums)-1; i < j; i, j = i+1, j-1 {
			nums[i], nums[j] = nums[j], nums[i]
		}
		res += next(nums)
	}
	return res
}

func main() {
	s := time.Now()
	lines := readInput()
	s1 := time.Now()
	fmt.Println(part1(lines))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(lines))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
