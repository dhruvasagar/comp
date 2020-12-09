package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

const (
	PreambleSize = 25
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func silver(nums []int) string {
	start := PreambleSize
	for i := start; i < len(nums); i++ {
		num := nums[i]
		for j := PreambleSize - i; j < i; j++ {
			num2 := nums[j]
		}
	}
}

func gold(nums []int) string {
	return ""
}

func parseInput(lines []string) []int {
	r := []int{}
	for _, line := range lines {
		ri, _ := strconv.Atoi(line)
		r = append(r, ri)
	}
	return r
}

func main() {
	in := parseInput(readInput())
	fmt.Println(silver(in))
	fmt.Println(gold(in))
}
