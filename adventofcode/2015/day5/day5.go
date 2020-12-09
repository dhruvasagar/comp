package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func isNice(str string) bool {
	badStrings := []string{"ab", "cd", "pq", "xy"}
	for _, bs := range badStrings {
		if strings.Contains(str, bs) {
			return false
		}
	}

	vowels := "aeiou"
	vowelCount := 0
	repeatCount := 1
	for i, c := range str {
		if strings.ContainsRune(vowels, c) {
			vowelCount++
		}
		if i > 0 && rune(str[i-1]) == c {
			repeatCount++
		}
		if vowelCount >= 3 && repeatCount >= 2 {
			return true
		}
	}
	return false
}

func part1(strings []string) int {
	count := 0
	for _, str := range strings {
		if isNice(str) {
			count++
		}
	}
	return count
}

func isReallyNice(str string) bool {
	index := 0
	repeatSize := 2
	pairNice := false
	palinNice := false
	for index+repeatSize < len(str) {
		sstr := str[index : index+repeatSize]
		if !pairNice && strings.Contains(str[index+repeatSize:], sstr) {
			pairNice = true
		}
		if !palinNice && index+2 < len(str) {
			palinNice = str[index] == str[index+2]
		}
		if pairNice && palinNice {
			return true
		}
		index++
	}
	return false
}

func part2(strings []string) int {
	count := 0
	for _, str := range strings {
		if isReallyNice(str) {
			count++
		}
	}
	return count
}

func main() {
	strings := readInput()
	fmt.Println(part1(strings))
	fmt.Println(part2(strings))
}
