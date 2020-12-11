package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func parseInput(lines []string) {
	fmt.Println()
}

func main() {
	parseInput(readInput())
}
