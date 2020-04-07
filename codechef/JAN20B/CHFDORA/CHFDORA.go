package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

func logCase(num int, out string) {
	fmt.Printf("Case #%d: %s\n", num, out)
}

type Case struct {
	m, n int
	grid [][]int
}

func (c Case) String() string {
	return fmt.Sprintf("m=%d, n=%d, grid=%v", c.m, c.n, c.grid)
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	ii := 1
	for i := 1; i <= r.t; i++ {
		c := Case{}
		mns := strings.Split(lines[i], " ")
		c.m, _ = strconv.Atoi(mns[0])
		c.n, _ = strconv.Atoi(mns[1])
		c.grid = make([][]int, c.n)
		for j := 0; j < c.n; j++ {
			c.grid[j] = make([]int, c.m)
			ms := strings.Split(lines[i+j+1], " ")
			for k := 0; k < c.m; k++ {
				msi, _ := strconv.Atoi(ms[k])
				c.grid[j][k] = msi
			}
		}
		r.cases = append(r.cases, c)
	}
	return r
}

func solve(c Case) string {
	return fmt.Sprintf("%s", c)
}

func main() {
	in := parseInput(readInput())
	fmt.Println(in)
	for i, c := range in.cases {
		logCase(i+1, solve(c))
	}
}
