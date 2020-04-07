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
	n      int
	matrix [][]int
}

func (c Case) trace() int {
	s := 0
	for i := 0; i < c.n; i++ {
		s += c.matrix[i][i]
	}
	return s
}

func hasRepetition(ns []int) bool {
	visited := make(map[int]int)
	for _, n := range ns {
		if _, ok := visited[n]; ok {
			return true
		}
		visited[n]++
	}
	return false
}

func (c Case) numRowWithRepetition() int {
	s := 0
	for i := 0; i < c.n; i++ {
		if hasRepetition(c.matrix[i]) {
			s++
		}
	}
	return s
}

func (c Case) numColWithRepetition() int {
	s := 0
	for i := 0; i < c.n; i++ {
		col := []int{}
		for j := 0; j < c.n; j++ {
			col = append(col, c.matrix[j][i])
		}
		if hasRepetition(col) {
			s++
		}
	}
	return s
}

func (c Case) String() string {
	// return fmt.Sprintf("Case{n: %d, matrix: %v}", c.n, c.matrix)
	return fmt.Sprintf("%d %d %d", c.trace(), c.numRowWithRepetition(), c.numColWithRepetition())
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	for i := 0; i < len(lines)-1; i++ {
		c := Case{}
		c.n, _ = strconv.Atoi(lines[i+1])
		c.matrix = make([][]int, c.n)
		for j := 0; j < c.n; j++ {
			ns := lines[i+2]
			nsa := strings.Split(ns, " ")
			for _, nsai := range nsa {
				l, _ := strconv.Atoi(nsai)
				c.matrix[j] = append(c.matrix[j], l)
			}
			i += 1
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
	for i, c := range in.cases {
		logCase(i+1, solve(c))
	}
}
