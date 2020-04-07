package main

import (
	"bufio"
	"fmt"
	"math"
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
	fmt.Println(out)
}

type Case struct {
	x, k int64
}

func (c Case) String() string {
	return fmt.Sprintf("x: %d, k: %d", c.x, c.k)
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	for i := 0; i < r.t; i++ {
		c := Case{}
		xka := strings.Split(lines[i+1], " ")
		c.x, _ = strconv.ParseInt(xka[0], 10, 64)
		c.k, _ = strconv.ParseInt(xka[1], 10, 64)
		r.cases = append(r.cases, c)
	}
	return r
}

func (c Case) solve() string {
	if c.x == int64(math.Pow(2, float64(c.k))) {
		return fmt.Sprintf("%d", 1)
	}
	return fmt.Sprintf("%d", 0)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
