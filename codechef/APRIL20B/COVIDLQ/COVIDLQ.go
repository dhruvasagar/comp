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
	fmt.Println(out)
}

type Case struct {
	n     int
	queue []int
}

func (c Case) String() string {
	return fmt.Sprintf("n: %d, queueu: %v", c.n, c.queue)
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	for i := 0; i < r.t*2; i += 2 {
		c := Case{}
		ns := lines[i+1]
		c.n, _ = strconv.Atoi(ns)
		as := strings.Split(lines[i+2], " ")
		for _, asi := range as {
			asii, _ := strconv.Atoi(asi)
			c.queue = append(c.queue, asii)
		}
		r.cases = append(r.cases, c)
	}
	return r
}

func (c Case) solve() string {
	ptr := -1
	for i, q := range c.queue {
		if q == 1 {
			if ptr >= 0 && i-ptr < 6 {
				return "NO"
			}
			ptr = i
		}
	}
	return "YES"
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
