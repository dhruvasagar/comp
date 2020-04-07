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

type Interval struct {
	start, end int
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func (i Interval) hasOverlap(i2 Interval) bool {
	return i.start == i2.start || i.end == i2.end ||
		(i.start < i2.start && i.end > i2.start) ||
		(i2.start < i.start && i2.end > i.start)
}

type Schedule []Interval

func (s Schedule) hasOverlap(i Interval) bool {
	for _, sc := range s {
		if sc.hasOverlap(i) {
			return true
		}
	}
	return false
}

type Case struct {
	n        int
	schedule Schedule
}

func (c Case) assignActivities() string {
	as := ""
	cs := Schedule{}
	js := Schedule{}
	for _, s := range c.schedule {
		if cs.hasOverlap(s) {
			if js.hasOverlap(s) {
				return "IMPOSSIBLE"
			}
			js = append(js, s)
			as += "J"
		} else {
			cs = append(cs, s)
			as += "C"
		}
	}
	return as
}

func (c Case) String() string {
	return c.assignActivities()
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
		for j := 0; j < c.n; j++ {
			is := lines[i+2]
			isa := strings.Split(is, " ")
			interval := Interval{}
			interval.start, _ = strconv.Atoi(isa[0])
			interval.end, _ = strconv.Atoi(isa[1])
			c.schedule = append(c.schedule, interval)
			i++
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
