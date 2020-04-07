package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
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

type Activity struct {
	start int
	end   int
	index int
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

func (i Activity) hasOverlap(i2 Activity) bool {
	return max(i.start, i2.start) < min(i.end, i2.end)
}

type Schedule []Activity

// Sort Interface
func (s Schedule) Len() int           { return len(s) }
func (s Schedule) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }
func (s Schedule) Less(i, j int) bool { return s[i].start < s[j].start }

func (s Schedule) hasOverlap(i Activity) bool {
	if len(s) == 0 {
		return false
	}
	return s[len(s)-1].hasOverlap(i)
}

type Case struct {
	n        int
	schedule Schedule
}

func (c Case) assignActivities() string {
	cs := Schedule{}
	js := Schedule{}
	sort.Sort(c.schedule)
	as := make([]string, len(c.schedule))

	for _, s := range c.schedule {
		if cs.hasOverlap(s) {
			if js.hasOverlap(s) {
				return "IMPOSSIBLE"
			}
			js = append(js, s)
			as[s.index] = "J"
		} else {
			cs = append(cs, s)
			as[s.index] = "C"
		}
	}
	return strings.Join(as, "")
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
			activity := Activity{}
			activity.start, _ = strconv.Atoi(isa[0])
			activity.end, _ = strconv.Atoi(isa[1])
			activity.index = j
			c.schedule = append(c.schedule, activity)
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
