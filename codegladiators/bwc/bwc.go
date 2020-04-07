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
	fmt.Println(out)
}

type BIntArray []int64

func (b BIntArray) Len() int           { return len(b) }
func (b BIntArray) Swap(i, j int)      { b[i], b[j] = b[j], b[i] }
func (b BIntArray) Less(i, j int) bool { return b[i] < b[j] }

type Case struct {
	n       int
	gpowers BIntArray
	opowers BIntArray
}

func (c Case) String() string {
	return fmt.Sprintf("n: %d, gpowers: %v, opowers: %v", c.n, c.gpowers, c.opowers)
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.Atoi(lines[0])
	for i := 0; i < r.t*3; i += 3 {
		c := Case{}
		c.n, _ = strconv.Atoi(lines[i+1])
		gps := strings.Split(lines[i+2], " ")
		ops := strings.Split(lines[i+3], " ")
		for i := 0; i < c.n; i++ {
			gpi, _ := strconv.ParseInt(gps[i], 10, 64)
			c.gpowers = append(c.gpowers, gpi)
			opi, _ := strconv.ParseInt(ops[i], 10, 64)
			c.opowers = append(c.opowers, opi)
		}
		r.cases = append(r.cases, c)
	}
	return r
}

func (c Case) solve() string {
	faught := make(map[int]bool)
	sort.Sort(c.gpowers)
	sort.Sort(sort.Reverse(c.opowers))
	for i := 0; i < c.n; i++ {
		gp := c.gpowers[i]
		for j := 0; j < c.n; j++ {
			op := c.opowers[j]
			if _, ok := faught[j]; ok {
				continue
			}
			if gp > op {
				faught[j] = true
				break
			}
		}
	}
	cnt := 0
	for range faught {
		cnt++
	}
	return fmt.Sprintf("%d", cnt)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
