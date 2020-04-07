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
	// fmt.Printf("Case #%d: %s\n", num, out)
}

type Case struct {
	s int
	w []int
}

func (c Case) String() string {
	return fmt.Sprintf("s: %d, ws: %v", c.s, c.w)
}

type Input struct {
	t     int
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{cases: []Case{}}
	r.t, _ = strconv.Atoi(lines[0])
	for i := 1; i <= r.t; i++ {
		ucase := lines[i]
		ucases := strings.Split(ucase, " ")
		icases := []int{}
		for _, uc := range ucases {
			ic, _ := strconv.Atoi(uc)
			icases = append(icases, ic)
		}
		c := Case{
			s: icases[0],
			w: icases[1:],
		}
		r.cases = append(r.cases, c)
	}
	return r
}

func solve(c Case) string {
	attempts := 0
	ssum := 0
	for _, wi := range c.w {
		if ssum+wi > c.s {
			ssum = 0
			attempts++
		}
		ssum += wi
	}
	return fmt.Sprintf("%d", attempts+1)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, solve(c))
	}
}
