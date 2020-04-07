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
	s    string
	nums []int
}

func nParens(opening bool, n int) string {
	s := ""
	paren := "("
	if !opening {
		paren = ")"
	}
	for i := 0; i < n; i++ {
		s += paren
	}
	return s
}

func (c Case) minParens() string {
	s := ""
	ocnt := 0
	for i, n := range c.nums {
		s += nParens(true, n-ocnt) + fmt.Sprintf("%d", n)
		ocnt = n
		cn := 0
		if i == len(c.nums)-1 {
			cn = n
		} else {
			cn = n - c.nums[i+1]
		}
		s += nParens(false, cn)
	}
	return s
}

func (c Case) String() string {
	return fmt.Sprint(c.minParens())
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
		c.s = lines[i+1]
		ns := strings.Split(c.s, "")
		for _, nsi := range ns {
			num, _ := strconv.Atoi(nsi)
			c.nums = append(c.nums, num)
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
