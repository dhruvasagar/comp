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

type PriceList []int64

func (p PriceList) Len() int           { return len(p) }
func (p PriceList) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }
func (p PriceList) Less(i, j int) bool { return p[i] < p[j] }

type Case struct {
	n      int64
	prices PriceList
}

func (c Case) String() string {
	return fmt.Sprintf("n: %d, prices: %v", c.n, c.prices)
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
		c.n, _ = strconv.ParseInt(ns, 10, 64)
		ps := strings.Split(lines[i+2], " ")
		for _, psi := range ps {
			psii, _ := strconv.ParseInt(psi, 10, 64)
			c.prices = append(c.prices, psii)
		}
		r.cases = append(r.cases, c)
	}
	return r
}

const MODULO = 1000000007

func (c Case) solve() string {
	s := int64(0)
	sort.Sort(sort.Reverse(c.prices))
	for i, p := range c.prices {
		pr := p - int64(i)
		if pr < 0 {
			pr = 0
		}
		s += pr % MODULO
	}
	return fmt.Sprintf("%d", s%MODULO)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
