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
	n                    int64
	minIngredients       []int64
	availableIngredients []int64
}

func (c Case) String() string {
	return fmt.Sprintf("minIngredients: %v, availableIngredients: %v", c.minIngredients, c.availableIngredients)
}

type Input struct {
	t     int64
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	r.t, _ = strconv.ParseInt(lines[0], 10, 64)
	c := Case{n: r.t}
	mis := strings.Split(lines[1], " ")
	ais := strings.Split(lines[2], " ")
	for i := int64(0); i < r.t; i++ {
		mi, _ := strconv.ParseInt(mis[i], 10, 64)
		ai, _ := strconv.ParseInt(ais[i], 10, 64)
		c.minIngredients = append(c.minIngredients, mi)
		c.availableIngredients = append(c.availableIngredients, ai)
	}
	r.cases = append(r.cases, c)
	return r
}

func (c Case) solve() string {
	minQuantity := int64(0)
	for i := int64(0); i < c.n; i++ {
		mi := c.minIngredients[i]
		ai := c.availableIngredients[i]
		q := ai / mi
		if minQuantity == 0 || minQuantity > q {
			minQuantity = q
		}
	}
	return fmt.Sprintf("%d", minQuantity)
}

func main() {
	in := parseInput(readInput())
	for i, c := range in.cases {
		logCase(i+1, c.solve())
	}
}
