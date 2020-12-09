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

type Case struct {
	l, w, h int
}

func (c Case) String() string {
	return fmt.Sprintf("%dx%dx%d", c.l, c.w, c.h)
}

func (c Case) SurfaceArea() int {
	return 2 * (c.l*c.w + c.w*c.h + c.h*c.l)
}

func (c Case) Volume() int {
	return c.l * c.w * c.h
}

func (c Case) WrappingPaperSize() int {
	dims := []int{c.l, c.w, c.h}
	sort.Ints(dims)
	return c.SurfaceArea() + dims[0]*dims[1]
}

func (c Case) BowLength() int {
	dims := []int{c.l, c.w, c.h}
	sort.Ints(dims)
	return 2*(dims[0]+dims[1]) + c.Volume()
}

type Input struct {
	cases []Case
}

func parseInput(lines []string) Input {
	r := Input{}
	for _, line := range lines {
		c := Case{}
		dims := strings.Split(line, "x")
		c.l, _ = strconv.Atoi(dims[0])
		c.w, _ = strconv.Atoi(dims[1])
		c.h, _ = strconv.Atoi(dims[2])
		r.cases = append(r.cases, c)
	}
	return r
}

func main() {
	in := parseInput(readInput())
	wrapLength := 0
	bowLength := 0
	for _, c := range in.cases {
		bowLength += c.BowLength()
		wrapLength += c.WrappingPaperSize()
	}
	fmt.Println(wrapLength)
	fmt.Println(bowLength)
}
