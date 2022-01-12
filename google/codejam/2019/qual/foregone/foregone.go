package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

type Input struct {
	t int
	n []string
}

func parseInput(in []string) (r Input) {
	r.t, _ = strconv.Atoi(in[0])
	for i := 0; i < r.t; i++ {
		r.n = append(r.n, in[i+1])
	}
	return r
}

func solve(n string) (r [2]string) {
	a, b := "", ""
	for _, c := range n {
		if string(c) == "4" {
			a += "1"
			b += "3"
		} else {
			if len(a) > 0 {
				a += "0"
			}
			b += string(c)
		}
	}
	r[0] = a
	r[1] = b
	return r
}

func main() {
	in := parseInput(readInput())
	for i := 0; i < in.t; i++ {
		r := solve(in.n[i])
		logCase(i+1, fmt.Sprintf("%s %s", r[0], r[1]))
	}
}
