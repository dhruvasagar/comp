package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Operator = string

const (
	and    = "AND"
	or     = "OR"
	lshift = "LSHIFT"
	rshift = "RSHIFT"
	not    = "NOT"
)

type Signaler interface {
	Output() uint16
}

type Signal uint16

func (s Signal) Output() uint16 {
	return uint16(s)
}

type Wire struct {
	id  string
	src Signaler
}

func (w Wire) Output() uint16 {
	return w.src.Output()
}

type Gate struct {
	operator Operator

	in []Wire
}

func (g Gate) Output() uint16 {
	switch g.operator {
	case and:
		return g.in[0].Output() & g.in[1].Output()
	case or:
		return g.in[0].Output() | g.in[1].Output()
	case lshift:
		return g.in[0].Output() << g.in[1].Output()
	case rshift:
		return g.in[0].Output() >> g.in[1].Output()
	case not:
		return ^g.in[0].Output()
	}
	return 0
}

type Circuit struct {
	wires []Wire
}

func parseInput(lines []string) Circuit {
	r := Circuit{}
	// for _, line := range lines {
	// 	ws := strings.Split(line, " -> ")
	// }
	return r
}

func main() {
	in := parseInput(readInput())
	fmt.Println(in)
}
