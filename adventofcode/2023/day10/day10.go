package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Pos struct {
	x int
	y int
}

func (p Pos) String() string {
	return fmt.Sprintf("[%d, %d]", p.x, p.y)
}

func (p Pos) neighbors() []Pos {
	return []Pos{
		{x: p.x - 1, y: p.y - 1},
		{x: p.x - 1, y: p.y},
		{x: p.x - 1, y: p.y + 1},
		{x: p.x, y: p.y - 1},
		{x: p.x, y: p.y + 1},
		{x: p.x + 1, y: p.y - 1},
		{x: p.x + 1, y: p.y},
		{x: p.x + 1, y: p.y + 1},
	}
}

type Pipe = rune

const (
	Vertical   Pipe = '|'
	Horizontal      = '-'
	NorthEast       = 'L'
	NorthWest       = 'J'
	SouthWest       = '7'
	SouthEast       = 'F'
	Ground          = '.'
	Start           = 'S'
)

type Grid struct {
	xmax  int
	ymax  int
	loop  []Pos
	start Pos
	pipes map[Pos]Pipe
}

func parseGrid(lines []string) Grid {
	pipes := make(map[Pos]Pipe)
	ymax := len(lines)
	xmax := len(lines[0])
	loop := []Pos{}
	start := Pos{-1, -1}
	for y := range lines {
		for x, t := range lines[y] {
			pos := Pos{x, y}
			if t == Start {
				start = pos
			}
			pipes[pos] = t
		}
	}
	return Grid{xmax, ymax, loop, start, pipes}
}

func (g Grid) String() string {
	var sb strings.Builder
	for y := 0; y < g.ymax; y++ {
		sb.WriteRune('\n')
		for x := 0; x < g.xmax; x++ {
			pos := Pos{x, y}
			sb.WriteRune(g.pipes[pos])
		}
	}
	return sb.String()
}

func (g Grid) inside(pos Pos) bool {
	return pos.x >= 0 && pos.x < g.xmax && pos.y >= 0 && pos.y < g.ymax
}

func (g Grid) connected(p1 Pos, p2 Pos) bool {
	t1 := g.pipes[p1]
	t2 := g.pipes[p2]
	if t1 == Ground || t2 == Ground {
		return false
	}

	switch t1 {
	case Start:
		// S
		switch t2 {
		case Vertical:
			// |
			// S
			// |
			return p2.x == p1.x
		case Horizontal:
			// -S-
			return p2.y == p1.y
		case NorthEast:
			// LS
			//  L
			return (p2.x == p1.x && p2.y == p1.y+1) || (p2.x == p1.x-1 && p2.y == p1.y)
		case NorthWest:
			// SJ
			// J
			return (p2.x == p1.x && p2.y == p1.y+1) || (p2.x == p1.x+1 && p2.y == p1.y)
		case SouthWest:
			// 7
			// S7
			return (p2.x == p1.x && p2.y == p1.y-1) || (p2.x == p1.x+1 && p2.y == p1.y)
		case SouthEast:
			//  F
			// FS
			return (p2.x == p1.x && p2.y == p1.y-1) || (p2.x == p1.x-1 && p2.y == p1.y)
		}
	case Vertical:
		// |
		switch t2 {
		case Vertical:
			// |
			// |
			return p2.x == p1.x
		case Horizontal:
			// |-
			return false
		case NorthEast:
			// |
			// L
			return p2.x == p1.x && p2.y == p1.y+1
		case NorthWest:
			// |
			// J
			return p2.x == p1.x && p2.y == p1.y+1
		case SouthWest:
			// 7
			// |
			return p2.x == p1.x && p2.y == p1.y-1
		case SouthEast:
			// F
			// |
			return p2.x == p1.x && p2.y == p1.y-1
		}
	case Horizontal:
		// -
		switch t2 {
		case Vertical:
			// -|
			return false
		case Horizontal:
			// --
			return p2.y == p1.y
		case NorthEast:
			// L-
			return p2.x == p1.x-1 && p2.y == p1.y
		case NorthWest:
			// -J
			return p2.x == p1.x+1 && p2.y == p1.y
		case SouthWest:
			// -7
			return p2.x == p1.x+1 && p2.y == p1.y
		case SouthEast:
			// F-
			return p2.x == p1.x-1 && p2.y == p1.y
		}
	case NorthEast:
		// L
		switch t2 {
		case Vertical:
			// |
			// L
			return p2.x == p1.x && p2.y == p1.y-1
		case Horizontal:
			// L-
			return p2.x == p1.x+1 && p2.y == p1.y
		case NorthEast:
			return false
		case NorthWest:
			// LJ
			return p2.x == p1.x+1 && p2.y == p1.y
		case SouthWest:
			// 7
			// L7
			return (p2.x == p1.x+1 && p2.y == p1.y) || (p2.x == p1.x && p2.y == p1.y-1)
		case SouthEast:
			// F
			// L
			return p2.x == p1.x && p2.y == p1.y-1
		}
	case NorthWest:
		// J
		switch t2 {
		case Vertical:
			// |
			// J
			return p2.x == p1.x && p2.y == p1.y-1
		case Horizontal:
			// -J
			return p2.x == p1.x-1 && p2.y == p1.y
		case NorthEast:
			// LJ
			return p2.x == p1.x-1 && p2.y == p1.y
		case NorthWest:
			return false
		case SouthWest:
			// 7
			// J
			return p2.x == p1.x && p2.y == p1.y-1
		case SouthEast:
			// FJ
			// J
			return (p2.x == p1.x && p2.y == p1.y-1) || (p2.x == p1.x-1 && p2.y == p1.y)
		}
	case SouthWest:
		// 7
		switch t2 {
		case Vertical:
			// 7
			// |
			return p2.x == p1.x && p2.y == p1.y+1
		case Horizontal:
			// -7
			return p2.x == p1.x-1 && p2.y == p1.y
		case NorthEast:
			// L7
			//  L
			return (p2.x == p1.x && p2.y == p1.y+1) || (p2.x == p1.x-1 && p2.y == p1.y)
		case NorthWest:
			// 7
			// J
			return p2.x == p1.x && p2.y == p1.y+1
		case SouthWest:
			return false
		case SouthEast:
			// F7
			return p2.x == p1.x-1 && p2.y == p1.y
		}
	case SouthEast:
		// F
		switch t2 {
		case Vertical:
			// F
			// |
			return p2.x == p1.x && p2.y == p1.y+1
		case Horizontal:
			// F-
			return p2.x == p1.x+1 && p2.y == p1.y
		case NorthEast:
			// F
			// L
			return p2.x == p1.x && p2.y == p1.y+1
		case NorthWest:
			// FJ
			// J
			return (p2.x == p1.x+1 && p2.y == p1.y) || (p2.x == p1.x && p2.y == p1.y+1)
		case SouthWest:
			// F7
			return p2.x == p1.x+1 && p2.y == p1.y
		case SouthEast:
			return false
		}
	}
	return false
}

func (g Grid) printDists(dists map[Pos]int) string {
	var sb strings.Builder
	for y := 0; y < g.ymax; y++ {
		sb.WriteRune('\n')
		for x := 0; x < g.xmax; x++ {
			pos := Pos{x, y}
			c := dists[pos]
			t := g.pipes[pos]
			if t == Start {
				sb.WriteRune('S')
			} else if c == 0 {
				sb.WriteRune('.')
			} else {
				sb.WriteString(fmt.Sprintf("%c", t))
			}
		}
	}
	return sb.String()
}

func (g *Grid) findLoop() int {
	count := 0
	visited := make(map[Pos]bool)
	for y := 0; y < g.ymax; y++ {
		for x := 0; x < g.xmax; x++ {
			pos := Pos{x, y}
			t := g.pipes[pos]
			if t == Ground {
				continue
			}
			visited[pos] = false
		}
	}

	start := g.start
	stack := []Pos{start}
	for len(stack) > 0 {
		top := stack[0]
		stack = stack[1:]
		if visited[top] {
			continue
		}
		count += 1
		visited[top] = true
		g.loop = append(g.loop, top)

		for _, npos := range top.neighbors() {
			if !visited[npos] && g.inside(npos) && g.connected(top, npos) {
				stack = append([]Pos{npos}, stack...)
			}
		}
	}

	return count / 2
}

func part1(grid *Grid) int {
	return grid.findLoop()
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (g Grid) loopArea() int {
	// Reference https://en.wikipedia.org/wiki/Shoelace_formula
	n := len(g.loop)
	area := 0
	for i := 0; i < n; i++ {
		j := (i + 1) % n
		area += (g.loop[i].x - g.loop[j].x) * (g.loop[i].y + g.loop[j].y)
	}
	return abs(area) / 2
}

func (g Grid) countEnclosed() int {
	// Reference https://nrich.maths.org/5441
	return g.loopArea() - len(g.loop)/2 + 1
}

func part2(grid *Grid) int {
	return grid.countEnclosed()
}

func main() {
	s := time.Now()
	grid := parseGrid(readInput())
	s1 := time.Now()
	fmt.Println(part1(&grid))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(&grid))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
