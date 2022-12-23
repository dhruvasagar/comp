package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

type Point struct {
	x, y int
}

type Value = rune

const (
	Empty = '.'
	Elf   = '#'
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func parseInput(lines []string) map[Point]Value {
	res := make(map[Point]Value)
	for y, line := range lines {
		for x, c := range line {
			point := Point{x, y}
			res[point] = c
		}
	}
	return res
}

func printGrid(grid map[Point]Value) {
	xmin := len(grid)
	xmax := -1
	ymin := len(grid)
	ymax := -1
	for p, v := range grid {
		if v == Empty {
			continue
		}

		if xmin > p.x {
			xmin = p.x
		}
		if xmax < p.x {
			xmax = p.x
		}
		if ymin > p.y {
			ymin = p.y
		}
		if ymax < p.y {
			ymax = p.y
		}
	}

	for y := ymin; y <= ymax; y++ {
		for x := xmin; x <= xmax; x++ {
			p := Point{x, y}
			c, ok := grid[p]
			if ok {
				fmt.Print(string(c))
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func (p Point) neighbors() []Point {
	return []Point{
		{x: p.x - 1, y: p.y - 1},
		{x: p.x, y: p.y - 1},
		{x: p.x + 1, y: p.y - 1},
		{x: p.x - 1, y: p.y},
		{x: p.x + 1, y: p.y},
		{x: p.x - 1, y: p.y + 1},
		{x: p.x, y: p.y + 1},
		{x: p.x + 1, y: p.y + 1},
	}
}

func (p Point) northNeighbors() []Point {
	return []Point{
		{x: p.x - 1, y: p.y - 1},
		{x: p.x, y: p.y - 1},
		{x: p.x + 1, y: p.y - 1},
	}
}

func (p Point) southNeighbors() []Point {
	return []Point{
		{x: p.x - 1, y: p.y + 1},
		{x: p.x, y: p.y + 1},
		{x: p.x + 1, y: p.y + 1},
	}
}

func (p Point) eastNeighbors() []Point {
	return []Point{
		{x: p.x + 1, y: p.y - 1},
		{x: p.x + 1, y: p.y},
		{x: p.x + 1, y: p.y + 1},
	}
}

func (p Point) westNeighbors() []Point {
	return []Point{
		{x: p.x - 1, y: p.y - 1},
		{x: p.x - 1, y: p.y},
		{x: p.x - 1, y: p.y + 1},
	}
}

type Direction int

const (
	North Direction = iota
	South
	West
	East
)

type DirectionNeighbors struct {
	direction Direction
	neighbors []Point
}

func directionNeighbors(p Point, round int) []DirectionNeighbors {
	switch round % 4 {
	case 0:
		return []DirectionNeighbors{
			{direction: North, neighbors: p.northNeighbors()},
			{direction: South, neighbors: p.southNeighbors()},
			{direction: West, neighbors: p.westNeighbors()},
			{direction: East, neighbors: p.eastNeighbors()},
		}
	case 1:
		return []DirectionNeighbors{
			{direction: South, neighbors: p.southNeighbors()},
			{direction: West, neighbors: p.westNeighbors()},
			{direction: East, neighbors: p.eastNeighbors()},
			{direction: North, neighbors: p.northNeighbors()},
		}
	case 2:
		return []DirectionNeighbors{
			{direction: West, neighbors: p.westNeighbors()},
			{direction: East, neighbors: p.eastNeighbors()},
			{direction: North, neighbors: p.northNeighbors()},
			{direction: South, neighbors: p.southNeighbors()},
		}
	case 3:
		return []DirectionNeighbors{
			{direction: East, neighbors: p.eastNeighbors()},
			{direction: North, neighbors: p.northNeighbors()},
			{direction: South, neighbors: p.southNeighbors()},
			{direction: West, neighbors: p.westNeighbors()},
		}
	}
	return []DirectionNeighbors{}
}

func nextPoint(grid map[Point]Value, p Point, round int) Point {
	valid := true
	for _, n := range p.neighbors() {
		c, ok := grid[n]
		valid = valid && (!ok || c == Empty)
	}
	if valid {
		return p
	}

	for _, ns := range directionNeighbors(p, round) {
		valid = true
		for _, n := range ns.neighbors {
			c, ok := grid[n]
			valid = valid && (!ok || c == Empty)
		}
		if valid {
			switch ns.direction {
			case North:
				return Point{x: p.x, y: p.y - 1}
			case South:
				return Point{x: p.x, y: p.y + 1}
			case West:
				return Point{x: p.x - 1, y: p.y}
			case East:
				return Point{x: p.x + 1, y: p.y}
			}
		}
	}
	return p
}

type Pair struct {
	p1 Point
	p2 Point
}

func firstHalf(grid map[Point]Value, round int) []Pair {
	pairs := []Pair{}
	for p, v := range grid {
		if v == Elf {
			pairs = append(pairs, Pair{p1: p, p2: nextPoint(grid, p, round)})
		}
	}
	return pairs
}

func secondHalf(grid map[Point]Value, pairs []Pair) int {
	moves := []Pair{}
	for _, pair1 := range pairs {
		dup := false
		for _, pair2 := range pairs {
			if pair1 == pair2 {
				continue
			}
			if pair2.p2 == pair1.p2 {
				dup = true
				break
			}
		}
		if !dup && pair1.p1 != pair1.p2 {
			moves = append(moves, pair1)
		}
	}
	for _, pair := range moves {
		grid[pair.p1] = Empty
		grid[pair.p2] = Elf
	}
	return len(moves)
}

func playRound(grid map[Point]Value, round int) int {
	pairs := firstHalf(grid, round)
	return secondHalf(grid, pairs)
}

func countEmpty(grid map[Point]Value) int {
	xmin := len(grid)
	xmax := -1
	ymin := len(grid)
	ymax := -1
	for p, v := range grid {
		if v == Empty {
			continue
		}

		if xmin > p.x {
			xmin = p.x
		}
		if xmax < p.x {
			xmax = p.x
		}
		if ymin > p.y {
			ymin = p.y
		}
		if ymax < p.y {
			ymax = p.y
		}
	}

	cnt := 0
	for y := ymin; y <= ymax; y++ {
		for x := xmin; x <= xmax; x++ {
			p := Point{x, y}
			c, ok := grid[p]
			if !ok || c == Empty {
				cnt += 1
			}
		}
	}
	return cnt
}

func part1(grid map[Point]Value) int {
	for i := 0; i < 10; i++ {
		playRound(grid, i)
	}
	return countEmpty(grid)
}

func part2(grid map[Point]Value) int {
	i := 0
	for {
		moves := playRound(grid, i)
		i += 1
		if moves == 0 {
			break
		}
	}
	return i
}

func main() {
	s := time.Now()
	lines := readInput()
	s1 := time.Now()
	fmt.Println(part1(parseInput(lines)))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(parseInput(lines)))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
