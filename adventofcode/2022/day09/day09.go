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

type Direction string

const (
	UP    Direction = "U"
	RIGHT           = "R"
	DOWN            = "D"
	LEFT            = "L"
)

type Move struct {
	direction Direction
	count     int
}

type Point struct {
	x int
	y int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (p Point) dist(p1 Point) int {
	xdiff := abs(p.x - p1.x)
	ydiff := abs(p.y - p1.y)
	if xdiff > ydiff {
		return xdiff
	}
	return ydiff
}

type Rope struct {
	knots []Point
}

func (r *Rope) len() int {
	return len(r.knots)
}

func (r *Rope) move(move Move, hist map[Point]bool) {
	len := r.len()
	for i := 0; i < move.count; i++ {
		for j := 0; j < len; j++ {
			t := r.knots[j]
			if j == 0 {
				switch move.direction {
				case RIGHT:
					t.x += 1
					break
				case LEFT:
					t.x -= 1
					break
				case UP:
					t.y -= 1
				case DOWN:
					t.y += 1
				}
			} else {
				h := r.knots[j-1]
				if h.dist(t) > 1 {
					if h.x > t.x {
						t.x += 1
					} else if h.x < t.x {
						t.x -= 1
					}
					if h.y < t.y {
						t.y -= 1
					} else if h.y > t.y {
						t.y += 1
					}
				}
			}

			if j == len-1 {
				hist[t] = true
			}
			r.knots[j] = t
		}
	}
}

func (r *Rope) simulateMoves(moves []Move) int {
	hist := make(map[Point]bool)
	hist[Point{x: 0, y: 0}] = true
	for _, move := range moves {
		r.move(move, hist)
	}
	// printMap(hist)
	return len(hist)
}

func parseInput(lines []string) []Move {
	moves := []Move{}
	for _, line := range lines {
		l := strings.Split(line, " ")
		count, _ := strconv.Atoi(l[1])
		move := Move{
			count:     count,
			direction: Direction(l[0]),
		}
		moves = append(moves, move)
	}
	return moves
}

func printPoints(points []Point) {
	minx := -15
	maxx := 15
	miny := -15
	maxy := 15
	for y := miny; y <= maxy; y++ {
		for x := minx; x <= maxx; x++ {
			if x == 0 && y == 0 {
				fmt.Print("s")
				continue
			}
			p := Point{x, y}
			pidx := -1
			for i, po := range points {
				if p == po {
					pidx = i
					break
				}
			}
			if pidx == -1 {
				fmt.Print(".")
			} else if pidx == 0 {
				fmt.Print("H")
			} else if pidx < len(points)-1 {
				fmt.Print(pidx)
			} else {
				fmt.Print("T")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func printMap(hist map[Point]bool) {
	minx := -15
	maxx := 15
	miny := -15
	maxy := 15
	// for p := range hist {
	// 	if minx > p.x {
	// 		minx = p.x
	// 	}
	// 	if maxx < p.x {
	// 		maxx = p.x
	// 	}
	// 	if miny > p.y {
	// 		miny = p.y
	// 	}
	// 	if maxy < p.y {
	// 		maxy = p.y
	// 	}
	// }
	for y := miny; y <= maxy; y++ {
		for x := minx; x <= maxx; x++ {
			if x == 0 && y == 0 {
				fmt.Print("s")
			} else {
				if _, ok := hist[Point{x, y}]; ok {
					fmt.Print("#")
				} else {
					fmt.Print(".")
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func part1(moves []Move) int {
	p := Point{x: 0, y: 0}
	rope := &Rope{
		knots: []Point{
			p,
			p,
		},
	}
	return rope.simulateMoves(moves)
}

func part2(moves []Move) int {
	p := Point{x: 0, y: 0}
	rope := &Rope{
		knots: []Point{
			p,
			p,
			p,
			p,
			p,
			p,
			p,
			p,
			p,
			p,
		},
	}
	return rope.simulateMoves(moves)
}

func main() {
	moves := parseInput(readInput())
	fmt.Println(part1(moves))
	fmt.Println(part2(moves))
}
