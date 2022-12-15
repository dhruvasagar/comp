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

type Point struct {
	x, y int
}

func (p Point) down() Point {
	return Point{x: p.x, y: p.y + 1}
}
func (p Point) left() Point {
	return Point{x: p.x - 1, y: p.y}
}
func (p Point) right() Point {
	return Point{x: p.x + 1, y: p.y}
}

func parsePaths(lines []string) map[Point]rune {
	paths := make(map[Point]rune)
	for _, line := range lines {
		coords := strings.Split(line, " -> ")
		points := []Point{}
		for _, coord := range coords {
			xys := strings.Split(coord, ",")
			x, _ := strconv.Atoi(xys[0])
			y, _ := strconv.Atoi(xys[1])
			p := Point{x, y}
			points = append(points, p)
		}

		for i := 0; i < len(points)-1; i++ {
			p1 := points[i]
			p2 := points[i+1]
			if p1.x == p2.x {
				if p1.y < p2.y {
					for yi := p1.y; yi <= p2.y; yi++ {
						p := Point{x: p1.x, y: yi}
						paths[p] = '#'
					}
				} else {
					for yi := p2.y; yi <= p1.y; yi++ {
						p := Point{x: p1.x, y: yi}
						paths[p] = '#'
					}
				}
			} else {
				if p1.x < p2.x {
					for xi := p1.x; xi <= p2.x; xi++ {
						p := Point{x: xi, y: p1.y}
						paths[p] = '#'
					}
				} else {
					for xi := p2.x; xi <= p1.x; xi++ {
						p := Point{x: xi, y: p1.y}
						paths[p] = '#'
					}
				}
			}
		}
	}
	return paths
}

func fallingForever(paths map[Point]rune, p Point) bool {
	ymax := 0
	for pi := range paths {
		if ymax < pi.y {
			ymax = pi.y
		}
	}
	return p.y > ymax
}

func isNotWall(paths map[Point]rune, p Point) bool {
	_, ok := paths[p]
	return !ok
}

func simulateSand(paths map[Point]rune) {
	ymin := 0
	p := Point{x: 500, y: 0}
	for {
		if fallingForever(paths, p) {
			return
		}

		np := p.down()
		if isNotWall(paths, np) {
			p = np
		} else {
			if isNotWall(paths, np.left()) {
				p = np.left()
			} else if isNotWall(paths, np.right()) {
				p = np.right()
			} else {
				paths[p] = 'o'
				if ymin > p.y {
					ymin = p.y
				}
				p = Point{x: 500, y: ymin - 1}
			}
		}
	}
}

func countResting(paths map[Point]rune) int {
	cnt := 0
	for _, c := range paths {
		if c == 'o' {
			cnt += 1
		}
	}
	return cnt
}

func part1(paths map[Point]rune) int {
	simulateSand(paths)
	return countResting(paths)
}

func isNotWallOrFloor(paths map[Point]rune, fy int, p Point) bool {
	if p.y == fy {
		return false
	}

	return isNotWall(paths, p)
}

func simulateSandWithFloor(paths map[Point]rune, fy int) {
	p := Point{x: 500, y: 0}
	top := Point{x: 500, y: 0}
	ymin := 0
	for {
		if paths[top] == 'o' {
			return
		}

		np := p.down()
		if isNotWallOrFloor(paths, fy, np) {
			p = np
		} else {
			if isNotWallOrFloor(paths, fy, np.left()) {
				p = np.left()
			} else if isNotWallOrFloor(paths, fy, np.right()) {
				p = np.right()
			} else {
				paths[p] = 'o'
				if ymin > p.y {
					ymin = p.y
				}
				p = Point{x: 500, y: ymin - 1}
			}
		}
	}
}

func part2(paths map[Point]rune) int {
	ymax := 0
	for p := range paths {
		if ymax < p.y {
			ymax = p.y
		}
	}
	simulateSandWithFloor(paths, ymax+2)
	return countResting(paths)
}

func main() {
	lines := readInput()
	fmt.Println(part1(parsePaths(lines)))
	fmt.Println(part2(parsePaths(lines)))
}
