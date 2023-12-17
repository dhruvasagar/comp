package main

import (
	"bufio"
	"fmt"
	"os"
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

type Pos struct{ x, y int }

func (p Pos) up() Pos {
	return Pos{p.x, p.y - 1}
}
func (p Pos) down() Pos {
	return Pos{p.x, p.y + 1}
}
func (p Pos) left() Pos {
	return Pos{p.x - 1, p.y}
}
func (p Pos) right() Pos {
	return Pos{p.x + 1, p.y}
}

func dfs(grid []string, pos Pos, dirn string, vis map[Pos]string) int {
	if pos.x < 0 || pos.y < 0 || pos.x >= len(grid[0]) || pos.y >= len(grid) {
		return len(vis)
	}

	c := grid[pos.y][pos.x]
	if d := vis[pos]; d == dirn {
		return len(vis)
	}

	vis[pos] = dirn
	switch c {
	case '.':
		switch dirn {
		case "up":
			dfs(grid, pos.up(), dirn, vis)
		case "down":
			dfs(grid, pos.down(), dirn, vis)
		case "left":
			dfs(grid, pos.left(), dirn, vis)
		case "right":
			dfs(grid, pos.right(), dirn, vis)
		}
	case '|':
		switch dirn {
		case "up":
			dfs(grid, pos.up(), dirn, vis)
		case "down":
			dfs(grid, pos.down(), dirn, vis)
		case "left", "right":
			dfs(grid, pos.up(), "up", vis)
			dfs(grid, pos.down(), "down", vis)
		}
	case '-':
		switch dirn {
		case "up", "down":
			dfs(grid, pos.left(), "left", vis)
			dfs(grid, pos.right(), "right", vis)
		case "left":
			dfs(grid, pos.left(), dirn, vis)
		case "right":
			dfs(grid, pos.right(), dirn, vis)
		}
	case '/':
		switch dirn {
		case "up":
			dfs(grid, pos.right(), "right", vis)
		case "down":
			dfs(grid, pos.left(), "left", vis)
		case "left":
			dfs(grid, pos.down(), "down", vis)
		case "right":
			dfs(grid, pos.up(), "up", vis)
		}
	case '\\':
		switch dirn {
		case "up":
			dfs(grid, pos.left(), "left", vis)
		case "down":
			dfs(grid, pos.right(), "right", vis)
		case "left":
			dfs(grid, pos.up(), "up", vis)
		case "right":
			dfs(grid, pos.down(), "down", vis)
		}
	}
	return len(vis)
}

func part1(lines []string) int {
	return dfs(lines, Pos{0, 0}, "right", map[Pos]string{})
}

func part2(lines []string) int {
	ymax, xmax := len(lines), len(lines[0])
	maxcount := 0
	for x := 0; x < xmax; x++ {
		c := dfs(lines, Pos{x, 0}, "down", map[Pos]string{})
		if maxcount < c {
			maxcount = c
		}
		c = dfs(lines, Pos{x, ymax - 1}, "up", map[Pos]string{})
		if maxcount < c {
			maxcount = c
		}
	}
	for y := 0; y < ymax; y++ {
		c := dfs(lines, Pos{0, y}, "right", map[Pos]string{})
		if maxcount < c {
			maxcount = c
		}
		c = dfs(lines, Pos{xmax - 1, y}, "left", map[Pos]string{})
		if maxcount < c {
			maxcount = c
		}
	}
	return maxcount
}

func main() {
	s := time.Now()
	lines := readInput()
	s1 := time.Now()
	fmt.Println(part1(lines))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(lines))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
