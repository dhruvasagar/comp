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

type Grid struct {
	xmax int
	ymax int
	grid [][]rune
}

func NewGrid(lines []string) Grid {
	ymax := len(lines)
	xmax := len(lines[0])
	grid := [][]rune{}
	for y := 0; y < ymax; y++ {
		grid = append(grid, []rune{})
		for x := 0; x < xmax; x++ {
			grid[y] = append(grid[y], rune(lines[y][x]))
		}
	}
	return Grid{xmax, ymax, grid}
}

func (g Grid) String() string {
	var sb strings.Builder
	for y := 0; y < g.ymax; y++ {
		for x := 0; x < g.xmax; x++ {
			sb.WriteRune(g.grid[y][x])
		}
		sb.WriteRune('\n')
	}
	return sb.String()
}

func (g Grid) hash() string {
	var sb strings.Builder
	for y := 0; y < g.ymax; y++ {
		for x := 0; x < g.xmax; x++ {
			sb.WriteRune(g.grid[y][x])
		}
	}
	return sb.String()
}

func (g *Grid) tilt_step(dx, dy int) {
	new_grid := [][]rune{}
	for _, row := range g.grid {
		new_grid = append(new_grid, row)
	}
	for y := 0; y < g.ymax; y++ {
		for x := 0; x < g.xmax; x++ {
			nx := x + dx
			ny := y + dy
			if nx < 0 || ny < 0 || nx >= g.xmax || ny >= g.ymax {
				continue
			}
			if new_grid[y][x] == 'O' && new_grid[ny][nx] == '.' {
				new_grid[y][x] = rune('.')
				new_grid[ny][nx] = rune('O')
			}
		}
	}
	g.grid = new_grid
}

func (g *Grid) tilt(dx, dy int) {
	phash := g.hash()
	for {
		g.tilt_step(dx, dy)
		hash := g.hash()
		if phash == hash {
			break
		}

		phash = hash
	}
}

func (g *Grid) cycle() {
	dirs := [][]int{
		{0, -1},
		{-1, 0},
		{0, 1},
		{1, 0},
	}
	for _, d := range dirs {
		g.tilt(d[0], d[1])
	}
}

func (g Grid) total_load() int {
	load := 0
	for y := 0; y < g.ymax; y++ {
		for x := 0; x < g.xmax; x++ {
			if g.grid[y][x] == 'O' {
				load += g.ymax - y
			}
		}
	}
	return load
}

func part1(grid *Grid) int {
	grid.tilt(0, -1)
	return grid.total_load()
}

func part2(grid *Grid) int {
	hashes := []string{grid.hash()}
	start := 0
	final := 0
	i := 0
	for {
		i += 1
		grid.cycle()
		hash := grid.hash()
		match := false
		for s := range hashes {
			if hash == hashes[s] {
				start = s
				match = true
				break
			}
		}
		if match {
			final = i
			break
		}
		hashes = append(hashes, hash)
	}
	remaining := (1000000000 - final) % (final - start)
	for i := 0; i < remaining; i++ {
		grid.cycle()
	}
	return grid.total_load()
}

func main() {
	s := time.Now()
	lines := readInput()

	s1 := time.Now()
	grid1 := NewGrid(lines)
	fmt.Println(part1(&grid1))
	e1 := time.Since(s1)

	s2 := time.Now()
	grid2 := NewGrid(lines)
	fmt.Println(part2(&grid2))
	e2 := time.Since(s2)

	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
