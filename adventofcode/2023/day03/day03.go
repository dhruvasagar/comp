package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
	"unicode"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func isSymbol(ch rune) bool {
	return ch != '.' && !unicode.IsDigit(ch)
}

type Pos struct {
	x int
	y int
}

func (p Pos) neighbors() []Pos {
	return []Pos{
		{x: p.x, y: p.y + 1},
		{x: p.x, y: p.y - 1},
		{x: p.x + 1, y: p.y + 1},
		{x: p.x + 1, y: p.y},
		{x: p.x + 1, y: p.y - 1},
		{x: p.x - 1, y: p.y + 1},
		{x: p.x - 1, y: p.y},
		{x: p.x - 1, y: p.y - 1},
	}
}

type Grid struct {
	grid []string
}

func NewGrid(grid []string) Grid {
	g := Grid{grid}
	g = g.padGrid('.')
	return g
}

func (g Grid) isNeighborSymbol(p Pos) bool {
	neighbors := p.neighbors()
	for _, npos := range neighbors {
		if isSymbol(rune(g.grid[npos.y][npos.x])) {
			return true
		}
	}
	return false
}

func (g *Grid) padGrid(ch rune) Grid {
	rg := Grid{}
	len := len(g.grid[0])
	rg.grid = append(rg.grid, strings.Repeat(string(ch), len+1))
	for _, row := range g.grid {
		rg.grid = append(rg.grid, fmt.Sprintf("%c%s%c", ch, row, ch))
	}
	rg.grid = append(rg.grid, strings.Repeat(string(ch), len+1))
	return rg
}

func (g Grid) getNumber(pos Pos) int {
	c := g.grid[pos.y][pos.x]
	if !unicode.IsDigit(rune(c)) {
		return 0
	}

	numstr := string(c)
	xi := pos.x
	for unicode.IsDigit(rune(g.grid[pos.y][xi-1])) {
		numstr = fmt.Sprintf("%c%s", g.grid[pos.y][xi-1], numstr)
		xi -= 1
	}
	xi = pos.x
	for unicode.IsDigit(rune(g.grid[pos.y][xi+1])) {
		numstr = fmt.Sprintf("%s%c", numstr, g.grid[pos.y][xi+1])
		xi += 1
	}
	num, err := strconv.Atoi(numstr)
	if err != nil {
		return 0
	}
	return num
}

func (g Grid) getPartNumbersSum() (int, int) {
	nums := []int{}
	p2 := 0
	for y := range g.grid {
		numstr := ""
		isNum := false
		isPartNumber := false
		for x, c := range g.grid[y] {
			pos := Pos{x, y}

			if unicode.IsDigit(c) {
				isNum = true
				numstr = fmt.Sprintf("%s%c", numstr, c)
			} else if isNum {
				isNum = false
				if isPartNumber {
					num, _ := strconv.Atoi(numstr)
					nums = append(nums, num)
					isPartNumber = false
				}
				numstr = ""
			} else {
				numstr = ""
			}

			if isNum {
				if g.isNeighborSymbol(pos) {
					isPartNumber = true
				}
			}

			if c == '*' {
				neighs := pos.neighbors()
				nums := []int{}
				for _, npos := range neighs {
					num := g.getNumber(npos)
					if num == 0 {
						continue
					}
					found := false
					for _, n := range nums {
						if n == num {
							found = true
							break
						}
					}
					if !found {
						nums = append(nums, g.getNumber(npos))
					}
				}
				if len(nums) == 2 {
					p2 += nums[0] * nums[1]
				}
			}
		}
	}
	sum := 0
	for _, num := range nums {
		sum += num
	}
	return sum, p2
}

func main() {
	s := time.Now()
	lines := readInput()
	grid := NewGrid(lines)
	p1, p2 := grid.getPartNumbersSum()
	fmt.Println(p1)
	fmt.Println(p2)
	e := time.Since(s)
	fmt.Printf("Total Time : %s\n", e)
}
