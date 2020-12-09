package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func min(x, y int) int {
	if x < y {
		return x
	}
	return y
}

func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

const GridSize = 1000

type Coordinate struct {
	x, y int
}

func NewCoord(coord string) Coordinate {
	c := Coordinate{}
	coords := strings.Split(coord, ",")
	c.x, _ = strconv.Atoi(coords[0])
	c.y, _ = strconv.Atoi(coords[1])
	return c
}

type Operation = string

const (
	on     = "turn on"
	off    = "turn off"
	toggle = "toggle"
)

type Instruction struct {
	operation Operation
	c1        Coordinate
	c2        Coordinate
}

type Grid struct {
	state [GridSize][GridSize]int
}

func (g Grid) String() string {
	r := ""
	for _, g := range g.state {
		for _, b := range g {
			r += fmt.Sprintf(" %d ", b)
		}
		r += fmt.Sprintln()
	}
	return r
}

func (g *Grid) LitCount() int {
	ans := 0
	for _, g := range g.state {
		for _, b := range g {
			if b == 1 {
				ans++
			}
		}
	}
	return ans
}

func (g *Grid) TotalBrightness() int {
	ans := 0
	for _, g := range g.state {
		for _, b := range g {
			ans += b
		}
	}
	return ans
}

func (g *Grid) Execute1(instructions []Instruction) {
	for _, ins := range instructions {
		sx := min(ins.c1.x, ins.c2.x)
		sy := min(ins.c1.y, ins.c2.y)
		ex := max(ins.c1.x, ins.c2.x)
		ey := max(ins.c1.y, ins.c2.y)
		for i := sx; i <= ex; i++ {
			for j := sy; j <= ey; j++ {
				switch ins.operation {
				case on:
					g.state[i][j] = 1
				case off:
					g.state[i][j] = 0
				case toggle:
					g.state[i][j] = 1 - g.state[i][j]
				}
			}
		}
	}
}

func (g *Grid) Execute2(instructions []Instruction) {
	for _, ins := range instructions {
		sx := min(ins.c1.x, ins.c2.x)
		sy := min(ins.c1.y, ins.c2.y)
		ex := max(ins.c1.x, ins.c2.x)
		ey := max(ins.c1.y, ins.c2.y)
		for i := sx; i <= ex; i++ {
			for j := sy; j <= ey; j++ {
				switch ins.operation {
				case on:
					g.state[i][j]++
				case off:
					if g.state[i][j] > 0 {
						g.state[i][j]--
					}
				case toggle:
					g.state[i][j] += 2
				}
			}
		}
	}
}

func part1(instructions []Instruction) string {
	grid := &Grid{}
	grid.Execute1(instructions)
	return fmt.Sprint(grid.LitCount())
}

func part2(instructions []Instruction) string {
	grid := &Grid{}
	grid.Execute2(instructions)
	return fmt.Sprint(grid.TotalBrightness())
}

func parseInput(input []string) []Instruction {
	instructions := []Instruction{}
	for _, in := range input {
		instruction := Instruction{}
		parts := strings.Split(in, " through ")
		opcord := strings.Split(parts[0], " ")
		if strings.HasPrefix(opcord[0], "toggle") {
			instruction.operation = toggle
		} else {
			instruction.operation = strings.Join(opcord[0:2], " ")
		}
		instruction.c1 = NewCoord(opcord[len(opcord)-1])
		instruction.c2 = NewCoord(parts[1])
		instructions = append(instructions, instruction)
	}
	return instructions
}

func main() {
	instructions := parseInput(readInput())
	fmt.Println(part1(instructions))
	fmt.Println(part2(instructions))
}
