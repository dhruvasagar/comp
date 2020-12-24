package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Turn string

const (
	Left  Turn = "L"
	Right      = "R"
)

func (t Turn) String() string {
	switch t {
	case Left:
		return "Left"
	case Right:
		return "Right"
	}
	return ""
}

type Instruction struct {
	turn   Turn
	amount int
}

func (i Instruction) String() string {
	return fmt.Sprintf("%s:%d", i.turn, i.amount)
}

type Pos struct {
	x, y int
}

func (p Pos) String() string {
	return fmt.Sprintf("%d,%d", p.x, p.y)
}

type Direction int

const (
	North Direction = iota
	East
	South
	West
)

var Directions = []Direction{North, East, South, West}

func (d Direction) String() string {
	switch d {
	case North:
		return "North"
	case East:
		return "East"
	case South:
		return "South"
	case West:
		return "West"
	}
	return ""
}

func findDirectionIndex(dir Direction) int {
	for i, d := range Directions {
		if d == dir {
			return i
		}
	}
	return -1
}

type Santa struct {
	pos  *Pos
	face Direction
}

func (p *Santa) Turn(turn Turn) {
	index := findDirectionIndex(p.face)
	switch turn {
	case Left:
		if index == 0 {
			index = len(Directions) - 1
		} else {
			index--
		}
	case Right:
		index = (index + 1) % len(Directions)
	}
	p.face = Directions[index]
}

func (p *Santa) Move(amount int) {
	switch p.face {
	case North:
		p.pos.y += amount
	case East:
		p.pos.x += amount
	case South:
		p.pos.y -= amount
	case West:
		p.pos.x -= amount
	}
}

func (p *Santa) ProcessInstruction(ins Instruction) {
	p.Turn(ins.turn)
	p.Move(ins.amount)
}

func (p *Santa) FirstRepeat(ins Instruction, posMap map[string]bool) *Pos {
	p.Turn(ins.turn)
	for i := 0; i < ins.amount; i++ {
		p.Move(1)
		key := fmt.Sprint(p.pos)
		if _, ok := posMap[key]; ok {
			return p.pos
		}
		posMap[key] = true
	}
	return nil
}

func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (p *Santa) MD() int {
	return Abs(p.pos.x) + Abs(p.pos.y)
}

func part1(instructions []string) string {
	santa := &Santa{pos: &Pos{0, 0}, face: North}
	for _, ins := range instructions {
		turn, amt := string(ins[0]), ins[1:]
		amount, _ := strconv.Atoi(amt)
		inst := Instruction{turn: Turn(turn), amount: amount}
		santa.ProcessInstruction(inst)
	}
	return fmt.Sprint(santa.MD())
}

func part2(instructions []string) string {
	santa := &Santa{pos: &Pos{0, 0}, face: North}
	posMap := make(map[string]bool)
	for _, ins := range instructions {
		turn, amt := string(ins[0]), ins[1:]
		amount, _ := strconv.Atoi(amt)
		inst := Instruction{turn: Turn(turn), amount: amount}
		pos := santa.FirstRepeat(inst, posMap)
		if pos != nil {
			return fmt.Sprint(santa.MD())
		}
	}
	return ""
}

func parseInput(input string) []string {
	return strings.Split(input, ", ")
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')
	fmt.Println(part1(parseInput(input[:len(input)-1])))
	fmt.Println(part2(parseInput(input[:len(input)-1])))
}
