package main

import (
	"bufio"
	"fmt"
	"os"
)

type Direction rune

const (
	Up    Direction = 'U'
	Left            = 'L'
	Down            = 'D'
	Right           = 'R'
)

func (d Direction) String() string {
	switch d {
	case Up:
		return fmt.Sprint("Up")
	case Left:
		return fmt.Sprint("Left")
	case Down:
		return fmt.Sprintf("Down")
	case Right:
		return fmt.Sprintf("Right")
	}
	return ""
}

type Pos struct {
	x, y int
}

type Instruction struct {
	moves []Direction
}

func (i Instruction) String() string {
	r := ""
	for _, m := range i.moves {
		r += fmt.Sprint(m)
	}
	return r
}

type Keypad struct {
	keys []string
}

var (
	keypad = Keypad{
		keys: []string{
			"123",
			"456",
			"789",
		},
	}
	keypad2 = Keypad{
		keys: []string{
			"  1  ",
			" 234 ",
			"56789",
			" ABC ",
			"  D  ",
		},
	}
)

func (k Keypad) Navigate(pos *Pos, ins Instruction) *Pos {
	for _, move := range ins.moves {
		switch move {
		case Up:
			if pos.y > 0 && k.keys[pos.y-1][pos.x] != ' ' {
				pos.y--
			}
		case Right:
			if pos.x+1 < len(k.keys[pos.y]) && k.keys[pos.y][pos.x+1] != ' ' {
				pos.x++
			}
		case Down:
			if pos.y+1 < len(k.keys) && k.keys[pos.y+1][pos.x] != ' ' {
				pos.y++
			}
		case Left:
			if pos.x > 0 && k.keys[pos.x-1][pos.y] != ' ' {
				pos.x--
			}
		}
	}
	return pos
}

func part1(ins []Instruction) string {
	pos := &Pos{1, 1}
	code := ""
	for _, is := range ins {
		pos := keypad.Navigate(pos, is)
		code += string(keypad.keys[pos.y][pos.x])
	}
	return code
}

func part2(ins []Instruction) string {
	pos := &Pos{0, 2}
	code := ""
	for _, is := range ins {
		pos := keypad2.Navigate(pos, is)
		code += string(keypad2.keys[pos.y][pos.x])
	}
	return code
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func parseInstruction(line string) Instruction {
	ins := Instruction{}
	for _, c := range line {
		ins.moves = append(ins.moves, Direction(c))
	}
	return ins
}

func parseInput(lines []string) []Instruction {
	inss := []Instruction{}
	for _, line := range lines {
		inss = append(inss, parseInstruction(line))
	}
	return inss
}

func main() {
	ins := parseInput(readInput())
	fmt.Println(part1(ins))
	fmt.Println(part2(ins))
}
