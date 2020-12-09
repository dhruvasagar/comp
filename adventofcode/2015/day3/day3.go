package main

import (
	"bufio"
	"fmt"
	"os"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Coordinate struct {
	x, y int
}

func (c Coordinate) key() string {
	return fmt.Sprintf("%d,%d", c.x, c.y)
}

func (c *Coordinate) up() {
	c.y--
}

func (c *Coordinate) down() {
	c.y++
}

func (c *Coordinate) left() {
	c.x--
}

func (c *Coordinate) right() {
	c.x++
}

func (c *Coordinate) move(direction rune) {
	switch direction {
	case '^':
		c.up()
	case '<':
		c.left()
	case '>':
		c.right()
	case 'v':
		c.down()
	}
}

func rem(visited map[string]bool, c Coordinate) bool {
	key := c.key()
	if _, ok := visited[key]; ok {
		return false
	}
	visited[key] = true
	return true
}

func part1(directions string) int {
	start := &Coordinate{0, 0}
	visited := make(map[string]bool)
	houseCount := 1
	rem(visited, *start)
	for _, d := range directions {
		start.move(d)
		if rem(visited, *start) {
			houseCount++
		}
	}
	return houseCount
}

func part2(directions string) int {
	start1 := &Coordinate{0, 0}
	start2 := &Coordinate{0, 0}
	visited := make(map[string]bool)
	rem(visited, *start1)
	santaMove := true
	houseCount := 1
	for _, d := range directions {
		if santaMove {
			start1.move(d)
			if rem(visited, *start1) {
				houseCount++
			}
		} else {
			start2.move(d)
			if rem(visited, *start2) {
				houseCount++
			}
		}
		santaMove = !santaMove
	}
	return houseCount
}

func main() {
	directions := readInput()[0]
	fmt.Println(part1(directions))
	fmt.Println(part2(directions))
}
