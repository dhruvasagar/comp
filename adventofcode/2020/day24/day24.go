package main

import (
	"bufio"
	"fmt"
	"os"
)

type Direction string

const (
	East      Direction = "e"
	West                = "w"
	Northeast           = "ne"
	Northwest           = "nw"
	Southeast           = "se"
	Southwest           = "sw"
)

var directions = []Direction{
	East,
	West,
	Northeast,
	Northwest,
	Southeast,
	Southwest,
}

type Pos struct {
	x, y int
}

func (p Pos) String() string {
	return fmt.Sprintf("%d,%d", p.x, p.y)
}

func (p Pos) Move(d Direction) Pos {
	switch d {
	case East:
		p.x += 2
	case West:
		p.x -= 2
	case Northeast:
		p.y++
		p.x++
	case Northwest:
		p.y++
		p.x--
	case Southeast:
		p.y--
		p.x++
	case Southwest:
		p.y--
		p.x--
	}
	return p
}

type Color int

const (
	White Color = iota
	Black
)

var tileMap = make(map[string]*Tile)
var allTiles = []*Tile{}

type Tile struct {
	pos   Pos
	color Color
}

func (t *Tile) String() string {
	return fmt.Sprintf("%s:%d", t.pos, t.color)
}

func (t *Tile) adjBlackCount() int {
	cnt := 0
	for _, d := range directions {
		pos := t.pos.Move(d)
		tile := fromMap(pos)
		cnt += int(tile.color)
	}
	return cnt
}

func blackCount() int {
	bcnt := 0
	for _, tile := range allTiles {
		bcnt += int(tile.color)
	}
	return bcnt
}

func fromMap(pos Pos) *Tile {
	key := fmt.Sprint(pos)
	tile := tileMap[key]
	if tile != nil {
		return tile
	}

	tile = &Tile{pos: pos, color: White}
	tileMap[key] = tile
	allTiles = append(allTiles, tile)
	return tile
}

func part1(ds [][]Direction) int {
	for _, dsi := range ds {
		pos := Pos{0, 0}
		tile := fromMap(pos)
		for _, d := range dsi {
			pos = pos.Move(d)
			tile = fromMap(pos)
		}
		tile.color = 1 - tile.color
	}
	return blackCount()
}

func shouldFlip(tile *Tile, cnt int) bool {
	if tile.color == Black {
		return cnt == 0 || cnt > 2
	}
	return cnt == 2
}

func tileGame() {
	flip := []*Tile{}
	visited := make(map[string]bool)
	for _, tile := range allTiles {
		key := fmt.Sprint(tile.pos)
		if visited[key] {
			continue
		}
		visited[key] = true
		cnt := tile.adjBlackCount()
		if shouldFlip(tile, cnt) {
			flip = append(flip, tile)
		}
	}
	for _, tile := range allTiles {
		key := fmt.Sprint(tile.pos)
		if visited[key] {
			continue
		}
		visited[key] = true
		cnt := tile.adjBlackCount()
		if shouldFlip(tile, cnt) {
			flip = append(flip, tile)
		}
	}
	for _, ft := range flip {
		ft.color = 1 - ft.color
	}
}

const (
	numOfDays = 100
)

func part2() int {
	for i := 0; i < numOfDays; i++ {
		tileGame()
	}
	return blackCount()
}

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func parseInput(lines []string) [][]Direction {
	ds := [][]Direction{}
	for _, line := range lines {
		dsi := []Direction{}
		for i := 0; i < len(line); i++ {
			c := line[i : i+1]
			if c == "e" || c == "w" {
				dsi = append(dsi, Direction(c))
			} else {
				dsi = append(dsi, Direction(line[i:i+2]))
				i++
			}
		}
		ds = append(ds, dsi)
	}
	return ds
}

func main() {
	ds := parseInput(readInput())
	fmt.Println(part1(ds))
	fmt.Println(part2())
}
