package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

type TileID int

const (
	empty = iota
	wall
	block
	paddle
	ball
)

type Tile struct {
	x      int
	y      int
	tileID TileID
}

func (t Tile) String() string {
	switch t.tileID {
	case empty:
		return " "
	case wall:
		return "="
	case block:
		return "█"
	case paddle:
		return "-"
	case ball:
		return "●"
	}
	return ""
}

type Game []Tile

func (g Game) String() string {
	var minX, maxX, minY, maxY int
	tileCache := make(map[string]Tile)
	for _, tile := range g {
		if minX > tile.x {
			minX = tile.x
		}
		if maxX < tile.x {
			maxX = tile.x
		}
		if minY > tile.y {
			minY = tile.y
		}
		if maxY < tile.y {
			maxY = tile.y
		}
		key := fmt.Sprintf("%d,%d", tile.x, tile.y)
		tileCache[key] = tile
	}

	game := ""
	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			key := fmt.Sprintf("%d,%d", x, y)
			if tile, ok := tileCache[key]; ok {
				game += fmt.Sprintf("%s", tile)
			}
		}
		game += "\n"
	}
	return game
}

func part1(instructions []int) {
	computer := util.NewComputer(instructions, 1)
	defer computer.Close()
	go computer.Run()
	outputs := computer.PollResult()

	tileSize := 3
	game := Game{}
	for i := 0; i < len(outputs); i += tileSize {
		tile := Tile{
			x:      outputs[i],
			y:      outputs[i+1],
			tileID: TileID(outputs[i+2]),
		}
		game = append(game, tile)
	}
	fmt.Printf("%+v\n", game)

	blockCount := 0
	for _, tile := range game {
		if tile.tileID == block {
			blockCount++
		}
	}
	fmt.Println(blockCount)
}

func part2(instructions []int) {
	instructions[0] = 2
	computer := util.NewComputer(instructions, 2)
	defer computer.Close()
	go computer.Run()

	scanner := bufio.NewScanner(os.Stdin)
	keyboard := make(chan string)

	go func() {
		for scanner.Scan() {
			key := scanner.Text()
			keycode := 0
			if key == "l" {
				keycode = -1
			} else if key == "r" {
				keycode = 1
			}
			computer.Type(keycode)
		}
	}()

	go func() {
		game := Game{}
		for {
			x, ok := c.Read()
			if !ok {
				break
			}
			y, ok := c.Read()
			if !ok {
				break
			}
			z, ok := c.Read()
			if !ok {
				break
			}
		}
	}()
}

func main() {
	instructions := util.ReadIntcodeInstructions()
	part1(instructions)
	part2(instructions)
}
