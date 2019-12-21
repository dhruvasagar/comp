package main

import (
	"errors"
	"fmt"
	"time"

	"github.com/dhruvasagar/adventofcode/2019/util"
	"github.com/gosuri/uilive"
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
	return fmt.Sprintf("x:%d,y:%d,tileID:%d", t.x, t.y, t.tileID)
}

func (t Tile) render() string {
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

func (t Tile) key() string {
	return fmt.Sprintf("%d,%d", t.x, t.y)
}

type Game struct {
	score int
	tiles []Tile

	tilesMap      map[string]Tile
	tilesIndexMap map[string]int
}

func (g Game) String() string {
	var minX, maxX, minY, maxY int
	for _, tile := range g.tiles {
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
	}

	gs := ""
	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			key := fmt.Sprintf("%d,%d", x, y)
			if tile, ok := g.tilesMap[key]; ok {
				gs += fmt.Sprintf("%s", tile.render())
			}
		}
		gs += "\n"
	}
	gs += "-------------\n"
	gs += fmt.Sprintf(" Score: %d\n", g.score)
	gs += "-------------\n"
	return gs
}

func (g Game) getTile(tileID TileID) Tile {
	for _, tile := range g.tiles {
		if tile.tileID == tileID {
			return tile
		}
	}
	return Tile{}
}

func (g Game) countTiles(tileID TileID) int {
	blockCount := 0
	for _, tile := range g.tiles {
		if tile.tileID == block {
			blockCount++
		}
	}
	return blockCount
}

func part1(instructions []int) *Game {
	computer := util.NewComputer(instructions, 1)
	defer computer.Close()
	go computer.Run()
	outputs := computer.PollResult()

	tileSize := 3
	game := &Game{
		tiles:         []Tile{},
		tilesMap:      make(map[string]Tile),
		tilesIndexMap: make(map[string]int),
	}
	for i := 0; i < len(outputs); i += tileSize {
		x := outputs[i]
		y := outputs[i+1]
		tileID := outputs[i+2]
		if x == -1 && y == 0 {
			game.score = tileID
		} else {
			tile := Tile{
				x:      x,
				y:      y,
				tileID: TileID(tileID),
			}
			game.tiles = append(game.tiles, tile)
			key := fmt.Sprintf("%d,%d", x, y)
			game.tilesMap[key] = tile
			game.tilesIndexMap[key] = len(game.tiles) - 1
		}
	}
	fmt.Printf("%+v\n", game)
	fmt.Println(game.countTiles(block))
	return game
}

var (
	gameReadError = errors.New("Error reading game from computer, possibly ended")
)

type joystick int

const (
	left = iota - 1
	neutral
	right
)

func (g *Game) play(computer *util.Computer) {
	writer := uilive.New()
	writer.Start()
	defer writer.Stop()
	for {
		select {
		case <-computer.Waiting:
			ballTile := g.getTile(ball)
			paddleTile := g.getTile(paddle)
			if ballTile.x < paddleTile.x {
				computer.Type(int(left))
			} else if ballTile.x > paddleTile.x {
				computer.Type(int(right))
			} else {
				computer.Type(int(neutral))
			}
		default:
			x, ok := computer.Read()
			if !ok {
				break
			}
			y, ok := computer.Read()
			if !ok {
				break
			}
			tileID, ok := computer.Read()
			if !ok {
				break
			}
			if x == -1 && y == 0 {
				// Scoreboard
				g.score = tileID
			} else {
				tile := Tile{
					x:      x,
					y:      y,
					tileID: TileID(tileID),
				}
				tileIndex := g.tilesIndexMap[tile.key()]
				g.tiles[tileIndex] = tile
				g.tilesMap[tile.key()] = tile
			}
		}
		fmt.Fprintln(writer, g)
		time.Sleep(time.Millisecond * 5)
		if g.countTiles(block) == 0 {
			break
		}
	}
}

func part2(instructions []int, game *Game) {
	instructions[0] = 2
	computer := util.NewComputer(instructions, 2)
	defer computer.Close()
	go computer.Run()
	game.play(computer)
	fmt.Println("Score: ", game.score)
}

func main() {
	instructions := util.ReadIntcodeInstructions()
	game := part1(instructions)
	part2(instructions, game)
}
