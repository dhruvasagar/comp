package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

func readInput() string {
	reader := bufio.NewReader(os.Stdin)
	text, _ := reader.ReadString('\n')
	return text[:len(text)-1]
}

const (
	Left  rune = '<'
	Right      = '>'
)

func parseInput(s string) {
}

type Point struct {
	x, y int64
}

func (p Point) left() Point {
	return Point{x: p.x - 1, y: p.y}
}

func (p Point) right() Point {
	return Point{x: p.x + 1, y: p.y}
}

func (p Point) below() Point {
	return Point{x: p.x, y: p.y + 1}
}

type Shape int

const (
	Line Shape = iota
	Plus
	L
	Vert
	Square
)

type Rock struct {
	shape  Shape
	points []Point
}

func (r Rock) top() int64 {
	switch r.shape {
	case Line:
		return r.points[0].y
	case Plus:
		return r.points[1].y
	case L:
		return r.points[4].y
	case Vert:
		return r.points[0].y
	case Square:
		return r.points[0].y
	}
	return -1
}

func (r Rock) bottom() int64 {
	switch r.shape {
	case Line:
		return r.points[0].y
	case Plus:
		return r.points[3].y
	case L:
		return r.points[0].y
	case Vert:
		return r.points[3].y
	case Square:
		return r.points[3].y
	}
	return -1
}

func (r Rock) height() int64 {
	switch r.shape {
	case Line:
		return 1
	case Plus:
		return 3
	case L:
		return 3
	case Vert:
		return 4
	case Square:
		return 2
	}
	return -1
}

func (r Rock) moveLeft() Rock {
	points := []Point{}
	for _, p := range r.points {
		points = append(points, Point{x: p.x - 1, y: p.y})
	}
	return Rock{shape: r.shape, points: points}
}

func (r Rock) moveRight() Rock {
	points := []Point{}
	for _, p := range r.points {
		points = append(points, Point{x: p.x + 1, y: p.y})
	}
	return Rock{shape: r.shape, points: points}
}

func (r Rock) moveDown(dy int64) Rock {
	points := []Point{}
	for _, p := range r.points {
		points = append(points, Point{x: p.x, y: p.y + dy})
	}
	return Rock{shape: r.shape, points: points}
}

func (r Rock) moveUp(dy int64) Rock {
	points := []Point{}
	for _, p := range r.points {
		points = append(points, Point{x: p.x, y: p.y - dy})
	}
	return Rock{shape: r.shape, points: points}
}

func (r Rock) onRock(point Point) bool {
	for _, p := range r.points {
		if p == point {
			return true
		}
	}
	return false
}

var (
	lineRock = Rock{
		shape: Line,
		points: []Point{
			{x: 2, y: 0},
			{x: 3, y: 0},
			{x: 4, y: 0},
			{x: 5, y: 0},
		},
	}
	plusRock = Rock{
		shape: Plus,
		points: []Point{
			{x: 2, y: 1},
			{x: 3, y: 0},
			{x: 3, y: 1},
			{x: 3, y: 2},
			{x: 4, y: 1},
		},
	}
	lRock = Rock{
		shape: L,
		points: []Point{
			{x: 2, y: 2},
			{x: 3, y: 2},
			{x: 4, y: 2},
			{x: 4, y: 1},
			{x: 4, y: 0},
		},
	}
	vertRock = Rock{
		shape: Vert,
		points: []Point{
			{x: 2, y: 0},
			{x: 2, y: 1},
			{x: 2, y: 2},
			{x: 2, y: 3},
		},
	}
	squareRock = Rock{
		shape: Square,
		points: []Point{
			{x: 2, y: 0},
			{x: 3, y: 0},
			{x: 2, y: 1},
			{x: 3, y: 1},
		},
	}
	noRock = Rock{}
)

var rocks = []Rock{lineRock, plusRock, lRock, vertRock, squareRock}

type Chamber struct {
	ytop   int64
	yfloor int64
	crock  Rock
	pmap   map[Point]rune
}

func (c Chamber) height() int64 {
	return c.yfloor - c.ytop
}

func (c Chamber) canMoveLeft(rock Rock) bool {
	can := true
	for _, p := range rock.points {
		np := p.left()
		if np.x == -1 {
			return false
		}
		can = can && c.pmap[np] != '#'
	}
	return can
}

func (c Chamber) canMoveRight(rock Rock) bool {
	can := true
	for _, p := range rock.points {
		np := p.right()
		if np.x == 7 {
			return false
		}
		can = can && c.pmap[np] != '#'
	}
	return can
}

func (c Chamber) canMoveDown(rock Rock) bool {
	can := true
	for _, p := range rock.points {
		np := p.below()
		if np.y == c.yfloor {
			return false
		}
		can = can && c.pmap[np] != '#'
	}
	return can
}

func (c *Chamber) enter(rock Rock) Rock {
	diff := rock.height() - (c.ytop - 3)
	rock = rock.moveUp(diff)
	c.crock = rock
	return rock
}

func (c *Chamber) push(rock Rock) {
	if rock.top() < c.ytop {
		c.ytop = rock.top()
	}
	c.crock = noRock
	for _, p := range rock.points {
		c.pmap[p] = '#'
	}
}

type CacheKey struct {
	midx    int
	ridx    int
	heights [7]int64
}

type CacheValue struct {
	cnt    int
	height int64
}

func (c *Chamber) play(moves string, limit int) int64 {
	cnt := 0
	ridx := 0
	midx := 0
	height_gained := int64(0)

	cache := make(map[CacheKey]CacheValue)
	cycle_found := false

	rock := rocks[ridx]
	rock = c.enter(rock)

	mlen := len(moves)
	rlen := len(rocks)

	for cnt < limit {
		m := rune(moves[midx])

		key := CacheKey{
			midx:    midx,
			ridx:    ridx,
			heights: c.getHeights(),
		}
		value := CacheValue{
			cnt:    cnt,
			height: c.height(),
		}

		if last, ok := cache[key]; !cycle_found && ok {
			cycle_length := cnt - last.cnt
			cycles_to_go := (limit - cnt) / cycle_length

			cnt += cycles_to_go * cycle_length
			height_gained = int64(cycles_to_go) * (value.height - last.height)
			cycle_found = true
		}

		cache[key] = value

		if m == Left {
			if c.canMoveLeft(rock) {
				rock = rock.moveLeft()
				c.crock = rock
			}
		} else if m == Right {
			if c.canMoveRight(rock) {
				rock = rock.moveRight()
				c.crock = rock
			}
		}

		if c.canMoveDown(rock) {
			rock = rock.moveDown(1)
			c.crock = rock
		} else {
			cnt += 1
			// come to rest
			c.push(rock)

			ridx = (ridx + 1) % rlen
			rock = rocks[ridx]
			rock = c.enter(rock)
		}

		midx = (midx + 1) % mlen
	}

	return c.height() + height_gained
}

func (c Chamber) getHeights() [7]int64 {
	res := [7]int64{0, 0, 0, 0, 0, 0, 0}
	if len(c.pmap) == 0 {
		return res
	}
	for x := int64(0); x < 7; x++ {
		y := c.ytop
		for {
			p := Point{x, y}
			if c.pmap[p] == '#' {
				res[x] = y - c.ytop
				break
			}
			y += 1
			if y == c.yfloor {
				break
			}
		}
	}
	return res
}

func (c Chamber) print() {
	for y := c.ytop; y <= c.yfloor; y++ {
		for x := int64(-1); x <= 7; x++ {
			p := Point{x, y}
			if x == -1 || x == 7 || y == c.yfloor {
				fmt.Print("|")
			} else if c.crock.onRock(p) {
				fmt.Print("@")
			} else {
				ch, ok := c.pmap[p]
				if ok {
					fmt.Print(string(ch))
				} else {
					fmt.Print(".")
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func part1(moves string) int64 {
	chamber := &Chamber{
		ytop:   3,
		yfloor: 3,
		pmap:   make(map[Point]rune),
	}
	return chamber.play(moves, 2022)
}

func part2(moves string) int64 {
	chamber := &Chamber{
		ytop:   3,
		yfloor: 3,
		pmap:   make(map[Point]rune),
	}
	return chamber.play(moves, 1000000000000)
}

func main() {
	s := time.Now()
	line := readInput()
	s1 := time.Now()
	fmt.Println(part1(line))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(line))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
