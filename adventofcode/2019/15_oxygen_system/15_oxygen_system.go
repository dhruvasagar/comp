package main

import (
	"fmt"

	"github.com/dhruvasagar/comp/adventofcode/2019/util"
)

type Direction int

const (
	north = iota + 1
	south
	west
	east
)

func (d Direction) reverse() Direction {
	switch d {
	case north:
		return south
	case south:
		return north
	case west:
		return east
	case east:
		return west
	}
	return -1
}

func (d Direction) next() Direction {
	switch d {
	case north:
		return east
	case south:
		return west
	case west:
		return north
	case east:
		return south
	}
	return -1
}

func (d Direction) String() string {
	switch d {
	case north:
		return "north"
	case south:
		return "south"
	case west:
		return "west"
	case east:
		return "east"
	}
	return ""
}

type Status int

const (
	walled = iota
	moved
	reachedOS
)

type PositionKind int

const (
	wall = iota
	empty
	os
)

func (pk PositionKind) String() string {
	if pk == wall {
		return "#"
	} else if pk == empty {
		return " "
	} else if pk == os {
		return "●"
	}
	return ""
}

type Position struct {
	x, y int
	kind PositionKind
}

func (p Position) String() string {
	return fmt.Sprintf("%d,%d", p.x, p.y)
}

func (p Position) log() string {
	return fmt.Sprintf("%d,%d,%s", p.x, p.y, p.kind)
}

func (p *Position) move(direction Direction) *Position {
	switch direction {
	case north:
		return &Position{x: p.x, y: p.y - 1}
	case south:
		return &Position{x: p.x, y: p.y + 1}
	case west:
		return &Position{x: p.x - 1, y: p.y}
	case east:
		return &Position{x: p.x + 1, y: p.y}
	}
	return nil
}

func (p Position) distance(pos Position) int {
	return (p.x - pos.x) + (p.y - pos.y)
}

type Robot struct {
	position  *Position
	direction Direction
	space     map[string]*Position
	history   map[string]int
}

func (r *Robot) chooseDirection() Direction {
	// Continue in the current direction if unexplored
	position := r.position.move(r.direction)
	key := fmt.Sprintf("%s", position)
	if _, ok := r.space[key]; !ok {
		return r.direction
	}

	// Choose any direction that is unexplored
	direction := r.direction.next()
	for direction != r.direction {
		position := r.position.move(direction)
		key := fmt.Sprintf("%s", position)
		if _, ok := r.space[key]; !ok {
			return direction
		}
		direction = direction.next()
	}

	// If no unexplored neighbors remain, look for one that's least travelled
	emptyPositions := make(map[Direction]*Position)
	direction = r.direction

	for i := r.direction; i < r.direction+4; i++ {
		position := r.position.move(direction)
		key := fmt.Sprintf("%s", position)
		p, ok := r.space[key]
		if ok && p.kind == empty {
			emptyPositions[direction] = p
		}
		direction = direction.next()
	}

	if len(emptyPositions) > 0 {
		minHistory := 10
		minHistoryDirection := Direction(north)
		for d, p := range emptyPositions {
			key := fmt.Sprintf("%s", p)
			c := r.history[key]
			if minHistory > c {
				minHistory = c
				minHistoryDirection = d
			}
		}
		return minHistoryDirection
	}

	return north
}

func (r *Robot) chooseOriginDirection() Direction {
	fmt.Println("robot position: ", r.position.log())
	origin := Position{x: 0, y: 0}
	var direction, direction1, direction2 Direction
	var position *Position
	var key string
	emptyPositions := make(map[Direction]*Position)

	if origin.x > r.position.x {
		direction = east
	} else if origin.x < r.position.x {
		direction = west
	}
	if direction != 0 {
		position = r.position.move(direction)
		key = fmt.Sprintf("%s", position)
		p, ok := r.space[key]
		if !ok {
			fmt.Printf("returning direction: %d\n", direction)
			return direction
		} else if p.kind == empty {
			emptyPositions[direction] = p
		}
		direction1 = direction
	}

	if origin.y > r.position.y {
		direction = south
	} else if origin.y < r.position.y {
		direction = north
	}
	position = r.position.move(direction)
	key = fmt.Sprintf("%s", position)
	if direction != 0 {
		p, ok := r.space[key]
		if !ok {
			fmt.Println("returning direction: 2", direction)
			return direction
		} else if p.kind == empty {
			emptyPositions[direction] = p
		}
		direction2 = direction
	}

	if direction1 != 0 {
		direction = direction1.reverse()
		position = r.position.move(direction)
		key = fmt.Sprintf("%s", position)
		p, ok := r.space[key]
		if !ok {
			fmt.Println("returning direction: 3", direction)
			return direction
		} else if p.kind == empty {
			emptyPositions[direction] = p
		}
	}

	if direction2 != 0 {
		direction = direction2.reverse()
		position = r.position.move(direction)
		key = fmt.Sprintf("%s", position)
		p, ok := r.space[key]
		if !ok {
			fmt.Println("returning direction: 4", direction)
			return direction
		} else if p.kind == empty {
			emptyPositions[direction] = p
		}
	}

	fmt.Println("EmptyPositions: ", len(emptyPositions))
	if len(emptyPositions) > 0 {
		minHistory := 100
		minHistoryDirection := Direction(north)
		for d, p := range emptyPositions {
			key := fmt.Sprintf("%s", p)
			c := r.history[key]
			fmt.Printf("history count: %d, of position %s\n", c, p)
			if minHistory > c {
				minHistory = c
				minHistoryDirection = d
			}
		}
		return minHistoryDirection
	}

	return north
}

func (r Robot) String() string {
	var minX, maxX, minY, maxY int
	for _, p := range r.space {
		if minX > p.x {
			minX = p.x
		}
		if maxX < p.x {
			maxX = p.x
		}
		if minY > p.y {
			minY = p.y
		}
		if maxY < p.y {
			maxY = p.y
		}
	}
	rs := ""
	pad := 2
	for y := minY - pad; y <= maxY+pad; y++ {
		for x := minX - pad; x < maxX+pad; x++ {
			key := fmt.Sprintf("%d,%d", x, y)
			if x == 0 && y == 0 {
				rs += "S"
			} else if r.position.x == x && r.position.y == y {
				rs += "D"
			} else {
				p, ok := r.space[key]
				if ok {
					rs += fmt.Sprintf("%s", p.kind)
				} else {
					rs += "█"
				}
			}
		}
		rs += fmt.Sprintln()
	}
	return rs
}

func (r *Robot) findOxygenSystem(computer *util.Computer) {
	done := false
	for {
		direction := r.chooseDirection()
		position := r.position.move(direction)
		key := fmt.Sprintf("%s", position)

		<-computer.Waiting
		computer.Type(int(direction))

		out, ok := computer.Read()
		if !ok {
			break
		}
		status := Status(out)
		switch status {
		case walled:
			position.kind = wall
			r.space[key] = position
		case moved:
			position.kind = empty
			r.position = position
			r.space[key] = position
			r.history[key] = r.history[key] + 1
			r.direction = direction
		case reachedOS:
			position.kind = os
			r.position = position
			r.space[key] = position
			r.history[key] = r.history[key] + 1
			r.direction = direction
			fmt.Println(r)
			done = true
			break
		}
		if done {
			break
		}
	}
	fmt.Println("final position:", r.position.log())
}

func (r *Robot) findBestPathLength(computer *util.Computer) int {
	count := 1
	for {
		direction := r.chooseOriginDirection()
		fmt.Println("Direction:", direction)
		position := r.position.move(direction)
		fmt.Printf("position: %s in direction: %s\n", position, direction)
		if position.x == 0 && position.y == 0 {
			break
		}
		key := fmt.Sprintf("%s", position)

		<-computer.Waiting
		computer.Type(int(direction))

		out, ok := computer.Read()
		if !ok {
			break
		}
		status := Status(out)
		fmt.Println("Got status:", status)
		switch status {
		case walled:
			position.kind = wall
			r.space[key] = position
		case moved:
			position.kind = empty
			r.position = position
			r.space[key] = position
			r.history[key] = r.history[key] + 1
			r.direction = direction
		}
		fmt.Println(r)
		count++
	}
	return count
}

func (r *Robot) clearHistory() {
	r.history = make(map[string]int)
}

func NewRobot() *Robot {
	return &Robot{
		direction: north,
		position:  &Position{x: 0, y: 0},
		space:     make(map[string]*Position),
		history:   make(map[string]int),
	}
}

func part1() {
	instructions := util.ReadIntcodeInstructions()
	computer := util.NewComputer(instructions, 1)
	defer computer.Close()
	go computer.Run()

	robot := NewRobot()
	robot.findOxygenSystem(computer)

	robot.clearHistory()

	fmt.Println(robot.findBestPathLength(computer))
}

func main() {
	part1()
}
