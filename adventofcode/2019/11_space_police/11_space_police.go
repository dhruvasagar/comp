package main

import (
	"fmt"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

type Color int

func (c Color) String() string {
	switch c {
	case black:
		return "Black"
	case white:
		return "White"
	}
	return ""
}

const (
	black = iota
	white
	empty
)

type Direction int

const (
	left = iota
	right
)

func (d Direction) String() string {
	switch d {
	case left:
		return "Left"
	case right:
		return "Right"
	}
	return ""
}

type Orientation int

const (
	upward = iota
	leftward
	rightward
	downward
)

func (o Orientation) String() string {
	switch o {
	case upward:
		return "^"
	case leftward:
		return "<"
	case rightward:
		return ">"
	case downward:
		return "v"
	}
	return ""
}

type Position struct {
	x, y int
}

func (p Position) String() string {
	return fmt.Sprintf("x:%d,y:%d", p.x, p.y)
}

type Panel struct {
	color    Color
	position Position
}

func (p Panel) String() string {
	return fmt.Sprintf("color: %v, position: %v", p.color, p.position)
}

type Robot struct {
	panels        []*Panel
	position      Position
	orientation   Orientation
	visitedPanels map[string]*Panel
	paintedPanels map[string]*Panel
}

func NewRobot() *Robot {
	robot := &Robot{
		visitedPanels: make(map[string]*Panel),
		paintedPanels: make(map[string]*Panel),
	}
	position := Position{x: 0, y: 0}
	robot.Update(position, upward)
	return robot
}

func (r *Robot) String() string {
	var minX, maxX, minY, maxY int
	for _, p := range r.panels {
		if p.position.x < minX {
			minX = p.position.x
		}
		if p.position.x > maxX {
			maxX = p.position.x
		}
		if p.position.y < minY {
			minY = p.position.y
		}
		if p.position.y > maxY {
			maxY = p.position.y
		}
	}
	visual := "\n"
	for y := minY - 1; y <= maxY+1; y++ {
		for x := minX - 1; x <= maxX+1; x++ {
			position := Position{x: x, y: y}
			panel := r.Panel(position)
			if r.position.x == x && r.position.y == y {
				visual += fmt.Sprintf("%s", r.orientation)
			} else if panel.color == black {
				visual += "."
			} else if panel.color == white {
				visual += "#"
			} else if panel.color == empty {
				visual += "█"
			}
		}
		visual += "\n"
	}
	return visual
}

func (r *Robot) Update(position Position, orientation Orientation) {
	key := fmt.Sprintln(position)
	panel, ok := r.visitedPanels[key]
	if !ok {
		panel = &Panel{color: black, position: position}
		r.visitedPanels[key] = panel
	}
	r.panels = append(r.panels, panel)
	r.position = panel.position
	r.orientation = orientation
}

func (r *Robot) CurrentPanel() *Panel {
	key := fmt.Sprintln(r.position)
	return r.visitedPanels[key]
}

func (r *Robot) Panel(position Position) *Panel {
	key := fmt.Sprintln(position)
	panel, ok := r.visitedPanels[key]
	if !ok {
		return &Panel{position: position, color: empty}
	}
	return panel
}

func (r *Robot) Paint(color Color) {
	panel := r.CurrentPanel()
	panel.color = color

	key := fmt.Sprintln(panel.position)
	r.paintedPanels[key] = panel
}

func (r *Robot) Move(direction Direction) {
	var position Position
	var orientation Orientation
	switch direction {
	case left:
		switch r.orientation {
		case upward:
			position = Position{x: r.position.x - 1, y: r.position.y}
			orientation = leftward
		case leftward:
			position = Position{x: r.position.x, y: r.position.y + 1}
			orientation = downward
		case rightward:
			position = Position{x: r.position.x, y: r.position.y - 1}
			orientation = upward
		case downward:
			position = Position{x: r.position.x + 1, y: r.position.y}
			orientation = rightward
		}
	case right:
		switch r.orientation {
		case upward:
			position = Position{x: r.position.x + 1, y: r.position.y}
			orientation = rightward
		case leftward:
			position = Position{x: r.position.x, y: r.position.y - 1}
			orientation = upward
		case rightward:
			position = Position{x: r.position.x, y: r.position.y + 1}
			orientation = downward
		case downward:
			position = Position{x: r.position.x - 1, y: r.position.y}
			orientation = leftward
		}
	}
	r.Update(position, orientation)
}

func (r *Robot) PaintAndMove(color Color, direction Direction) {
	r.Paint(color)
	r.Move(direction)
}

func (r *Robot) Length() int {
	return len(r.paintedPanels)
}

func part1() {
	instructions := util.ReadIntcodeInstructions()
	computer := util.NewComputer(instructions, 0)
	defer computer.Close()

	robot := NewRobot()

	computer.Type(0)
	go computer.Run()

	for {
		// fmt.Println("robot: ", robot)
		color, ok := computer.Read()
		if !ok {
			break
		}
		direction, ok := computer.Read()
		if !ok {
			break
		}
		// fmt.Println("Panel: ", panel)
		robot.PaintAndMove(Color(color), Direction(direction))

		panel := robot.CurrentPanel()
		computer.Type(int(panel.color))
	}

	fmt.Println(robot)
	fmt.Println(robot.Length())
}

func part2() {
	instructions := util.ReadIntcodeInstructions()
	computer := util.NewComputer(instructions, 0)
	defer computer.Close()

	robot := NewRobot()

	computer.Type(1)
	go computer.Run()

	for {
		// fmt.Println("robot: ", robot)
		color, ok := computer.Read()
		if !ok {
			break
		}
		direction, ok := computer.Read()
		if !ok {
			break
		}
		// fmt.Println("Panel: ", panel)
		robot.PaintAndMove(Color(color), Direction(direction))

		panel := robot.CurrentPanel()
		computer.Type(int(panel.color))
	}

	fmt.Println(robot)
	fmt.Println(robot.Length())
}

func main() {
	part1()
	part2()
}
