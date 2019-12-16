package main

import (
	"fmt"
	"testing"
)

type Input struct {
	color     Color
	direction Direction
}

func TestRobot(t *testing.T) {
	inputs := []Input{
		Input{
			color:     Color(1),
			direction: Direction(0),
		},
		Input{
			color:     Color(0),
			direction: Direction(0),
		},
		Input{
			color:     Color(1),
			direction: Direction(0),
		},
		Input{
			color:     Color(1),
			direction: Direction(0),
		},
		Input{
			color:     Color(0),
			direction: Direction(1),
		},
		Input{
			color:     Color(1),
			direction: Direction(0),
		},
		Input{
			color:     Color(1),
			direction: Direction(0),
		},
	}
	robot := NewRobot()
	for _, input := range inputs {
		robot.PaintAndMove(input.color, input.direction)
	}
	fmt.Println("Robot: ", robot)
	result := robot.Length()
	if result != 6 {
		t.Errorf("Got: %d, want: 6", result)
	}
}
