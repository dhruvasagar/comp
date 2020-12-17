package main

import (
	"bufio"
	"fmt"
	"math"
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

type Coord struct {
	x, y, z int
}

func (c Coord) String() string {
	return fmt.Sprintf("%d,%d,%d", c.x, c.y, c.z)
}

type State rune

func (s State) String() string {
	if s == Active {
		return "active"
	}
	return "inactive"
}

const (
	Active   State = '#'
	Inactive       = '.'
)

type Cube struct {
	coord Coord
	state State
}

func (c *Cube) String() string {
	return fmt.Sprintf("%s: %s", c.coord, c.state)
}

func (c *Cube) Print() string {
	return fmt.Sprint(string(c.state))
}

func (c *Cube) IsActive() bool {
	return c.state == Active
}

func (c *Cube) AdjacentCubes() []*Cube {
	r := []*Cube{}
	for i := -1; i < 2; i++ {
		for j := -1; j < 2; j++ {
			for k := -1; k < 2; k++ {
				if i == 0 && j == 0 && k == 0 {
					continue
				}
				crd := Coord{
					x: c.coord.x + i,
					y: c.coord.y + j,
					z: c.coord.z + k,
				}
				k := fmt.Sprint(crd)
				cb, ok := cubeMap[k]
				if ok {
					r = append(r, cb)
				} else {
					r = append(r, &Cube{coord: crd, state: Inactive})
				}
			}
		}
	}
	return r
}

var cubeMap = make(map[string]*Cube)

func parseInput(lines []string) []*Cube {
	cubes := []*Cube{}
	for i, line := range lines {
		for j, c := range line {
			cube := &Cube{coord: Coord{i, j, 0}, state: State(c)}
			key := fmt.Sprint(cube.coord)
			cubeMap[key] = cube
			cubes = append(cubes, cube)
		}
	}
	return cubes
}

const NumCycles = 6

func ActiveLen(cubes []*Cube) int {
	count := 0
	for _, cube := range cubes {
		if cube.IsActive() {
			count++
		}
	}
	return count
}

func SimulateCycle(cubes []*Cube) []*Cube {
	r := []*Cube{}

	for _, cube := range cubes {
		acubes := cube.AdjacentCubes()
		fmt.Println(len(acubes))
		al := ActiveLen(acubes)
		if cube.IsActive() {
			if al != 2 && al != 3 {
				cube.state = Inactive
			}
		} else {
			if al == 3 {
				cube.state = Active
			}
		}
		r = append(r, cube)
	}
	return r
}

func printCubes(cubes []*Cube) {
	sqrt := int(math.Sqrt(float64(len(cubes))))
	for i, cube := range cubes {
		fmt.Printf(cube.Print())
		if i%sqrt == sqrt-1 {
			fmt.Println()
		}
	}
	fmt.Println()
}

func part1(cubes []*Cube) int {
	printCubes(cubes)
	cubes = SimulateCycle(cubes)
	fmt.Println()
	printCubes(cubes)
	return 0
}

func main() {
	cubes := parseInput(readInput())
	fmt.Println(part1(cubes))
}
