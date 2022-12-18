package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Cube struct {
	x, y, z int
}

func parseInput(lines []string) []Cube {
	cubes := []Cube{}
	for _, line := range lines {
		coords := strings.Split(line, ",")
		x, _ := strconv.Atoi(coords[0])
		y, _ := strconv.Atoi(coords[1])
		z, _ := strconv.Atoi(coords[2])
		cubes = append(cubes, Cube{x, y, z})
	}
	return cubes
}

func Abs(a int) int {
	if a < 0 {
		return -a
	}

	return a
}

func (c Cube) isAdjacent(cube Cube) bool {
	diff := Abs(c.x-cube.x) + Abs(c.y-cube.y) + Abs(c.z-cube.z)
	return diff == 1
}

func (c Cube) countAdjacent(cubes []Cube) int {
	cnt := 0
	for _, cube := range cubes {
		if c == cube {
			continue
		}

		if c.isAdjacent(cube) {
			cnt += 1
		}
	}
	return cnt
}

func (c Cube) String() string {
	return fmt.Sprintf("cube: [%d,%d,%d]", c.x, c.y, c.z)
}

func part1(cubes []Cube) int {
	adjCount := 0
	for _, cube := range cubes {
		adjCount += cube.countAdjacent(cubes)
	}
	return len(cubes)*6 - adjCount
}

func (c Cube) neighbors() [6]Cube {
	return [6]Cube{
		{c.x - 1, c.y, c.z},
		{c.x + 1, c.y, c.z},
		{c.x, c.y - 1, c.z},
		{c.x, c.y + 1, c.z},
		{c.x, c.y, c.z - 1},
		{c.x, c.y, c.z + 1},
	}
}

func boundingCube(cubes []Cube) Cube {
	c := Cube{}
	for _, cube := range cubes {
		if cube.x > c.x {
			c.x = cube.x
		}
		if cube.y > c.y {
			c.y = cube.y
		}
		if cube.z > c.z {
			c.z = cube.z
		}
	}
	return c
}

func findEnclosedCubes(cubes []Cube) []Cube {
	bcube := boundingCube(cubes)
	ecubes := []Cube{}
	queue := []Cube{{0, 0, 0}}
	vis := make(map[Cube]bool)
	cubesMap := make(map[Cube]bool)
	for _, cube := range cubes {
		cubesMap[cube] = true
	}
	for len(queue) > 0 {
		top := queue[0]
		queue = queue[1:]

		if top.x < 0 || top.y < 0 || top.z < 0 || top.x > bcube.x || top.y > bcube.y || top.z > bcube.z {
			continue
		}
		if cubesMap[top] {
			continue
		}
		if vis[top] {
			continue
		}
		vis[top] = true
		for _, n := range top.neighbors() {
			if cubesMap[n] {
				continue
			}
			if vis[n] {
				continue
			}
			queue = append(queue, n)
		}
	}
	for x := 0; x < bcube.x; x++ {
		for y := 0; y < bcube.y; y++ {
			for z := 0; z < bcube.z; z++ {
				c := Cube{x, y, z}
				if vis[c] {
					continue
				}
				if cubesMap[c] {
					continue
				}
				ecubes = append(ecubes, c)
			}
		}
	}
	return ecubes
}

func internalAdjacencyCount(cubes []Cube, ecubes []Cube) int {
	ntouches := 0
	for _, cube := range cubes {
		ntouches += cube.countAdjacent(ecubes)
	}
	return ntouches
}

func part2(cubes []Cube) int {
	adjCount := 0
	for _, cube := range cubes {
		adjCount += cube.countAdjacent(cubes)
	}
	ecubes := findEnclosedCubes(cubes)
	etouchCount := internalAdjacencyCount(cubes, ecubes)
	return len(cubes)*6 - adjCount - etouchCount
}

func main() {
	s := time.Now()
	cubes := parseInput(readInput())
	s1 := time.Now()
	fmt.Println(part1(cubes))
	e1 := time.Since(s1)
	s2 := time.Now()
	fmt.Println(part2(cubes))
	e2 := time.Since(s2)
	e := time.Since(s)
	fmt.Printf("Time for part1: %s, part2: %s, total: %s\n", e1, e2, e)
}
