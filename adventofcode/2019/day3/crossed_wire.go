package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x, y int
}

func (p Point) mdist(p2 Point) int {
	return int(math.Abs(float64(p.x-p2.x))) + int(math.Abs(float64(p.y-p2.y)))
}

type Path struct {
	points []Point
}

func (p Path) intersection(p2 Path) []Point {
	points := []Point{}
	for _, p1 := range p.points {
		for _, p2 := range p2.points {
			if p1 == p2 {
				points = append(points, p1)
			}
		}
	}
	return points
}

func (p Path) stepCount(point Point) int {
	for i, _point := range p.points {
		if point == _point {
			return i
		}
	}
	return -1
}

func NewPathFromMoves(pmoves string) *Path {
	start := Point{x: 0, y: 0}
	path := &Path{
		points: []Point{start},
	}
	moves := strings.Split(pmoves, ",")
	spoint := start
	for _, move := range moves {
		dirn := move[0:1]
		dist, _ := strconv.Atoi(move[1:])
		for i := 0; i < dist; i++ {
			var point Point
			if dirn == "U" {
				point.x = spoint.x
				point.y = spoint.y + 1
			} else if dirn == "D" {
				point.x = spoint.x
				point.y = spoint.y - 1
			} else if dirn == "L" {
				point.x = spoint.x - 1
				point.y = spoint.y
			} else if dirn == "R" {
				point.x = spoint.x + 1
				point.y = spoint.y
			}
			path.points = append(path.points, point)
			spoint = point
		}
	}
	return path
}

func movedLeft(p1, p2 Point) bool {
	return p2.x < p1.x
}

func movedRight(p1, p2 Point) bool {
	return p2.x > p1.x
}

func movedUp(p1, p2 Point) bool {
	return p2.y > p1.y
}

func movedDown(p1, p2 Point) bool {
	return p2.y < p1.y
}

func main() {
	reader := bufio.NewReader(os.Stdin)
	moves1, _ := reader.ReadString('\n')
	moves2, _ := reader.ReadString('\n')
	path1 := NewPathFromMoves(moves1)
	path2 := NewPathFromMoves(moves2)
	iPoints := path1.intersection(*path2)[1:]
	minDist := -1
	for _, iPoint := range iPoints {
		md := path1.stepCount(iPoint) + path2.stepCount(iPoint)
		if minDist < 0 || md < minDist {
			minDist = md
		}
	}
	fmt.Println(minDist)
}
