package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

var sensorRegex = regexp.MustCompile(`Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)`)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func parseSensor(line string) Sensor {
	caps := sensorRegex.FindStringSubmatch(line)
	sx, _ := strconv.Atoi(caps[1])
	sy, _ := strconv.Atoi(caps[2])
	bx, _ := strconv.Atoi(caps[3])
	by, _ := strconv.Atoi(caps[4])
	spos := Point{x: sx, y: sy}
	bpos := Point{x: bx, y: by}
	return Sensor{
		pos:   spos,
		bpos:  bpos,
		mdist: mdist(spos, bpos),
	}
}

func parseInput(lines []string) map[Point]Sensor {
	res := make(map[Point]Sensor)
	for _, line := range lines {
		s := parseSensor(line)
		res[s.pos] = s
	}
	return res
}

type Point struct {
	x, y int
}

func abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

func mdist(p1 Point, p2 Point) int {
	return abs(p1.x-p2.x) + abs(p1.y-p2.y)
}

type Sensor struct {
	pos   Point
	bpos  Point
	mdist int
}

func no_beacon(sensors map[Point]Sensor, point Point) bool {
	if _, ok := sensors[point]; ok {
		return false
	}

	for p, s := range sensors {
		if point == s.bpos {
			return false
		}

		if mdist(p, point) <= s.mdist {
			return true
		}
	}
	return false
}

func count_point_no_beacon(sensors map[Point]Sensor, xmin, xmax, y int) int {
	cnt := 0
	for i := xmin; i <= xmax; i++ {
		p := Point{x: i, y: y}
		if no_beacon(sensors, p) {
			cnt += 1
		}
	}
	return cnt
}

func part1(sensors map[Point]Sensor) int {
	xmin := 0
	xmax := 0
	for p, s := range sensors {
		if xmin > p.x-s.mdist {
			xmin = p.x - s.mdist
		}
		if xmax < p.x+s.mdist {
			xmax = p.x + s.mdist
		}
	}
	return count_point_no_beacon(sensors, xmin, xmax, 2000000)
}

func tuning_frequency(p Point) int64 {
	return int64(p.x)*4000000 + int64(p.y)
}

func part2(sensors map[Point]Sensor) int64 {
	pchan := make(chan Point)
	pdiffs := [][]int{{-1, -1}, {-1, 1}, {1, -1}, {1, 1}}
	for _, s := range sensors {
		go func(c chan Point, s Sensor) {
			for dx := 0; dx <= (s.mdist + 1); dx++ {
				dy := s.mdist + 1 - dx
				for _, sxy := range pdiffs {
					x := s.pos.x + dx*sxy[0]
					y := s.pos.y + dy*sxy[1]
					point := Point{x, y}

					if x < 0 || x > 4000000 || y < 0 || y > 4000000 {
						continue
					}

					if !no_beacon(sensors, point) {
						c <- point
					}
				}
			}
		}(pchan, s)
	}
	point := <-pchan
	return tuning_frequency(point)
}

func main() {
	sensors := parseInput(readInput())
	fmt.Println(part1(sensors))
	fmt.Println(part2(sensors))
}
