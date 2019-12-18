package main

import (
	"fmt"
	"math"
	"regexp"
	"strconv"
	"strings"

	"github.com/dhruvasagar/adventofcode/2019/util"
)

type Position struct {
	x, y, z int
}

func (p Position) String() string {
	return fmt.Sprintf("pos=<x=%d, y=%d, z=%d>", p.x, p.y, p.z)
}

func (p Position) energy() int {
	return int(math.Abs(float64(p.x)) + math.Abs(float64(p.y)) + math.Abs(float64(p.z)))
}

type Velocity struct {
	x, y, z int
}

func (v Velocity) String() string {
	return fmt.Sprintf("vel=<x=%d, y=%d, z=%d>", v.x, v.y, v.z)
}

func (v Velocity) energy() int {
	return int(math.Abs(float64(v.x)) + math.Abs(float64(v.y)) + math.Abs(float64(v.z)))
}

func (v *Velocity) compute(p1, p2 Position) {
	if p1.x < p2.x {
		v.x++
	} else if p1.x > p2.x {
		v.x--
	}

	if p1.y < p2.y {
		v.y++
	} else if p1.y > p2.y {
		v.y--
	}

	if p1.z < p2.z {
		v.z++
	} else if p1.z > p2.z {
		v.z--
	}
}

type Moon struct {
	name     string
	position *Position
	velocity *Velocity

	positionHistory []Position
}

func (m Moon) String() string {
	return fmt.Sprintf("moon-%s %s, %s", m.name, m.position, m.velocity)
}

func (m *Moon) reset() {
	if len(m.positionHistory) > 0 {
		m.position = &m.positionHistory[0]
		m.velocity = &Velocity{}
		m.positionHistory = []Position{}
	}
}

func (m *Moon) applyGravity(moons []*Moon) {
	for _, moon := range moons {
		m.velocity.compute(*m.position, *moon.position)
	}
}

func (m *Moon) updatePosition() {
	if len(m.positionHistory) == 0 {
		m.positionHistory = append(m.positionHistory, *m.position)
	}
	m.position.x += m.velocity.x
	m.position.y += m.velocity.y
	m.position.z += m.velocity.z
	m.positionHistory = append(m.positionHistory, *m.position)
}

func (m *Moon) potEnergy() int {
	energy := m.position.energy()
	return energy
}

func (m *Moon) kinEnergy() int {
	energy := m.velocity.energy()
	return energy
}

func (m *Moon) energy() int {
	return m.potEnergy() * m.kinEnergy()
}

type System []*Moon

func (s System) String() string {
	res := ""
	for _, m := range s {
		res += fmt.Sprintln(m)
	}
	return res
}

func (s System) indexOf(moon *Moon) int {
	for i, m := range s {
		if m == moon {
			return i
		}
	}
	return -1
}

func (s System) neighbors(m *Moon) []*Moon {
	n := make([]*Moon, len(s))
	copy(n, s)
	index := s.indexOf(m)
	n[index] = n[len(n)-1]
	n = n[:len(n)-1]
	return n
}

func (s System) step(n int) {
	for i := 0; i < n; i++ {
		for _, m := range s {
			m.applyGravity(s.neighbors(m))
		}
		for _, m := range s {
			m.updatePosition()
		}
	}
}

func (s System) findSteps() int {
	n := 0
	history := make(map[string]int)
	for {
		for _, m := range s {
			m.applyGravity(s.neighbors(m))
		}
		for _, m := range s {
			m.updatePosition()
		}
		key := fmt.Sprintln(s)
		if _, ok := history[key]; ok {
			break
		}
		history[key] = 1
		n++
	}
	return n
}

func (s System) findPeriod() int {
	n := 1000
	fmt.Printf("running %d steps to find periods\n", n)
	s.step(n)
	positionXs := make([][]int, n+1)
	positionYs := make([][]int, n+1)
	positionZs := make([][]int, n+1)
	for i, m := range s {
		for j, h := range m.positionHistory {
			if len(positionXs[j]) == 0 {
				positionXs[j] = make([]int, len(s))
			}
			if len(positionYs[j]) == 0 {
				positionYs[j] = make([]int, len(s))
			}
			if len(positionZs[j]) == 0 {
				positionZs[j] = make([]int, len(s))
			}
			positionXs[j][i] = h.x
			positionYs[j][i] = h.y
			positionZs[j][i] = h.z
		}
	}
	periodX := period(positionXs)
	periodY := period(positionYs)
	periodZ := period(positionZs)
	fmt.Printf("periods X:%d, Y:%d, Z:%d\n", periodX, periodY, periodZ)
	if periodX == -1 || periodY == -1 || periodZ == -1 {
		return s.findPeriod()
	}
	return lcm(periodX, lcm(periodY, periodZ))
}

func gcd(a, b int) int {
	if a == 0 {
		return b
	}
	if b == 0 {
		return a
	}
	if a < b {
		return gcd(a, b-a)
	}
	return gcd(b, a-b)
}

func lcm(a, b int) int {
	return (a * b) / gcd(a, b)
}

func period(as [][]int) int {
	cache := make(map[string]int)
	for i, a := range as {
		key := strings.Trim(strings.Join(strings.Fields(fmt.Sprint(a)), ","), "[]")
		if j, ok := cache[key]; ok {
			return i - j
		}
		cache[key] = i
	}
	return -1
}

func (s System) energy() int {
	energy := 0
	for _, m := range s {
		energy += m.energy()
	}
	return energy
}

func (s System) reset() {
	for _, m := range s {
		m.reset()
	}
}

func parseInput() System {
	s := System{}
	lines := util.ReadLines()
	posRegex := regexp.MustCompile(`^<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>$`)
	for i, line := range lines {
		rposes := posRegex.FindAllStringSubmatch(line, -1)[0]
		x, _ := strconv.Atoi(rposes[1])
		y, _ := strconv.Atoi(rposes[2])
		z, _ := strconv.Atoi(rposes[3])
		moon := &Moon{
			name:     fmt.Sprintf("%d", i),
			position: &Position{x: x, y: y, z: z},
			velocity: &Velocity{},
		}
		s = append(s, moon)
	}
	return s
}

func part1(system System) {
	system.reset()
	system.step(1000)
	fmt.Println(system.energy())
}

func part2(system System) {
	// Not right
	system.reset()
	fmt.Println(system.findPeriod())
}

func main() {
	system := parseInput()
	part1(system)
	part2(system)
}
