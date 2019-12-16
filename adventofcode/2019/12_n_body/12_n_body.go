package main

import (
	"fmt"
	"math"
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
}

func (m Moon) String() string {
	return fmt.Sprintf("moon-%s %s, %s", m.name, m.position, m.velocity)
}

func (m *Moon) applyGravity(moons []*Moon) {
	for _, moon := range moons {
		m.velocity.compute(*m.position, *moon.position)
	}
}

func (m *Moon) updatePosition() {
	m.position.x += m.velocity.x
	m.position.y += m.velocity.y
	m.position.z += m.velocity.z
}

func (m *Moon) potEnergy() int {
	energy := m.position.energy()
	fmt.Printf("potential energy of moon %s: %d\n", m.name, energy)
	return energy
}

func (m *Moon) kinEnergy() int {
	energy := m.velocity.energy()
	fmt.Printf("kinetic energy of moon %s: %d\n", m.name, energy)
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

func (s System) energy() int {
	energy := 0
	for _, m := range s {
		energy += m.energy()
	}
	return energy
}

func main() {
	moon1 := &Moon{
		name:     "1",
		position: &Position{x: 1, y: 2, z: -9},
		velocity: &Velocity{},
	}
	moon2 := &Moon{
		name:     "2",
		position: &Position{x: -1, y: -9, z: -4},
		velocity: &Velocity{},
	}
	moon3 := &Moon{
		name:     "3",
		position: &Position{x: 17, y: 6, z: 8},
		velocity: &Velocity{},
	}
	moon4 := &Moon{
		name:     "3",
		position: &Position{x: 12, y: 4, z: 2},
		velocity: &Velocity{},
	}
	system := System{moon1, moon2, moon3, moon4}
	system.step(1000)
	fmt.Println(system)
	fmt.Println(system.energy())
}
