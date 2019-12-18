package main

import "testing"

func TestPeriod(t *testing.T) {
	data := []struct {
		in  [][]int
		out int
	}{
		{
			[][]int{
				[]int{2, 3, 4, 5},
				[]int{1, 2, 3, 4},
				[]int{4, 3, 4, 5},
				[]int{3, 4, 5, 6},
				[]int{1, 2, 3, 4},
			}, 3,
		},
	}
	for _, d := range data {
		res := period(d.in)
		if res != d.out {
			t.Errorf("Got %d, expected %d", res, d.out)
		}
	}
}

func TestGCD(t *testing.T) {
	data := []struct {
		in  []int
		out int
	}{
		{[]int{2, 4}, 2},
		{[]int{6, 4}, 2},
		{[]int{4, 8}, 4},
	}
	for _, d := range data {
		res := gcd(d.in[0], d.in[1])
		if res != d.out {
			t.Errorf("Got %d, expected %d", res, d.out)
		}
	}
}
