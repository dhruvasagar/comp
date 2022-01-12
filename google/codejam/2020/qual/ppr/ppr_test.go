package main

import "testing"

func TestIntervalHasOverlap(t *testing.T) {
	testCases := []struct {
		i       Interval
		i2      Interval
		overlap bool
	}{
		{i: Interval{1, 3}, i2: Interval{2, 4}, overlap: true},
		{i: Interval{1, 2}, i2: Interval{1, 4}, overlap: true},
		{i: Interval{1, 4}, i2: Interval{3, 4}, overlap: true},
		{i: Interval{1, 4}, i2: Interval{2, 3}, overlap: true},
		{i: Interval{2, 3}, i2: Interval{1, 4}, overlap: true},
		{i: Interval{2, 4}, i2: Interval{1, 3}, overlap: true},
		{i: Interval{1, 2}, i2: Interval{2, 3}, overlap: false},
		{i: Interval{2, 4}, i2: Interval{1, 2}, overlap: false},
	}

	for _, tcase := range testCases {
		if tcase.i.hasOverlap(tcase.i2) != tcase.overlap {
			t.Errorf("TestCase %v, expected: %v, got: %v\n", tcase, tcase.overlap, tcase.i.hasOverlap(tcase.i2))
		}
	}
}

func TestScheduleHasOverlap(t *testing.T) {
	testCases := []struct {
		i       Interval
		s       Schedule
		overlap bool
	}{
		{Interval{1, 3}, Schedule{Interval{1, 2}, Interval{4, 5}}, true},
		{Interval{1, 3}, Schedule{Interval{3, 4}, Interval{4, 5}}, false},
	}

	for _, tcase := range testCases {
		if tcase.s.hasOverlap(tcase.i) != tcase.overlap {
			t.Errorf("TestCase %v, expected: %v, got: %v\n", tcase, tcase.overlap, tcase.s.hasOverlap(tcase.i))
		}
	}
}
