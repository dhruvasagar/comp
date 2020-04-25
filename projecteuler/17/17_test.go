package main

import "testing"

func TestCountLetters(t *testing.T) {
	testCases := []struct {
		n     int
		count int
	}{
		{1, 3},
		{11, 6},
		{20, 6},
		{21, 9},
		{100, 10},
		{342, 23},
		{115, 20},
	}

	for _, tcase := range testCases {
		count := countLettersInNumberWords(tcase.n)
		if count != tcase.count {
			t.Errorf("Wrong count for number: %d, expected: %d, got: %d", tcase.n, tcase.count, count)
		}
	}
}
