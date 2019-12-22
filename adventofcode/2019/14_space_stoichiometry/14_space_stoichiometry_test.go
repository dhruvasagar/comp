package main

import "testing"

func TestParseChemical(t *testing.T) {
	data := []struct {
		in  string
		res Chemical
	}{
		{"10 ORE", Chemical{"ORE", 10}},
		{"7 A", Chemical{"A", 7}},
		{"1 B", Chemical{"B", 1}},
		{"1 B ", Chemical{"B", 1}},
	}
	for _, d := range data {
		res := parseChemical(d.in)
		if res != d.res {
			t.Errorf("Chemical parsing failed, expected %v, got %v", d.res, res)
		}
	}
}

func TestParseReactants(t *testing.T) {
	data := []struct {
		in        string
		reactants Reactants
	}{
		{"7 A, 1 B", Reactants{Chemical{"A", 7}, Chemical{"B", 1}}},
		{"7 A, 1 C", Reactants{Chemical{"A", 7}, Chemical{"C", 1}}},
		{"7 A, 1 D", Reactants{Chemical{"A", 7}, Chemical{"D", 1}}},
		{"7 A, 1 E ", Reactants{Chemical{"A", 7}, Chemical{"E", 1}}},
	}
	for _, d := range data {
		res := parseReactants(d.in)
		if len(res) != 2 && res[0] != d.reactants[0] && res[1] != d.reactants[1] {
			t.Errorf("Reactants parsing failed, expected %v, got %v", d.reactants, res)
		}
	}
}
