package main

import (
	"reflect"
	"testing"
)

func TestGroupAnagrams(t *testing.T) {
	testCases := []struct {
		strs   []string
		groups [][]string
	}{
		{
			strs: []string{"eat", "tea", "tan", "ate", "nat", "bat"},
			groups: [][]string{
				[]string{"ate", "eat", "tea"},
				[]string{"nat", "tan"},
				[]string{"bat"},
			},
		},
		{
			strs: []string{"rag", "orr", "err", "bad", "foe", "ivy", "tho", "gem", "len", "cat", "ron", "ump", "nev", "cam", "pen", "dis", "rob", "tex", "sin", "col", "buy", "say", "big", "wed", "eco", "bet", "fog", "buy", "san", "kid", "lox", "sen", "ani", "mac", "eta", "wis", "pot", "sid", "dot", "ham", "gay", "oar", "sid", "had", "paw", "sod", "sop"},
			groups: [][]string{
				[]string{"sop"},
				[]string{"dis", "sid", "sid"},
				[]string{"sin"},
				[]string{"gem"},
				[]string{"pen"},
				[]string{"nev"},
				[]string{"oar"},
				[]string{"rob"},
				[]string{"ivy"},
				[]string{"eco"},
				[]string{"bad"},
				[]string{"ron"},
				[]string{"rag"},
				[]string{"bet"},
				[]string{"lox"},
				[]string{"buy", "buy"},
				[]string{"sod"},
				[]string{"tho"},
				[]string{"cam", "mac"},
				[]string{"say"},
				[]string{"orr"},
				[]string{"sen"},
				[]string{"pot"},
				[]string{"foe"},
				[]string{"fog"},
				[]string{"err"},
				[]string{"gay"},
				[]string{"len"},
				[]string{"wed"},
				[]string{"wis"},
				[]string{"big"},
				[]string{"col"},
				[]string{"had"},
				[]string{"tex"},
				[]string{"san"},
				[]string{"ump"},
				[]string{"kid"},
				[]string{"cat"},
				[]string{"ani"},
				[]string{"eta"},
				[]string{"dot"},
				[]string{"ham"},
				[]string{"paw"},
			},
		},
	}
	for _, tcase := range testCases {
		grps := groupAnagrams(tcase.strs)
		if len(grps) != len(tcase.groups) {
			t.Errorf("Test Failed %v, expected: %v, got: %v", tcase, tcase.groups, grps)
		}
		if reflect.DeepEqual(grps, tcase.groups) {
			t.Errorf("Test Failed %v, expected: %v, got: %v", tcase, tcase.groups, grps)
		}
	}
}
