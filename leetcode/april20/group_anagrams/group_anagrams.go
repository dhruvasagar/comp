package main

import (
	"fmt"
	"sort"
	"strings"
)

func chars(w string) []string {
	return strings.Split(w, "")
}

func sortedKey(s string) string {
	c := chars(s)
	sort.Strings(c)
	return strings.Join(c, "")
}

func groupAnagrams(strs []string) [][]string {
	res := [][]string{}
	resmap := make(map[string][]string)
	for i := 0; i < len(strs); i++ {
		str := strs[i]
		key := sortedKey(str)
		_, ok := resmap[key]
		if !ok {
			resmap[key] = []string{}
		}
		resmap[key] = append(resmap[key], str)
	}
	for _, v := range resmap {
		res = append(res, v)
	}
	return res
}

func main() {
	fmt.Println(groupAnagrams([]string{"eat", "tea", "tan", "ate", "nat", "bat"}))
	fmt.Println(groupAnagrams([]string{"", ""}))
	fmt.Println(groupAnagrams([]string{"ray", "cod", "abe", "ned", "arc", "jar", "owl", "pop", "paw", "sky", "yup", "fed", "jul", "woo", "ado", "why", "ben", "mys", "den", "dem", "fat", "you", "eon", "sui", "oct", "asp", "ago", "lea", "sow", "hus", "fee", "yup", "eve", "red", "flo", "ids", "tic", "pup", "hag", "ito", "zoo"}))
}
