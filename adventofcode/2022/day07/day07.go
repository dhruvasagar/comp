package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func readInput() []string {
	lines := []string{}
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

func logCase(num int, out string) {
	fmt.Printf("Case #%d: %s\n", num, out)
}

type File struct {
	name string
	size int
}

type Folder struct {
	name   string
	parent *Folder

	files   []File
	folders []*Folder
}

func (f *Folder) total_size() int {
	tot := 0
	for _, file := range f.files {
		tot += file.size
	}
	for _, fold := range f.folders {
		tot += fold.total_size()
	}
	return tot
}

func (f *Folder) flat_folders() []*Folder {
	folders := []*Folder{}
	for _, fold := range f.folders {
		folders = append(folders, fold)
		folders = append(folders, fold.flat_folders()...)
	}
	return folders
}

type Folders []*Folder

func (f Folders) Len() int           { return len(f) }
func (f Folders) Swap(i, j int)      { f[i], f[j] = f[j], f[i] }
func (f Folders) Less(i, j int) bool { return f[i].total_size() < f[j].total_size() }

func parseInput(lines []string) Folder {
	root := Folder{name: "/"}
	idx := 1

	f := &root
	for idx < len(lines) {
		line := lines[idx]

		if strings.HasPrefix(line, "$ ls") {
			idx += 1
			for idx < len(lines) {
				line = lines[idx]
				if strings.HasPrefix(line, "$") {
					break
				}

				if strings.HasPrefix(line, "dir") {
					dname := strings.TrimSpace(strings.Split(line, "dir")[1])
					f.folders = append(f.folders, &Folder{
						name:   dname,
						parent: f,
					})
				} else {
					lp := strings.Split(line, " ")
					file := File{name: lp[1]}
					file.size, _ = strconv.Atoi(lp[0])
					f.files = append(f.files, file)
				}
				idx += 1
			}
		}

		if strings.HasPrefix(line, "$ cd") {
			if line == "$ cd .." {
				f = f.parent
			} else {
				dname := strings.TrimSpace(strings.Split(line, "$ cd")[1])
				for _, fold := range f.folders {
					if fold.name == dname {
						f = fold
						break
					}
				}
			}
		}

		idx += 1
	}
	return root
}

func part1(root Folder, limit int) int {
	res := 0
	for _, fold := range root.flat_folders() {
		ts := fold.total_size()
		if ts < limit {
			res += ts
		}
	}
	return res
}

func part2(root Folder, total_space int, required_space int) int {
	free_space := total_space - root.total_size()
	limit := required_space - free_space
	folders := root.flat_folders()
	sort.Sort(Folders(folders))
	for _, fold := range folders {
		ts := fold.total_size()
		if ts > limit {
			return ts
		}
	}
	return 0
}

func main() {
	root := parseInput(readInput())
	fmt.Println(part1(root, 100000))
	fmt.Println(part2(root, 70000000, 30000000))
}
