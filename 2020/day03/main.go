package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Area [][]int

func (a Area) MoveRight(start int, n int) int {
	w := len(a[0])
	return (start + n) % w
}

func calcSlope(area Area, r int, d int) int {
	x := 0
	y := 0

	h := len(area)

	treeHits := 0
	for {
		y = y + d
		if y >= h {
			break
		}

		x = area.MoveRight(x, r)

		if area[y][x] == 1 {
			treeHits++
		}
	}

	return treeHits
}

func readArea() Area {
	file, err := os.Open("input")
	if err != nil {
		log.Fatalln(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	area := Area{}
	for scanner.Scan() {
		row := []int{}
		for _, c := range scanner.Text() {
			if c == '#' {
				row = append(row, 1)
			} else {
				row = append(row, 0)
			}
		}
		area = append(area, row)
	}

	return area
}

func main() {
	area := readArea()

	s1 := calcSlope(area, 1, 1)
	s2 := calcSlope(area, 3, 1)
	s3 := calcSlope(area, 5, 1)
	s4 := calcSlope(area, 7, 1)
	s5 := calcSlope(area, 1, 2)

	fmt.Printf("Part 1: %d\n", s2)
	fmt.Printf("Part 2: %d\n", s1*s2*s3*s4*s5)
}
