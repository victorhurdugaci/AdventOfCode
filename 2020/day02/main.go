package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

const lineRegex = `(\d+)[-](\d+) (.): (.+)`

var lineMatcher = regexp.MustCompile(lineRegex)

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatalln(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	type Line struct {
		Min  int
		Max  int
		C    byte
		Pass string
	}

	lines := []Line{}
	for scanner.Scan() {
		match := lineMatcher.FindStringSubmatch(scanner.Text())

		min, _ := strconv.Atoi(match[1])
		max, _ := strconv.Atoi(match[2])
		c := match[3][0]
		pass := match[4]
		line := Line{
			Min:  min,
			Max:  max,
			C:    c,
			Pass: pass,
		}

		lines = append(lines, line)
	}

	part1Count := 0
	for _, line := range lines {
		count := 0
		for _, c := range line.Pass {
			if byte(c) == line.C {
				count++
			}
		}
		if line.Min <= count && count <= line.Max {
			part1Count++
		}
	}

	fmt.Printf("Total valid part 1: %d\n", part1Count)

	part2Count := 0
	for _, line := range lines {
		pass := line.Pass
		valid := len(pass) >= line.Max &&
			(pass[line.Min-1] == line.C || pass[line.Max-1] == line.C) &&
			pass[line.Min-1] != pass[line.Max-1]
		if valid {
			part2Count++
		}
	}

	fmt.Printf("Total valid part 2: %d\n", part2Count)
}
