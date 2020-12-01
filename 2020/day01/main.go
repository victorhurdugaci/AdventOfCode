package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
)

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatalln(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	values := []int{}
	for scanner.Scan() {
		val, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatalln(err)
		}
		values = append(values, val)
	}

	sort.Ints(values)

	part1(values)
	part2(values)
}

func part1(values []int) {
	for i := 0; i < len(values); i++ {
		for j := i + 1; j < len(values); j++ {
			sum := values[i] + values[j]
			if sum > 2020 {
				break
			}

			if sum != 2020 {
				continue
			}

			v1 := values[i]
			v2 := values[j]
			fmt.Printf("Part 1: %d * %d = %d\n", v1, v2, v1*v2)
			return
		}
	}
}

func part2(values []int) {
	for i := 0; i < len(values); i++ {
		for j := i + 1; j < len(values); j++ {
			for k := j + 1; j < len(values); k++ {
				sum := values[i] + values[j] + values[k]
				if sum > 2020 {
					break
				}
				if sum != 2020 {
					continue
				}

				v1 := values[i]
				v2 := values[j]
				v3 := values[k]
				fmt.Printf("Part 2: %d * %d * %d = %d\n", v1, v2, v3, v1*v2*v3)
				return
			}
		}
	}
}
