package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, _ := os.Open("input")
	defer file.Close()

	text, _ := ioutil.ReadAll(file)
	textArray := strings.Split(string(text), ",")

	memory := make([]int, len(textArray))
	for idx, s := range textArray {
		n, _ := strconv.Atoi(s)
		memory[idx] = n
	}

	part1(memory)
	part2(memory, 19690720)
}

func run(memory []int) {
	idx := 0
	keepRunning := true
	for keepRunning {
		switch memory[idx] {
		case 1:
			memory[memory[idx+3]] = memory[memory[idx+2]] + memory[memory[idx+1]]
			idx += 4
		case 2:
			memory[memory[idx+3]] = memory[memory[idx+2]] * memory[memory[idx+1]]
			idx += 4
		case 99:
			keepRunning = false
		default:
			panic("Err")
		}
	}
}

func part1(memory []int) {
	workingMemory := make([]int, len(memory))
	copy(workingMemory, memory)

	workingMemory[1] = 12
	workingMemory[2] = 2

	run(workingMemory)

	fmt.Printf("Part 1: %d\n", workingMemory[0])
}

func part2(memory []int, expectedSolution int) {

	for noun := 0; noun < 100; noun++ {
		for verb := 0; verb < 100; verb++ {
			workingMemory := make([]int, len(memory))
			copy(workingMemory, memory)

			workingMemory[1] = noun
			workingMemory[2] = verb

			run(workingMemory)

			if workingMemory[0] == expectedSolution {
				fmt.Printf("Part 2: %d (n=%d,v=%d)\n", 100*noun+verb, noun, verb)
			}
		}
	}
}
