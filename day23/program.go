package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

var registers map[string]uint = map[string]uint {
    "a": 0,
    "b": 0,
}
var instructions [][]string
var currentInstruction int = 0

var verbose bool = false

func main() {
    for _, arg := range (os.Args[1:]) {
        if (arg == "verbose") {
            verbose = true
        } else if (arg == "part2") {
            registers["a"] = 1
        }
    }

    parseInstructions()
    runProgram()
    
    fmt.Printf("b: %v\n", registers["b"])
}

func runProgram() {
    for 0 <= currentInstruction && currentInstruction < len(instructions) {
        showStatus()
        instruction := instructions[currentInstruction]
        switch(instruction[0]) {
            case "hlf":
                reg := instruction[1]
                registers[reg] = registers[reg] / 2 
                currentInstruction++
                break
            case "tpl":
                reg := instruction[1]
                registers[reg] = registers[reg] * 3
                currentInstruction++
                break
            case "inc":
                reg := instruction[1]
                registers[reg]++
                currentInstruction++
                break
            case "jmp":
                val, err := strconv.Atoi(instruction[1])
                check(err)
                
                currentInstruction += val
                break
            case "jie":
                reg := instruction[1]
                val, err := strconv.Atoi(instruction[2])
                check(err)
                
                if (registers[reg] % 2 == 0) {
                    currentInstruction += val
                } else {
                    currentInstruction++
                }
                break
            case "jio":
                reg := instruction[1]
                val, err := strconv.Atoi(instruction[2])
                check(err)
                
                if (registers[reg] == 1) {
                    currentInstruction += val
                } else {
                    currentInstruction++
                }
                break
            default:
                panic("Unknown instruction")
        }
    }
}

func showStatus() {
    if (verbose) {
        fmt.Printf("%v| %v: %v\n", registers, currentInstruction, instructions[currentInstruction])
    }
}

func parseInstructions() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    parseInstructionsInternal(scanner, 0)
}

func parseInstructionsInternal(scanner *bufio.Scanner, count int) {
    if (!scanner.Scan()) {
        instructions = make([][]string, count)
        return
    }
    
    text := scanner.Text()
    parseInstructionsInternal(scanner, count+1)
    
    instructions[count] = strings.Split(text, " ")
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}


