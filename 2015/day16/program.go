package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)


var senderProperties = map[string]int{
    "children": 3,
    "cats": 7,
    "samoyeds": 2, 
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)

    bestMatch1 := 0
    bestMatchAunt1 := -1
    bestMatch2 := 0
    bestMatchAunt2 := -1
    i := 1

    for scanner.Scan() {
        aunt := parseAunt(scanner.Text())
        match1 := evaluateAunt(aunt, false)
        if (match1 > bestMatch1) {
            bestMatch1 = match1
            bestMatchAunt1 = i
        } 
        
        match2 := evaluateAunt(aunt, true)
        if (match2 > bestMatch2) {
            bestMatch2 = match2
            bestMatchAunt2 = i
        } 
        
        i++
    }

    fmt.Printf("Aunt 1: %d(%d)\n", bestMatchAunt1, bestMatch1)
    fmt.Printf("Aunt 2: %d(%d)\n", bestMatchAunt2, bestMatch2)
}

func evaluateAunt(aunt map[string]int, part2 bool) int {
    score := 0
    
    for k, v := range senderProperties {
        auntValue, found := aunt[k]
        if (found) {
            if (part2 && (k == "cats" || k == "trees")) {
                if (auntValue > v) {
                    score++
                }
            } else if (part2 && (k == "pomerianians" || k == "goldfish")) {
                if (auntValue < v) {
                    score++
                }
            } else if (auntValue == v) {
                score++
            }
        }
    }
    
    return score
}


func parseAunt(line string) map[string]int {
    properties := make(map[string]int)
    
    tokens := strings.Split(line, " ")
    
    for i := 0; i < 3; i++ {
        name := strings.Trim(tokens[2 + 2*i], ":")
        value, err := strconv.Atoi(strings.TrimRight(tokens[3 + 2*i], ","))
        check(err)
        
        properties[name] = value
    }
    
    return properties
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

