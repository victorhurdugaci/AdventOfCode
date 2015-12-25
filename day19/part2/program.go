package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func main() {
    f, err := os.Open("../input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)
    
    rules, input := parseInputFile(scanner)
    
    result, steps :=  reduce(input, rules)
    
    fmt.Printf("Result: %v\n", result)
    fmt.Printf("Min steps: %d\n", steps)
}

func reduce(input string, rules map[string]string) (string, int) {
    for rk, rv := range rules {
        rkLen := len(rk)
        
        for i:=0;i<len(input)-len(rk)+1;i++ {
            candidate := input[i:i+rkLen]
            if (candidate == rk) {
                newInput := input[:i] + rv + input[i+rkLen:]
                output, steps := reduce(newInput, rules)
                return output, steps+1
            }
        }
    }
    
    return input, 0
}

func parseInputFile(scanner *bufio.Scanner) (map[string]string, string) {
    rules := make(map[string]string)
    for scanner.Scan() {
        text := scanner.Text()
        if (text == "") {
            scanner.Scan()
            break
        }
        
        tokens := strings.Split(text, " => ");
        rules[tokens[1]] = tokens[0]
    }
    
    input := scanner.Text()
    
    return rules, input
}

func min(a int, b int) int {
    if (a < b) {
        return a
    }
    return b
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}


