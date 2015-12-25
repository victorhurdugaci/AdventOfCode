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
    
    moleculeCount := len(generateAllMolecules(input, rules))
    
    fmt.Printf("Distinct molecules: %d\n", moleculeCount)
}

func generateAllMolecules(input string, rules map[string][]string) map[string]bool {
     molecules := make(map[string]bool)
    
     for i := 0; i < len(input); i++ {
        testGroup1 := input[i:i+1]
        testGroup2 := ""
        
        if (i < len(input) -1) {
            testGroup2 = input[i:i+2]
        }
        
        for rk, rv := range rules {
            match1 := rk == testGroup1
            match2 := rk == testGroup2
            
            if (match1 || match2) {
                prefix := ""
                if (i > 0) {
                    prefix = input[:i]
                }
                
                suffix := ""
                if (match1) {
                    if (i < len(input) - 1) {
                        suffix = input[i+1:]
                    }
                }
                if (match2) {
                    if (i < len(input) - 2) {
                        suffix = input[i+2:]
                    }
                }
                
                for _, s := range rv {
                    molecule := prefix + s + suffix
                    molecules[molecule] = true
                }
            }
        }
    }
    
    return molecules
}

func parseInputFile(scanner *bufio.Scanner) (map[string][]string, string) {
    rules := make(map[string][]string)
    for scanner.Scan() {
        text := scanner.Text()
        if (text == "") {
            scanner.Scan()
            break
        }
        
        tokens := strings.Split(text, " => ");
        
        values, found := rules[tokens[0]]
        if (!found) {
            values = []string{ tokens[1] }
        } else {
            values = append(values, tokens[1])
        }
        
        rules[tokens[0]] = values
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


