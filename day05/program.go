package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    
    niceStrings := 0
    niceStringsV2 := 0
    
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        if (isNiceString(scanner.Text())) {
            niceStrings++
        }
        if (isNiceStringV2(scanner.Text())) {
            niceStringsV2++
        }
    }
    
    fmt.Printf("Nice strings: %d\n", niceStrings)
    fmt.Printf("Nice strings V2: %d\n", niceStringsV2)
}

func isNiceString(s string) bool {
    vowelCount := 0
    hasDoubleLetter := false
    hasNaugtyGroup := false
    
    inputLen := len(s)
    for i := 0; i < inputLen; i++ {
        c := s[i]
        
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u') {
            vowelCount++
        }
        
        if (i < inputLen-1) {
            cNext := s[i+1]
            
            if (c == cNext) {
                hasDoubleLetter = true
            }
            
            if ((c == 'a' && cNext == 'b') || 
                (c == 'c' && cNext == 'd') ||
                (c == 'p' && cNext == 'q') ||
                (c == 'x' && cNext == 'y')) {
                hasNaugtyGroup = true 
            }
        }
    }
    
    return !hasNaugtyGroup && 
        hasDoubleLetter && 
        (vowelCount >= 3)
}

func isNiceStringV2(s string) bool {
    hasDoubleLetter := false
    hasDoubleGroup := false
    
    inputLen := len(s)
    for i := 0; i < inputLen-1; i++ {
        c := s[i]
        cNext := s[i+1]
             
        for j := i + 2; j < inputLen-1; j++ {
            if (s[j] == c && 
                s[j+1] == cNext) {
                hasDoubleGroup = true
                break
            }
        }
        
        if (i < inputLen-2 &&
            (c == s[i+2])) {
                hasDoubleLetter = true
        }
    }
    
    return hasDoubleLetter && hasDoubleGroup
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

