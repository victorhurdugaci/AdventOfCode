package main

import (
    "fmt"
    "io"
    "os"
)

func main() {
    f, err := os.Open("input")
    check(err) 
    
    buffer := make([]byte, 128)
    
    currentFloor := 0
    
    basementChar := 0
    totalChars := 0
    
    for {
    n, err := f.Read(buffer)
        if err != nil && 
        err == io.EOF {
            break
        }
        
        check(err)
        
        for i := 0; i < n; i++ {
            totalChars++
            
            if buffer[i] == '(' {
                currentFloor++
            } else {
                currentFloor--
            }
            
            if basementChar == 0 &&
            currentFloor < 0 {
                basementChar = totalChars	   
            }
        }
    }
    f.Close()
    
    fmt.Printf("Basement entry char: %d\n", basementChar)
    fmt.Printf("Final floor: %d\n", currentFloor)
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}