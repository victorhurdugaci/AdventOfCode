package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
)

func main() {
    f, err := os.Open("input")
    check(err)
    
    defer f.Close()
    
    buffer := bytes.NewBuffer(nil)    
    _, err = io.Copy(buffer, f)
    check(err)
    
    text := string(buffer.Bytes())
    
    for i := 0; i < 40; i++ {
        text = lookAndSay(text)
    }
    fmt.Printf("Result length 40: %v\n", len(text))
    
    for i := 0; i < 10; i++ {
        text = lookAndSay(text)
    }
    fmt.Printf("Result length 50: %v\n", len(text))
   
}

func lookAndSay(text string) string {
    var result bytes.Buffer
    
    for i := 0; i < len(text); i++ {
        count := 1
        
        for j := i + 1; j < len(text); j++ {
            if text[j] != text[i] {
                break
            }
            
            count++
        }
        
        result.WriteString(fmt.Sprintf("%v%c", count, text[i]))
        i += count -1
    }
    
    return result.String()
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}