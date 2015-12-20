package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
    "strconv"
    "strings"
    "crypto/md5"
    "encoding/hex"
)

func main() {
    showProgress := false
    
    f, err := os.Open("input")
    check(err)
    
    defer f.Close()
    
    buffer := bytes.NewBuffer(nil)    
    _, err = io.Copy(buffer, f)
    check(err)
    
    input := string(buffer.Bytes())
    
    suffix5 := findMinSuffixForLeading0(input, 5, showProgress)
    suffix6 := findMinSuffixForLeading0(input, 6, showProgress)
    
    fmt.Printf("Suffix 5: %d\n", suffix5)
    fmt.Printf("Suffix 6: %d\n", suffix6)
}

func findMinSuffixForLeading0(input string, noOfZero int, showProgress bool) int {
    searchedPrefix := strings.Repeat("0", noOfZero)
    
    suffix := 0
    for {
        suffix++
        hashInput := input + strconv.Itoa(suffix)
        
        if (showProgress) { fmt.Printf("Checking: %d... ", suffix) }
        
        inputBytes := []byte(hashInput)
        hashBytes := md5.Sum(inputBytes)
        hash := hex.EncodeToString(hashBytes[:])
        
        if (showProgress) { fmt.Printf("Hash: %s\n", hash) }
    
        if (strings.HasPrefix(hash, searchedPrefix)) {
            return suffix;
        }
    }
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}
