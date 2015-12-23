package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
)

type action func(string)

func main() {
    f, err := os.Open("input")
    check(err)
    
    defer f.Close()
    
    buffer := bytes.NewBuffer(nil)    
    _, err = io.Copy(buffer, f)
    check(err)
    
    password := string(buffer.Bytes())
    nextPass, found := findNextPassword(password)
    
    if (found) {
        fmt.Printf("Next pass: %v\n", nextPass)
    } else {
        fmt.Printf("Out of passwords")
    }
    
    nextPass, found = findNextPassword(nextPass)
    if (found) {
        fmt.Printf("Next pass: %v\n", nextPass)
    } else {
        fmt.Printf("Out of passwords")
    }
}

func findNextPassword(pass string) (string, bool) {
    for {
        newPass, found := incrementString(pass)
        if (!found) {
            break
        }
        
        if (isValidPassword(newPass)) {
            return newPass, true
        }
        
        pass = newPass
    }
    
    return "", false
}

func isValidPassword(pass string) bool {
    if (len(pass) != 8) {
        return false
    }
    
    foundIncreasingGroup := false
    containsUnallowedChars := false
    
    doubleGroups := make(map[byte]bool)
    
    for i := 0; i < len(pass); i++ {
        if ((pass[i] == 'i') ||
            (pass[i] == 'o') ||
            (pass[i] == 'l')) {
                containsUnallowedChars = true
                break
        }
        
        if ((i < len(pass) - 2) &&
            ((pass[i] + 1 == pass[i+1]) &&
             (pass[i] + 2 == pass[i+2]))) {
                foundIncreasingGroup = true
        }
        
        if ((i < len(pass) -1) &&
            (pass[i] == pass[i+1])) {
                doubleGroups[pass[i]] = true        
        }
    }
    
    return !containsUnallowedChars && 
           foundIncreasingGroup &&
           len(doubleGroups) >= 2
}

func incrementString(startPass string) (string, bool) {
    newPass := bytes.NewBufferString(startPass)
    passBytes := newPass.Bytes()
    
    found := false
    
    for i := len(passBytes) - 1; i >= 0; i-- {
        newC, foundNewC := incrementChar(passBytes[i])
        if (foundNewC) {
            passBytes[i] = newC
            found = true
            break
        }
        
        passBytes[i] = 'a'
    }
    
    return newPass.String(), found
}

func incrementChar(c byte) (byte, bool) {
    if (c >= 'z') {
        return c, false
    }
    
    return c + 1, true
}


func check(e error) {
    if e != nil {
        panic(e)
    }
}