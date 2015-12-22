package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func main() {
    f, err := os.Open("input")
    check(err) 
    defer f.Close()
    
    totalCode := 0
    totalPrinted := 0
    totalEncoded := 0
    
    scanner := bufio.NewScanner(f)
    for scanner.Scan() {
        text := scanner.Text()
        text = strings.TrimSpace(text)
        code, printed := countCharactersDecode(text)
        _, encode := countCharactersEncode(text)
        totalCode += code
        totalPrinted += printed
        totalEncoded += encode
    }
    
    fmt.Printf("Total: %d\n\n", totalCode)
    fmt.Printf("Printed: %d\n", totalPrinted)
    fmt.Printf("Difference: %d\n", totalCode - totalPrinted)
    
    fmt.Printf("Encoded: %d\n\n", totalEncoded)
    fmt.Printf("Difference: %d\n", totalEncoded - totalCode)
}


// 1 = visible characters
// 2 = input characters
func countCharactersDecode(text string) (int, int) {
    // the text always starts and ends with quotes
    // so no point in visiting those
    codeCharCount := 2
    printedCharCount := 0
    
    endPos := len(text) - 1
    for i := 1; i < endPos; i++ {
        c := text[i]
        
        if (c == '\\') {   
            if ((i < endPos - 1) &&
                ((text[i+1] == '"') ||
                 (text[i+1] == '\\'))) {
                printedCharCount++
                codeCharCount += 2
                i += 1
                continue
            }
            
            if ((i < endPos - 3) &&
                (text[i+1] == 'x') &&
                isHex(text[i+2]) &&
                isHex(text[i+3])) {
                printedCharCount++
                codeCharCount += 4
                i += 3
                continue
            }
        }
             
        codeCharCount++
        printedCharCount++
    }
    
    return codeCharCount, printedCharCount
}

// 1 = visible characters
// 2 = input characters
func countCharactersEncode(text string) (int, int) {
    codeCharCount := 0
    encodeCharCount := 2
    
    for i := 0; i < len(text); i++ {
        c := text[i]
        codeCharCount++
        encodeCharCount++
        
        if ((c == '\\') ||
            (c == '"')) {
            encodeCharCount++
        }
    }
    
    return codeCharCount, encodeCharCount
}

func isHex(c byte) bool {
    return ('0' <= c && c <= '9') ||
           ('a' <= c && c <= 'f')
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

