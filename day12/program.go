package main

import (
    "bytes"
    "fmt"
    "io"
    "encoding/json"
    "os"
)

type action func(string)

var sum int 
var ignoreRed bool

func main() {
    f, err := os.Open("input")
    check(err)
    
    defer f.Close()
    
    buffer := bytes.NewBuffer(nil)
    _, err = io.Copy(buffer, f)
    check(err)
    
    var jObject interface{}
    err = json.Unmarshal(buffer.Bytes(), &jObject)
    check(err)
    
    sum = 0
    visitJson(jObject)
    fmt.Printf("Sum: %d\n", sum)
    
    sum = 0
    ignoreRed = true
    visitJson(jObject)
    fmt.Printf("Sum (no red): %d\n", sum)
}

func visitJson(j interface{}) {
    switch j.(type) {
        case string: 
            visitJsonString(j.(string))
            break
        case float64:
            i := int(j.(float64))
            visitJsonNumber(i)
            break
        case []interface{}:
            visitJsonArray(j.([]interface{}))
            break
        case map[string]interface{}:
            visitJsonObject(j.(map[string]interface{}))
            break
        default:
            panic(j)
    }
}

func visitJsonObject(jObj map[string]interface{}) {
    if (ignoreRed) {
        for _, v := range jObj {
            strVal, isString := v.(string)
            if (isString && 
                strVal == "red") {
                return
            }
        }
    } 
    for _, v := range jObj {
        visitJson(v)
    }
}

func visitJsonArray(jArr []interface{}) {
    for _, item := range jArr {
        visitJson(item)
    }
}

func visitJsonString(jStr string) {
    // do nothing
}

func visitJsonNumber(jNum int) {
    sum += jNum
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}