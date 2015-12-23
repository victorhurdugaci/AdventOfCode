package main

import (
    "fmt"
    "testing"
)

func Test_parseAunt(t *testing.T) {
    parseAuntTest(t, 
        "Sue 304: samoyeds: 8, vizslas: 2, cars: 1",
        map[string]int {
            "samoyeds": 8 ,
            "vizslas": 2,
            "cars": 1,
        })
}

func parseAuntTest(t *testing.T, input string, expectedProperties map[string]int) {
    aunt := parseAunt(input)
    for k,v := range expectedProperties {
        assertEqual(t, v, aunt[k])    
    } 
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}