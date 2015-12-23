package main

import (
    "fmt"
    "testing"
)

func Test_lookAndSay(t *testing.T) {
    lookAndSayTest(t, "1", "11")
    lookAndSayTest(t, "11", "21")
    lookAndSayTest(t, "21", "1211")
    lookAndSayTest(t, "1211", "111221")
    lookAndSayTest(t, "111221", "312211")
}


func lookAndSayTest(t *testing.T, input string, expected string) {
    actual := lookAndSay(input)
    assertEqual(t, expected, actual)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}