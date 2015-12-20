package main

import (
    "testing"
)

func Test_findMinSuffixFor5Leading0(t *testing.T) {
    // From puzzle text
    findMinSuffixFor5Leading0Test(t, "abcdef", 609043)
    findMinSuffixFor5Leading0Test(t, "pqrstuv", 1048970)
}

func findMinSuffixFor5Leading0Test(t *testing.T, input string, expected int) {
   suffix := findMinSuffixForLeading0(input, 5, false)
   assertEqual(t, expected, suffix)
}

func assertEqual(t *testing.T, expected int, actual int) {
    if expected != actual {
        t.Error( "Expected: ", expected, "; got: ", actual);
    }
}