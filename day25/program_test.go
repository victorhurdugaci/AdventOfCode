package main

import (
    "fmt"
    "testing"
)

func Test_calculateCodeIndex(t *testing.T) {
    assertEqual(t, uint64(1), calculateCodeIndex(1,1))
    assertEqual(t, uint64(2), calculateCodeIndex(2,1))
    assertEqual(t, uint64(3), calculateCodeIndex(1,2))
    assertEqual(t, uint64(13), calculateCodeIndex(3, 3))
    assertEqual(t, uint64(12), calculateCodeIndex(4, 2))
    assertEqual(t, uint64(17), calculateCodeIndex(5, 2))
    assertEqual(t, uint64(20), calculateCodeIndex(2, 5))
}

func Test_calculateNthCode(t *testing.T) {
    assertEqual(t, uint64(20151125), calculateNthCode(calculateCodeIndex(1,1)))
    assertEqual(t, uint64(31916031), calculateNthCode(calculateCodeIndex(2,1)))
    assertEqual(t, uint64(18749137), calculateNthCode(calculateCodeIndex(1,2)))
    assertEqual(t, uint64(1601130), calculateNthCode(calculateCodeIndex(3,3)))
    assertEqual(t, uint64(32451966), calculateNthCode(calculateCodeIndex(4,2)))
    assertEqual(t, uint64(17552253), calculateNthCode(calculateCodeIndex(5,2)))
    assertEqual(t, uint64(15514188), calculateNthCode(calculateCodeIndex(2,5)))
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if (expected != actual) {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}