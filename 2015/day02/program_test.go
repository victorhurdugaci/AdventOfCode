package main

import (
    "testing"
)

func Test_parsePackageDimensions(t *testing.T) {
    l, w, h := parsePackageDimensions("1x2x3")
    assertEqual(t, 1, l)
    assertEqual(t, 2, w)
    assertEqual(t, 3, h)
    
    l, w, h = parsePackageDimensions("1x1x10")
    assertEqual(t, 1, l)
    assertEqual(t, 1, w)
    assertEqual(t, 10, h)
    
    l, w, h = parsePackageDimensions("10x20x30")
    assertEqual(t, 10, l)
    assertEqual(t, 20, w)
    assertEqual(t, 30, h)
    
    l, w, h = parsePackageDimensions("7x12x1")
    assertEqual(t, 7, l)
    assertEqual(t, 12, w)
    assertEqual(t, 1, h)
}

func Test_calculatePaperNeeded(t *testing.T) {
    needed := calculatePaperNeeded(2, 3, 4)
    assertEqual(t, 58, needed)
    
    needed = calculatePaperNeeded(2, 4, 3)
    assertEqual(t, 58, needed)
    
    needed = calculatePaperNeeded(4, 2, 3)
    assertEqual(t, 58, needed)
    
    needed = calculatePaperNeeded(1, 1, 10)
    assertEqual(t, 43, needed)
}

func Test_calculateRibbonNeeded(t *testing.T) {
    needed := calculateRibbonNeeded(2, 3, 4)
    assertEqual(t, 34, needed)
    
    needed = calculateRibbonNeeded(2, 4, 3)
    assertEqual(t, 34, needed)
    
    needed = calculateRibbonNeeded(4, 2, 3)
    assertEqual(t, 34, needed)
    
    needed = calculateRibbonNeeded(1, 1, 10)
    assertEqual(t, 14, needed)
}

func assertEqual(t *testing.T, expected int, actual int) {
    if expected != actual {
        t.Error( "Expected: ", expected, "; got: ", actual);
    }
}