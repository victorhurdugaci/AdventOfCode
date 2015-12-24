package main

import (
    "fmt"
    "testing"
)

func Test_computeNextStateTest_StuckCorners(t *testing.T) {
     start := [][]int {
        {1,1,0,1,0,1},
        {0,0,0,1,1,0},
        {1,0,0,0,0,1},
        {0,0,1,0,0,0},
        {1,0,1,0,0,1},
        {1,1,1,1,0,1},
    }
    expected := [][]int {
        {1,0,1,1,0,1},
        {1,1,1,1,0,1},
        {0,0,0,1,1,0},
        {0,0,0,0,0,0},
        {1,0,0,0,1,0},
        {1,0,1,1,1,1},
    }
    computeNextStateTest(t, start, true, expected)
    
    start = [][]int {
        {1,0,1,1,0,1},
        {1,1,1,1,0,1},
        {0,0,0,1,1,0},
        {0,0,0,0,0,0},
        {1,0,0,0,1,0},
        {1,0,1,1,1,1},
    }
    expected = [][]int {
        {1,0,0,1,0,1},
        {1,0,0,0,0,1},
        {0,1,0,1,1,0},
        {0,0,0,1,1,0},
        {0,1,0,0,1,1},
        {1,1,0,1,1,1},
    }
    computeNextStateTest(t, start, true, expected)
}

func Test_computeNextStateTest(t *testing.T) {
     start := [][]int {
        {0,1,0,1,0,1},
        {0,0,0,1,1,0},
        {1,0,0,0,0,1},
        {0,0,1,0,0,0},
        {1,0,1,0,0,1},
        {1,1,1,1,0,0},
    }
    expected := [][]int {
        {0,0,1,1,0,0},
        {0,0,1,1,0,1},
        {0,0,0,1,1,0},
        {0,0,0,0,0,0},
        {1,0,0,0,0,0},
        {1,0,1,1,0,0},
    }
    computeNextStateTest(t, start, false, expected)
    
    start = [][]int {
        {0,0,1,1,0,0},
        {0,0,1,1,0,1},
        {0,0,0,1,1,0},
        {0,0,0,0,0,0},
        {1,0,0,0,0,0},
        {1,0,1,1,0,0},
    }
    expected = [][]int {
        {0,0,1,1,1,0},
        {0,0,0,0,0,0},
        {0,0,1,1,1,0},
        {0,0,0,0,0,0},
        {0,1,0,0,0,0},
        {0,1,0,0,0,0},
    }
    computeNextStateTest(t, start, false, expected)
}

func Test_getOnNeightborCount(t *testing.T) {
    matrix := [][]int {
        {0,1,0,1,0,1},
        {0,0,0,1,1,1},
        {1,0,0,0,0,1},
        {0,0,1,0,0,0},
        {1,0,1,0,0,1},
        {1,1,1,1,0,0},
    }
    
    // Corners
    getOnNeightborCountTest(t, matrix, 0, 0, 1)
    getOnNeightborCountTest(t, matrix, 5, 0, 2)
    getOnNeightborCountTest(t, matrix, 0, 5, 2)
    getOnNeightborCountTest(t, matrix, 5, 5, 1)
    
    // Edge
    getOnNeightborCountTest(t, matrix, 0, 2, 3)
    getOnNeightborCountTest(t, matrix, 2, 0, 0)
    getOnNeightborCountTest(t, matrix, 5, 2, 3)
    getOnNeightborCountTest(t, matrix, 2, 5, 2)

    // Inner
    getOnNeightborCountTest(t, matrix, 1, 1, 2)
    getOnNeightborCountTest(t, matrix, 1, 3, 2)
    getOnNeightborCountTest(t, matrix, 2, 4, 4)
}

func Test_parseMatrixLineTest(t *testing.T) {
    parseMatrixLineTest(t, "......", []int{0,0,0,0,0,0})
    parseMatrixLineTest(t, "######", []int{1,1,1,1,1,1})
    parseMatrixLineTest(t, ".##..#", []int{0,1,1,0,0,1})
}

func parseMatrixLineTest(t *testing.T, input string, expectedLine []int) {
    actualLine := parseMatrixLine(input)
    assertEqual(t, len(expectedLine), len(expectedLine))
    for i := 0; i < len(expectedLine); i++ {
        assertEqual(t, expectedLine[i], actualLine[i])
    }
}

func getOnNeightborCountTest(t *testing.T, matrix [][]int, y int, x int, expectedCount int) {
    actualCount := getOnNeightborCount(matrix, y, x)
    assertEqual(t, expectedCount, actualCount)
}

func computeNextStateTest(t *testing.T, start [][]int, stuckCorners bool, expected [][]int) {
    actual := computeNextState(start, stuckCorners)
    forEach2DMatrixElement(expected, func(y int, x int) {
        if (expected[y][x] != actual[y][x]) {
            t.Error(fmt.Sprintf(
                "[%d,%d] Expected: %d; got: %d", 
                y, 
                x,
                expected[y][x],
                actual[y][x]))
        }
    })
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}