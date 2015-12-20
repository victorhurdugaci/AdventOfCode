package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
)

func main() {
    f, err := os.Open("input")
    check(err)
    
    defer f.Close()
    
    buffer := bytes.NewBuffer(nil)    
    _, err = io.Copy(buffer, f)
    check(err)
    
    directions := string(buffer.Bytes())
    
    visitedHouses := countVisitedHouses(directions)
    visitedHousesRobo := countVisitedHousesRobo(directions)
    
    fmt.Printf("Visited houses: %d\n", visitedHouses)
    fmt.Printf("Visited houses Robo: %d\n", visitedHousesRobo)
}

func countVisitedHouses(directions string) int {
    visited := map[position]bool {}
    
    currentPos := position {x: 0, y: 0}
    visited[currentPos] = true
    
    for i := 0; i < len(directions); i++ {
        currentPos = currentPos.move(directions[i])
        visited[currentPos] = true
    }
    
    return len(visited)
}

func countVisitedHousesRobo(directions string) int {
    visited := map[position]bool {}
    
    currentPosSanta := position {x: 0, y: 0}
    currentPosRoboSanta := position {x: 0, y: 0}
    visited[currentPosSanta] = true
    
    for i := 0; i < len(directions); i++ {
        if (i % 2 == 0) {
            currentPosSanta = currentPosSanta.move(directions[i])
            visited[currentPosSanta] = true
        } else {
            currentPosRoboSanta = currentPosRoboSanta.move(directions[i])
            visited[currentPosRoboSanta] = true
        }
    }
    
    return len(visited)
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

type position struct {
    x int
    y int
}

func (p position) move(direction byte) position {
    newX := p.x
    newY := p.y
    
    switch direction {
        case '^':
            newX++
        case '>':
            newY++
        case 'v':
            newX--
        case '<':
            newY--
        default: 
            panic("Unknown character")
    }
    
    return position {
        x: newX,
        y: newY,
    }
}

