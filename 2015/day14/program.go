package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type raindeer struct{
    name string
    speed int
    speedTime int
    restTime int
    points int
    distance int
}

const RaceTime int = 2503
const RaindeerCount int = 9

func main() {
    f, err := os.Open("../input")
    check(err) 
    defer f.Close()
    scanner := bufio.NewScanner(f)

    var raindeers [9]raindeer 
    r := 0
    for scanner.Scan() {
        raindeers[r] = parseRaindeer(scanner.Text())
        r++
    }
    
    winnerDistance := 0 
    winnerPoints := 0
    
    // Race!!!
    for t := 0; t < RaceTime; t++ {
        timeWinDistance := 0
        for i := 0; i < RaindeerCount; i++ {
            r := &raindeers[i]
            newDistance := moveRaindeer(r, t)
            timeWinDistance = max(timeWinDistance, newDistance)
            winnerDistance = max(winnerDistance, newDistance) 
        }
        
        for i := 0; i < RaindeerCount; i++ {
            r := &raindeers[i]
            // Might have multiple in first place
            if (r.distance == timeWinDistance) {
                r.points++
                winnerPoints = max(winnerPoints, r.points)
            }
        }
    }
    
    fmt.Printf("Winner distance: %d\nWinner:\n", winnerDistance)
    for i := 0; i < RaindeerCount; i++ {
        r := raindeers[i]
        if (r.distance == winnerDistance) {
            fmt.Printf("- %s\n", r.name)
        }
    }
    fmt.Printf("Winner points: %d\nWinner:\n", winnerPoints)
    for i := 0; i < RaindeerCount; i++ {
        r := raindeers[i]
        if (r.points == winnerPoints) {
            fmt.Printf("- %s\n", r.name)
        }
    }
}

// Returns new distance
func moveRaindeer(r *raindeer, timeIndex int) int {
    currentIntervalTime := timeIndex % (r.speedTime + r.restTime)
    currentIntervalTime++
    if (currentIntervalTime <= r.speedTime) {
        r.distance += r.speed
    }
    
    return r.distance
}

func parseRaindeer(line string) raindeer {
    tokens := strings.Split(line, " ")
    
    speed, err := strconv.Atoi(tokens[3])
    check(err)
    
    speedTime, err := strconv.Atoi(tokens[6])
    check(err)
    
    restTime, err := strconv.Atoi(tokens[13])
    check(err)
    
    return raindeer {
        name: tokens[0],
        speed: speed,
        speedTime: speedTime,
        restTime: restTime,
    }
}

func max(a int, b int) int {
    if (a > b) {
        return a
    }    
    return b
}

func min(a int, b int) int {
    if (a < b) {
        return a
    }    
    return b
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}

