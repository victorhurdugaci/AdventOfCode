package main

import (
    "fmt"
    "math"
    "os"
    "strings"
)

type character struct {
    hp int
}

type player struct {
    character // inherit
    mana int
    shield int
    poison int
    recharge int
    
    shieldStrength int
    manaSpent int
}

type action func(p *player, b *character) bool

var availableAction []action = []action{
    action_missle,
    action_drain,
    action_shield,
    action_poison,
    action_recharge,
}

const bossHitPoints int = 71
const bossDamage int = 10

const playerHitPoints int = 50
const playerMana int = 500

var minManaSpent int = math.MaxInt32

var verbose bool = false
var hard bool = false

func main() {  
    for _, arg := range (os.Args[1:]) {
        if (arg == "hard") {
            // part 2
            hard = true
        } else if (arg == "verbose") {
            verbose = true
        }
    }
    
    p := player {
        character: character {
            hp: playerHitPoints,
        }, 
        mana: playerMana,
    }   
    b := character {
        hp: bossHitPoints,
    }
    
    findMinManaFight(p, b, 1)
    
    fmt.Printf("Min mana fight: %d\n", minManaSpent)
}

func findMinManaFight(p player, b character, turn int) {
    isPlayerTurn := (turn % 2 == 1)
    
    action_doMagic(&p, &b)
    
    if (isFightOver(&p, &b, turn)) {
        return
    }
    
    // Player turn
    if (isPlayerTurn) {
        if (hard) {
            p.hp--
            if (isFightOver(&p, &b, turn)) {
                return
            }
        }
        
        wasAbleToDoAction := false
        for _, act := range(availableAction) {
            turnP := p
            turnB := b
            executed := act(&turnP, &turnB)
            if (executed) {
                wasAbleToDoAction = true
                if (isFightOver(&turnP, &turnB, turn)) {
                    continue
                }
                findMinManaFight(turnP, turnB, turn+1)
            }
        }
        if (!wasAbleToDoAction) {
            // No action possible
            return 
        }
        
    } else {
        p.hp -= max(1, bossDamage - p.shieldStrength)
        if (isFightOver(&p, &b, turn)) {
            return
        }
        findMinManaFight(p, b, turn+1)
    }
}

func printFightStatus(p *player, b *character, turn int) {
    if (verbose) {
        player := "B"
        if (turn % 2 == 1) {
            player = "P"
        }
        fmt.Print(strings.Repeat(" ", turn))
        fmt.Printf("%v(%v) %v %v\n", player, turn, p, b)
    }
}

func isFightOver(p *player, b *character, turn int) bool {
    printFightStatus(p, b, turn)
    if (p.hp > 0 && b.hp > 0) {
        return false
    }
    
    if (p.hp > 0) {
        // The player won
        minManaSpent = min(minManaSpent, p.manaSpent)
    }
    
    return true
}

func action_missle(p *player, b *character) bool {
    const cost int = 53
    const damage int = 4
    
    if (p.mana < cost) { return false }
    
    b.hp -= damage
    
    p.mana -= cost
    p.manaSpent += cost
    
    return true
}

func action_drain(p *player, b *character) bool {
    const cost int = 73
    const damage int = 2
    
    if (p.mana < cost) { return false }
    
    b.hp -= damage
    p.hp += damage
    
    p.mana -= cost
    p.manaSpent += cost
    
    return true
}

func action_shield(p *player, b *character) bool {
    const cost int = 113
    
    if (p.mana < cost || p.shield != 0) { return false }
    
    p.shield = 6
    
    p.mana -= cost
    p.manaSpent += cost
    
    return true
}

func action_poison(p *player, b *character) bool {
    const cost int = 173

    if (p.mana < cost || p.poison != 0) { return false }
    
    p.poison = 6
    
    p.mana -= cost
    p.manaSpent += cost
    
    return true
}

func action_recharge(p *player, b *character) bool {
    const cost int = 229
    
    if (p.mana < cost || p.recharge != 0) { return false }
    
    p.recharge = 5
    
    p.mana -= cost
    p.manaSpent += cost
    
    return true
}

func action_doMagic(p *player, b *character) bool {
    p.shieldStrength = 0
    if (p.shield != 0) {
        p.shieldStrength = 7
        p.shield--
    }
    
    if (p.poison != 0) {
        b.hp -= 3
        p.poison--
    }
    
    if (p.recharge != 0){
        p.mana += 101
        p.recharge--
    }
    
    return true
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