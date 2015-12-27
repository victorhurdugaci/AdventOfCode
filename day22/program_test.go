package main

import (
    "fmt"
    "testing"
)

func Test_action_NotEnoughMana(t *testing.T) {
    p := player {
        character: character {
            hp: 99,
        },
    }
    b := character { }

    p.mana = 52
    ok := action_missle(&p, &b)
    assertEqual(t, false, ok)
    
    p.mana = 72
    ok = action_drain(&p, &b)
    assertEqual(t, false, ok)
    
    p.mana = 112
    ok = action_shield(&p, &b)
    assertEqual(t, false, ok)
    
    p.mana = 172
    ok = action_poison(&p, &b)
    assertEqual(t, false, ok)
    
    p.mana = 228
    ok = action_recharge(&p, &b)
    assertEqual(t, false, ok)
}

func Test_action_missle(t *testing.T) {
    p := player {
        character: character {
            hp: 200,
        },
        mana: 53,
    }
    b := character {
        hp: 100,
    }
    
    expectedP := p
    expectedP.mana = 0
    expectedP.manaSpent = 53
    
    expectedB := b
    expectedB.hp = 96
    
    ok := action_missle(&p, &b)
    assertEqual(t, true, ok)
    
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}


func Test_action_drain(t *testing.T) {
    p := player {
        character: character {
            hp: 200,
        },
        mana: 73,
    }
    b := character {
        hp: 100,
    }
    
    expectedP := p
    expectedP.hp = 202
    expectedP.mana = 0
    expectedP.manaSpent = 73
    
    expectedB := b
    expectedB.hp = 98
    
    ok := action_drain(&p, &b)
    assertEqual(t, true, ok)
    
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_shield(t *testing.T) {
    p := player {
        mana: 113,
    }
    b := character { }
    
    expectedP := p
    expectedP.mana = 0
    expectedP.manaSpent = 113
    expectedP.shield = 6
    
    expectedB := b
    
    ok := action_shield(&p, &b)
    assertEqual(t, true, ok)
    
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_poison(t *testing.T) {
    p := player {
        mana: 173,
    }
    b := character { }
    
    expectedP := p
    expectedP.mana = 0
    expectedP.manaSpent = 173
    expectedP.poison = 6
    
    expectedB := b
    
    ok := action_poison(&p, &b)
    assertEqual(t, true, ok)
    
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_recharge(t *testing.T) {
    p := player {
        mana: 229,
    }
    b := character { }
    
    expectedP := p
    expectedP.mana = 0
    expectedP.manaSpent = 229
    expectedP.recharge = 5
    
    expectedB := b
    
    ok := action_recharge(&p, &b)
    assertEqual(t, true, ok)
    
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_nothing(t *testing.T) {
    p := player {
        mana: 229,
    }
    b := character { }
    
    expectedP := p
    expectedB := b
    
    action_doMagic(&p, &b)
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_shield_2(t *testing.T) {
    p := player {
        mana: 229,
        shield: 2,
    }
    b := character { }
    
    expectedP := p
    expectedP.shield = 1
    expectedP.shieldStrength = 7
    
    expectedB := b
    
    action_doMagic(&p, &b)
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_shield_1(t *testing.T) {
    p := player {
        mana: 229,
        shield: 1,
    }
    b := character { }
    
    expectedP := p
    expectedP.shield = 0
    expectedP.shieldStrength = 7
    
    expectedB := b
    
    action_doMagic(&p, &b)
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_shield_0(t *testing.T) {
    p := player {
        mana: 229,
        shieldStrength: 7,
        shield: 0,
    }
    b := character { }
    
    expectedP := p
    expectedP.shieldStrength = 0
    
    expectedB := b
    
    action_doMagic(&p, &b)
    assertPlayerEqual(t, &expectedP, &p)
    assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_poison(t *testing.T) {
     p := player {
         mana: 229,
         poison: 3,
     }
     b := character {
         hp: 100,
    }
     
     expectedP := p
     expectedP.poison = 2
     
     expectedB := b
     expectedB.hp = 97
     
     action_doMagic(&p, &b)
     assertPlayerEqual(t, &expectedP, &p)
     assertCharacterEqual(t, &expectedB, &b)
}

func Test_action_doMagic_recharge(t *testing.T) {
     p := player {
         mana: 229,
         recharge: 3,
     }
     b := character { }
     
     expectedP := p
     expectedP.recharge = 2
     expectedP.mana = 330
     
     expectedB := b
     
     action_doMagic(&p, &b)
     assertPlayerEqual(t, &expectedP, &p)
     assertCharacterEqual(t, &expectedB, &b)
}

func assertCharacterEqual(t *testing.T, expectedC *character, actualC *character) {
    assertEqual(t, expectedC.hp, actualC.hp)
}

func assertPlayerEqual(t *testing.T, expectedP *player, actualP *player) {    
    assertCharacterEqual(t, &expectedP.character, &actualP.character)
    
    assertEqual(t, expectedP.mana, actualP.mana)
    assertEqual(t, expectedP.shield, actualP.shield)
    assertEqual(t, expectedP.poison, actualP.poison)
    assertEqual(t, expectedP.recharge, actualP.recharge)
    
    assertEqual(t, expectedP.manaSpent, actualP.manaSpent)
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if (expected != actual) {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}