package main

import (
    "bufio"
    "bytes"
    "fmt"
    "os"
    "strconv"
    "strings"
)

type variableNode struct {
    name string
}

type constNode struct {
    value uint16
}

type binaryOperation struct {
    lOperand interface{}
    rOperand interface{}
    operator string
}

type unaryOperation struct {
    operand interface{}
    operator string
}

var variables map[string]bool

func main() {
    f, err := os.Open("input_part2")
    check(err) 
    defer f.Close()
    
    scanner := bufio.NewScanner(f)
    
    variables = make(map[string]bool)
    
    statements := make([]binaryOperation, 0)
    for scanner.Scan() {
        statement := parseStatement(scanner.Text())
        statements = append(statements, statement)
    }
    
    statements = orderStatements(statements)
    code := generateProgram(statements)
    
    generatedFile, err := os.Create("generated_part2.go")
    check(err)
    defer f.Close()

    generatedFile.WriteString(code)
}

// Super hacky mark and sweep with multiple passes
// but hey, it works
func orderStatements(statements []binaryOperation) []binaryOperation {
    orderedStatements := make([]binaryOperation, 0)
    availableVariables := make(map[string]bool)
    usedStatements := make(map[binaryOperation]bool)
    
    for i := range statements {
        usedStatements[statements[i]] = false
    }
    
    for {
        moreSweepsNeeded := false
        for i := range statements {
            statement := statements[i]
            if (!usedStatements[statement]) {
                
                // check if operands are available
                if (isOperandFullyAvailable(statement.rOperand, availableVariables)) {
                    orderedStatements = append(orderedStatements, statement)
                    usedStatements[statement] = true
                    
                    varNode, _ := statement.lOperand.(variableNode)
                    availableVariables[varNode.name] = true
                    moreSweepsNeeded = true
                }
            }
        }
        
        if (!moreSweepsNeeded) {
            break
        }
    }
    
    return orderedStatements
}

func isOperandFullyAvailable(operand interface{}, availableVariables map[string] bool) bool{
    _, ok := operand.(constNode)
    if (ok) {
        // Consts are always available
        return true
    }
    
    varNode, ok := operand.(variableNode)
    if (ok) {
        return availableVariables[varNode.name]
    }

    unaryOp, ok := operand.(unaryOperation)
    if (ok) {
        return isOperandFullyAvailable(unaryOp.operand, availableVariables)
    }
    
    binaryOp, ok := operand.(binaryOperation)
    if (ok) {
        return isOperandFullyAvailable(binaryOp.lOperand, availableVariables) && 
               isOperandFullyAvailable(binaryOp.rOperand, availableVariables)
    }
    
    panic("WTF")
}

func generateProgram(statements []binaryOperation) string {
    return fmt.Sprintf(
`package main
import "fmt"

func main() {
%v

}`,
        generateCodeFromStatements(statements));
}


func generateCodeFromStatements(statements []binaryOperation) string {
    var code bytes.Buffer
    
    for key, _ := range variables {
        code.WriteString(fmt.Sprintf("    var %s uint16\n", key))
    }

    for i := range statements {
        code.WriteString("    ")
        generateStatement(&code, statements[i])
        code.WriteString("\n")
    }
    
    for key, _ := range variables {
        code.WriteString(
            fmt.Sprintf("    fmt.Printf(\"%s: %%d\\n\", %s)\n", key, key))
    }
    
    if (variables["a"]) {
        code.WriteString(fmt.Sprintf("    fmt.Printf(\"a: %%d\\n\", a)\n"))
    }
    
    return code.String()
}

func generateStatement(code *bytes.Buffer, statement binaryOperation) {
    generateBinaryOperation(code, statement)
}

func generateBinaryOperation(code *bytes.Buffer, binary binaryOperation) {
    generateOperation(code, binary.lOperand)
    code.WriteString(binary.operator + " ")
    generateOperation(code, binary.rOperand)
}

func generateOperation(code *bytes.Buffer, op interface{}) {
    binaryOp, ok := op.(binaryOperation)
    if (ok) {
        generateBinaryOperation(code, binaryOp)
    } else {
        unaryOp, ok := op.(unaryOperation)
        if (ok) {
            generateUnaryOperation(code, unaryOp)
        } else {
            generateConstOrVar(code, op)
        }
    }
}

func generateUnaryOperation(code *bytes.Buffer, unary unaryOperation) {
    code.WriteString(unary.operator + " ")
    generateConstOrVar(code, unary.operand)
}

func generateConstOrVar(code *bytes.Buffer, constOrVar interface{}) {
     constType, ok := constOrVar.(constNode)
     if (ok) {
         code.WriteString(strconv.Itoa(int(constType.value)))
     } else {
         varType := constOrVar.(variableNode)
         code.WriteString(varType.name)
     }
     code.WriteString(" ")
}

func parseStatement(line string) binaryOperation {
    sides := strings.Split(line, "->")
    
    // We flip the RHS and LHS for convenience
    lOperand := parseConstOrVar(strings.TrimSpace(sides[1]))
    rOperand := parseExpression(strings.TrimSpace(sides[0]))
    
    return binaryOperation {
        lOperand: lOperand,
        rOperand: rOperand,
        operator: "=",
    }
}

func parseExpression(expression string) interface{} {
    parts := strings.Split(expression, " ")

    switch len(parts) {
        case 1:
            return parseConstOrVar(strings.TrimSpace(parts[0]))
        case 2:
            return parseUnaryOperation(
                strings.TrimSpace(parts[0]),
                strings.TrimSpace(parts[1]))
        case 3:
            return parseBinaryOperation(
                strings.TrimSpace(parts[0]),
                strings.TrimSpace(parts[1]),
                strings.TrimSpace(parts[2]))
        default:
            panic("I have no idea how to parse this")
    }    
}

func parseUnaryOperation(part1 string, part2 string) unaryOperation {
    return unaryOperation {
        operator: parseOperator(part1),
        operand: parseConstOrVar(part2),
    }
}

func parseBinaryOperation(part1 string, part2 string, part3 string) binaryOperation {
    return binaryOperation {
        operator: parseOperator(part2),
        lOperand: parseConstOrVar(part1),
        rOperand: parseConstOrVar(part3),
    }
}

func parseOperator(operatorToken string) string {
    switch operatorToken {
        case "NOT":
            return "^"
        case "AND":
            return "&"
        case "OR":
            return "|"
        case "LSHIFT":
            return "<<"
        case "RSHIFT":
            return ">>"
        default:
            panic("Unknown operator " + operatorToken)
    }
}

func parseConstOrVar(token string) interface{} {
    numValue, err := strconv.Atoi(token)
    if (err != nil) {
        // Not a number
        if (token == "go") {
            token = "go2"
        }     
        if (token == "if") {
            token = "if2"
        }
        
        variables[token] = true
        
        return variableNode { name: token }
    }

    return constNode { value: uint16(numValue) }
}

func check(e error) {
    if e != nil {
        panic(e)
    }
}