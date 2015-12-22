package main

import (
    "fmt"
    "reflect"
    "strings"
    "testing"
)

func Test_parseConstOrVar(t *testing.T) {
    ast := parseConstOrVar("abc")
    assertEqual(t, "variableNode", reflect.TypeOf(ast).Name())
    assertEqual(t, "abc", ast.(variableNode).name)
    
    ast = parseConstOrVar("42")
    assertEqual(t, "constNode", reflect.TypeOf(ast).Name())
    assertEqual(t, uint16(42), ast.(constNode).value)
}

func Test_parseOperator(t *testing.T) {
    assertEqual(t, "&", parseOperator("AND"))
    assertEqual(t, "|", parseOperator("OR"))
    assertEqual(t, ">>", parseOperator("RSHIFT"))
    assertEqual(t, "<<", parseOperator("LSHIFT"))
    assertEqual(t, "^", parseOperator("NOT"))
}

func Test_parseUnaryOperation(t *testing.T) {
    unaryOp := parseUnaryOperation("NOT", "abc")
    assertEqual(t, "^", unaryOp.operator)
    assertEqual(t, "variableNode", reflect.TypeOf(unaryOp.operand).Name())
}

func Test_parseBinaryOperation(t *testing.T) {
    binOp := parseBinaryOperation("42", "AND", "abc")
    assertEqual(t, "&", binOp.operator)
    assertEqual(t, "constNode", reflect.TypeOf(binOp.lOperand).Name())
    assertEqual(t, "variableNode", reflect.TypeOf(binOp.rOperand).Name())
}

func Test_parseStatement(t *testing.T) {
    parseStatemenTest(t, "dz AND ef -> eh", "binaryOperation")
    parseStatemenTest(t, "jp RSHIFT 3 -> jr", "binaryOperation")
    parseStatemenTest(t, "lc LSHIFT 1 -> lw", "binaryOperation")
    parseStatemenTest(t, "NOT fx -> fy", "unaryOperation")
    parseStatemenTest(t, "44430 -> b", "constNode")
}

func Test_generateCodeFromStatements(t *testing.T) {
    generateCodeFromStatementsTest(t,
        "dz AND ef -> eh",
        "eh = dz & ef")
    generateCodeFromStatementsTest(t,
        "NOT gs -> gt",
        "gt = ^ gs")
    generateCodeFromStatementsTest(t,
        "44430 -> b",
        "b = 44430")
    generateCodeFromStatementsTest(t,
        "lv LSHIFT 15 -> lz",
        "lz = lv << 15")
        
    // reserved words
    generateCodeFromStatementsTest(t,
        "go LSHIFT 15 -> if",
        "if2 = go2 << 15")
}

func generateCodeFromStatementsTest(t *testing.T, statement string, expected string) {
    statements := []binaryOperation { parseStatement(statement) }
    code := generateCodeFromStatements(statements)
    code = strings.TrimSpace(code)
    assertEqual(t, expected, code)
}

func parseStatemenTest(t *testing.T, input string, expectedROperand string) {
    binOp := parseStatement(input)
    assertEqual(t, "=", binOp.operator)
    assertEqual(t, "variableNode", reflect.TypeOf(binOp.lOperand).Name())
    assertEqual(t, expectedROperand, reflect.TypeOf(binOp.rOperand).Name())
}

func assertEqual(t *testing.T, expected interface{}, actual interface{}) {
    if expected != actual {
        t.Error(fmt.Sprintf("Expected:%v; got: %v;", expected, actual));
    }
}