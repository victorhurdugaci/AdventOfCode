import qualified Data.Map as Map
import Data.Maybe
import System.IO
import Text.Read

type Registers = Map.Map String Int

readReg :: String -> Registers -> Int
readReg name registers = fromMaybe 0 $ Map.lookup name registers

writeReg :: String -> Int -> Registers -> Registers
writeReg name value registers = Map.insert name value registers

data Instruction = CpyR String String |
                   CpyC Int String |
                   JnzR String Int |
                   JnzC Int Int |
                   Inc String |
                   Dec String
                   deriving (Show)

parseInstructions :: [String] -> [Instruction]
parseInstructions txt = parseLoop txt []
                        where
                        parseLoop [] instr = instr
                        parseLoop (l:rest) instr = parseLoop rest $ instr ++ [parseInstruction l]

parseInstruction :: String -> Instruction
parseInstruction txt
    | (instr == "cpy") && (isNothing arg1Int)  = CpyR (t !! 0) (t !! 1)
    | (instr == "cpy")                         = CpyC (fromJust arg1Int) (t !! 1)
    | (instr == "jnz") && (isNothing arg1Int)  = JnzR (t !! 0) (read $ t !! 1)
    | (instr == "jnz")                         = JnzC (fromJust arg1Int) (read $ t !! 1)
    | instr == "inc"                           = Inc (t !! 0)
    | instr == "dec"                           = Dec (t !! 0)
    where 
    arg1Int   = readMaybe (t !! 0) :: Maybe Int
    (instr:t) = tokenizeLine txt
   
tokenizeLine :: String -> [String]
tokenizeLine input = tokenizeLoop input "" []
                     where
                     tokenizeLoop []         token tokens = tokens ++ [token]
                     tokenizeLoop (' ':rest) token tokens = tokenizeLoop rest "" (tokens ++ [token])
                     tokenizeLoop (c:rest)   token tokens = tokenizeLoop rest (token ++ [c]) tokens

executeInstructions :: [Instruction] -> (Int, Registers) -> Registers
executeInstructions instr frame 
    | execP >= (length instr) = registers
    | otherwise               = executeInstructions instr $ executeInstruction (instr !! execP) frame
    where
    (execP, registers) = frame

executeInstruction :: Instruction -> (Int, Registers) -> (Int, Registers)
executeInstruction (CpyR nameSrc nameDst) (execP, registers) = (execP+1, writeReg nameDst (readReg nameSrc registers) registers)
executeInstruction (CpyC value name) (execP, registers)      = (execP+1, writeReg name value registers)
executeInstruction (JnzR name offset) (execP, registers)      = (if (readReg name registers) /= 0 then execP + offset else execP + 1, registers)
executeInstruction (JnzC value offset) (execP, registers)      = (if value /= 0 then execP + offset else execP + 1, registers)
executeInstruction (Inc name) (execP, registers)             = (execP+1, writeReg name ((readReg name registers) + 1) registers)
executeInstruction (Dec name) (execP, registers)             = (execP+1, writeReg name ((readReg name registers) - 1) registers)

main = do
    content <- readFile "input.txt"
    
    let instructions1 = parseInstructions $ lines content
    let output1 = executeInstructions instructions1 (0, Map.empty)
    print output1

    let instructions2 = (CpyC 1 "c"):instructions1
    let output2 = executeInstructions instructions2 (0, Map.empty)
    print output2