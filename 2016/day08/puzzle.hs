import Data.Bits
import Data.Char (intToDigit)
import Data.Int (Int64)
import Numeric (showIntAtBase)
import System.IO

data Op = Rect | RotC | RotR deriving (Eq,Ord,Enum,Show)
type Instr = (Op, Int, Int)
type Screen = [Int64] -- The integers are rows. The bits in the ints are columns

appendIfNotEmpty ::  String -> [String] -> [String]
appendIfNotEmpty e lst
    | e == ""   = lst
    | otherwise = lst ++ [e]

getTokens :: String -> [String]
getTokens txt = getTokensLoop txt "" [] where
                getTokensLoop (c:rest) token result
                    | c == ' ' || c == '=' || c == 'x' || c == 'y' = getTokensLoop rest "" (appendIfNotEmpty token result)
                    | otherwise                                    = getTokensLoop rest (token ++ [c]) result
                getTokensLoop [] token result                      = appendIfNotEmpty token result

parseInstruction :: [String] -> Instr
parseInstruction (op:rest) 
    | op == "rect"   = (Rect, 
                        read (rest !! 0), 
                        read (rest !! 1))
    | op == "rotate" = (if (rest !! 0) == "column" then RotC else RotR, 
                        read (rest !! 1), 
                        read (rest !! 3)) 

rowToString :: Int64 -> String
rowToString r = map repl b2Str where
                b2Str = (take (50 - length b2) (repeat '0')) ++ b2 
                b2 = showIntAtBase 2 intToDigit r ""
                repl '0' = ' '
                repl '1' = '@'

screenToString :: Screen -> String
screenToString (r:rest) = rowToString r ++ "\n" ++ screenToString rest
screenToString []       = ""

setBitInInt :: Int -> Int -> Int64 -> Int64
setBitInInt x val row = if (val == 1) then setBit row bitPos
                                      else clearBit row bitPos
                                      where bitPos = 50 - x - 1

rect :: Int -> Int -> Screen -> Screen
rect w h screen = rectLoop w h screen 0
                  where
                      rectLoop _ _ [] _ = []
                      rectLoop w h (row:rest) rIdx
                          | rIdx >= h = (row:rest)
                          | otherwise = (row .|. ((1 `shift` w) -1) `shift` (50 - w)):
                                          rectLoop w h rest (rIdx+1)
                      
rotateRow :: Int -> Int -> Screen -> Screen
rotateRow r n scr = rotateRowLoop r n scr 0
                    where 
                        rotateRowLoop _ _ [] _ = []
                        rotateRowLoop r n (row:rest) rIdx
                            | r /= rIdx = row:rotateRowLoop r n rest (rIdx+1)
                            | otherwise = ((row `shift` (-n)) .|. (fallBits `shift` (50 - n))):rest
                                          where fallBits = row .&. ((1 `shift` n) - 1)

rotateCol :: Int -> Int -> Screen -> Screen
rotateCol c n scr = rotateColLoop c n scr 0 (extractCol c scr)
                    where
                        rotateColLoop _ _ [] _ _ = []
                        rotateColLoop c n (row:rest) rIdx newCol = (setBitInInt c (newCol !! ((rIdx - n) `mod` 6)) row):rotateColLoop c n rest (rIdx+1) newCol
                        extractCol _ []         = []
                        extractCol n (row:rest) = bitVal:extractCol n rest
                                                  where bitVal = if (testBit row (50 -1 - n)) then 1 else 0

executeInstructions :: [Instr] -> Screen
executeInstructions [] = [0,0,0,0,0,0]
executeInstructions (instr:rest) = executeSingleInstruction instr screen
                                   where screen = executeInstructions rest

executeSingleInstruction :: Instr -> Screen -> Screen
executeSingleInstruction (op, x, y) screen
    | op == Rect = rect x y screen
    | op == RotC = rotateCol x y screen
    | op == RotR = rotateRow x y screen

countSetPixels :: Screen -> Int
countSetPixels scr = sum [popCount row | row <- scr]

main = do
    content <- readFile "input.txt"
    let instructions = [ parseInstruction (getTokens line) | line <- lines content ]
   
    let finalScreen = executeInstructions (reverse instructions)

    putStr(screenToString finalScreen)
   
    print (countSetPixels finalScreen)
