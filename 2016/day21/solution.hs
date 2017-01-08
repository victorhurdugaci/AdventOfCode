import Data.List
import Data.List.Split
import Data.Maybe
import System.IO

data Instruction = SwapP Int Int |
                   SwapL Char Char |
                   RotL Char |
                   RotP Bool Int |
                   Rev Int Int |
                   Mov Int Int
                   deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction txt
    | t0 == "swap" && t1 == "position" = SwapP (read $ t !! 2) (read $ t !! 5)
    | t0 == "swap" && t1 == "letter"   = SwapL (head $ t !! 2) (head $ t !! 5)
    | t0 == "rotate" && t1 == "based"  = RotL (head $ t !! 6)
    | t0 == "rotate"                   = RotP (t1 == "left") (read $ t !! 2)
    | t0 == "reverse"                  = Rev (read $ t !! 2) (read $ t !! 4)
    | t0 == "move"                     = Mov (read $ t !! 2) (read $ t !! 5)
    where t0 = t !! 0
          t1 = t !! 1
          t = splitOn " " txt

executeInstruction :: Instruction -> String -> String
executeInstruction (SwapP a b) s 
    | a > b                         = executeInstruction (SwapP b a) s
    | otherwise                     = (take a s) ++ [s !! b] ++ (take (b-a-1) $ drop (a+1) s) ++ [s !! a] ++ (drop (b+1) s)
executeInstruction (SwapL a b) s    = executeInstruction (SwapP p1 p2) s
                                      where p1 = fromJust $ findIndex (\c -> c == a) s
                                            p2 = fromJust $ findIndex (\c -> c == b) s
executeInstruction (RotP True p) s  = take l (drop (p `mod` l) (s ++ s))
                                      where l = length s
executeInstruction (RotP False p) s = take l (drop (l-(p `mod` l)) (s ++ s))
                                      where l = length s
executeInstruction (RotL a) s
    | p >= 4                        = executeInstruction (RotP False $ 2 + p) s
    | otherwise                     = executeInstruction (RotP False $ 1 + p) s
    where p = fromJust $ findIndex (\c -> c == a) s
executeInstruction (Rev a b) s
    | a > b                         = executeInstruction (Rev b a) s
    | otherwise                     = (take a s) ++ (reverse $ take ((b-a)+1) $ drop a s) ++ (drop (b+1) s)
executeInstruction (Mov a b) s
    | a > b                         = (take b s) ++ [ s !! a ] ++ (take (a-b) $ drop b s) ++  (drop (a+1) s)
    | otherwise                     = (take a s) ++ (take (b-a) $ drop (a+1) s) ++ [s !! a] ++ (drop (b+1) s)

executeInstruction' :: Instruction -> String -> String
executeInstruction' (RotL c) s        = executeInstruction (RotP True p) s
                                        where  p = if cp == 0 then 1
                                                   else if odd cp then ((cp+1) `div` 2)
                                                        else ((cp `div` 2) + 5) `mod` 8
                                               cp = fromJust $ findIndex (\x -> x == c) s
executeInstruction' (RotP isLeft p) s = executeInstruction (RotP (not isLeft) p) s
executeInstruction' (Mov a b) s       = executeInstruction (Mov b a) s
executeInstruction' x s               = executeInstruction x s

main = do
      input <- readFile "input.txt"
      let instructions = map (parseInstruction) $ lines input
      
      -- Part 1 --
      print $ foldl(\x i -> executeInstruction i x) "abcdefgh" instructions

      -- Part 2 --
      print $ foldl(\x i -> executeInstruction' i x) "fbgdceah" $ reverse instructions
