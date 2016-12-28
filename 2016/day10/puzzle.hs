import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import System.IO

type Bot = (Int, Int) -- two registers
type Bots = Map.Map String Bot -- the output is also bots. They start with "O_"

botAssign :: Int -> Maybe Bot -> Bot
botAssign val Nothing = (val, 0)
botAssign val (Just (v,0)) = (min v val, max v val)

botCanRun :: Maybe Bot -> Bool
botCanRun Nothing = False
botCanRun (Just (v1,v2)) = (v1 /= 0) && (v2 /= 0)

data Instruction = Assign String Int |           -- Bot, Value
                   Give String String String     -- Bot, lBot, hBot
                   deriving (Show)

parseInstructions :: [String] -> [Instruction]
parseInstructions txt = parseLoop txt []
                        where
                        parseLoop [] instr = instr
                        parseLoop (l:rest) instr = parseLoop rest $ instr ++ [parseInstruction l]

parseInstruction :: String -> Instruction
parseInstruction txt
    | (t !! 0) == "bot" = Give ("B_" ++ (t !! 1)) ((botOrOut $ t !! 5) ++ (t !! 6 )) ((botOrOut $ t !! 10) ++ (t !! 11))
    | otherwise         = Assign ("B_" ++  (t !! 5)) (read $ t !! 1)
    where
    t = tokenizeLine txt
    botOrOut txt = if txt == "output" then "O_" else "B_"

tokenizeLine :: String -> [String]
tokenizeLine input = tokenizeLoop input "" []
                     where
                     tokenizeLoop []         token tokens = tokens ++ [token]
                     tokenizeLoop (' ':rest) token tokens = tokenizeLoop rest "" (tokens ++ [token])
                     tokenizeLoop (c:rest)   token tokens = tokenizeLoop rest (token ++ [c]) tokens

splitInstructions :: [Instruction] -> ([Instruction],[Instruction])
splitInstructions instr = (assignInstr, giveInstr)
                          where
                          assignInstr = [ x | x@Assign {} <- instr]
                          giveInstr = [ y | y@Give {} <- instr]

execInstruction :: Instruction -> Bots -> Bots
execInstruction (Assign botId val) bots     = Map.insert botId (botAssign val $ Map.lookup botId bots) bots
execInstruction (Give botId lBot hBot) bots = if ((lVal == 17) && (hVal == 61)) then trace botId result else result -- hacky way to get the bot that compares 17 and 61
                                              where
                                              result = Map.delete botId b2
                                              b2 = execInstruction (Assign hBot hVal) b1
                                              b1 = execInstruction (Assign lBot lVal) bots
                                              Just (lVal, hVal) = Map.lookup botId bots

execAssigns :: [Instruction] -> Bots
execAssigns instr = execLoop instr Map.empty
                    where
                    execLoop [] bots     = bots
                    execLoop (x:xs) bots = execLoop xs $ execInstruction x bots

execUntilNotPossible :: [Instruction] -> Bots -> Bots
execUntilNotPossible instr bots = if ran then execUntilNotPossible instr newBots else bots
                                  where
                                  (ran, newBots) = execFirstPossible instr bots

execFirstPossible :: [Instruction] -> Bots -> (Bool, Bots)
execFirstPossible [] bots     = (False, bots)
execFirstPossible (x:xs) bots = if ran then (True, result) else execFirstPossible xs bots
                                where
                                (ran, result) = execIfPossible x bots

execIfPossible :: Instruction -> Bots -> (Bool, Bots)
execIfPossible instr bots = (canRun, if canRun then execInstruction instr bots else bots)
                            where
                            canRun = botCanRun $ Map.lookup botId bots
                            (Give botId lBot hBot) = instr

main = do
    content <- readFile "input.txt"
    let instructionsTxt = lines content

    let (assigns, gives) = splitInstructions $ parseInstructions instructionsTxt

    let startState = execAssigns assigns
    let finalState = execUntilNotPossible gives startState

    let (Just (v0,_)) = Map.lookup "O_0" finalState
    let (Just (v1,_)) = Map.lookup "O_1" finalState
    let (Just (v2,_)) = Map.lookup "O_2" finalState

    print (v0 * v1 * v2)
