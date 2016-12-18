import Data.Int (Int64)
import System.IO
import Debug.Trace

decompressOnePass :: String -> String
decompressOnePass []         = []
decompressOnePass ('(':rest) = decompressed ++ decompressOnePass unparsed where 
                               (decompressed, unparsed) = decompressSection rest
decompressOnePass (c:rest)   = c:decompressOnePass rest

decompressSection :: String -> (String, String)
decompressSection input = (concat(take count (repeat (take len rest))), drop len rest) where 
                          ((len, count), _, rest) = nextDecompressTuple input
                               
nextDecompressTuple :: String -> ((Int, Int), Int, String) -- ((section len, section count), section str len, remainder)
nextDecompressTuple input = ((len, count), intLen1 + intLen2 + 3, drop 1 afterCount) where -- final len is 3+ because (, x, )                              
                            (count, intLen2, afterCount) = nextInt (drop 1 afterLen) -- drop the 'x'
                            (len, intLen1, afterLen) = nextInt input

nextInt :: String -> (Int, Int, String) -- (int val, int val len, remainder)
nextInt input = (read intStr, intLen, remainingInput) where
                (intStr, intLen, remainingInput) = nextIntToken [] 0 input
                nextIntToken token len (c:rest)
                    | '0' <= c && c <= '9' = nextIntToken (token ++ [c]) (len+1) rest
                    | otherwise            = (token, len, c:rest)

decompressLenOnly :: String -> Int64
decompressLenOnly input = decompressLenOnlyLoop input (fromIntegral(length input)) 0

decompressLenOnlyLoop :: String -> Int64 -> Int64 -> Int64 -- input, remaining length, count
decompressLenOnlyLoop _          0      cnt = cnt -- end of len
decompressLenOnlyLoop []         _      cnt = cnt -- end of input
decompressLenOnlyLoop ('(':rest) remLen cnt = count64 * (decompressLenOnlyLoop afterTuple len64 0) + afterLen where
                                              afterLen = decompressLenOnlyLoop afterSection (remLen-tupleLen64-len64) cnt
                                              afterSection =take (fromIntegral (remLen-tupleLen64-len64)) (drop (tupleLen + len - 1) rest)
                                              ((len64, count64), tupleLen64, _) = ((fromIntegral(len), fromIntegral(count)), fromIntegral(tupleLen), afterTuple)
                                              ((len, count), tupleLen, afterTuple) = nextDecompressTuple rest
decompressLenOnlyLoop (c:rest)   remLen cnt = decompressLenOnlyLoop rest (remLen-1) (cnt+1)

main = do
    content <- readFile "input.txt"
    
    let decompressed = decompressOnePass content
    print (length decompressed)

    let decompressLen = decompressLenOnly content
    print decompressLen