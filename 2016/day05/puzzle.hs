import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Digest.Pure.MD5
import Data.List

replaceNthChar :: Int -> Char -> String -> String
replaceNthChar n rc (c:rest)
    | n == 0          = rc:rest
    | otherwise       = c:replaceNthChar (n-1) rc rest
replaceNthChar _ _ [] = []

findCode2 :: [(Int, Char)] -> String -> String
findCode2 ((d,c):rest) currentCode 
    | not ('.' `elem` currentCode) = currentCode
    | (currentCode !! d) == '.'    = findCode2 rest (replaceNthChar d c currentCode)
    | otherwise                    = findCode2 rest currentCode

main :: IO ()
main = do
    let input = "ffykfhsq"

    let hashSeq = [ show (md5 (BLU.fromString (input ++ (show c)))) | c <- [1..] ]
    let fiveZeroHashSeq = [ h | h <- hashSeq, "00000" `isPrefixOf` h ]

    let code1 = take 8 [ (h !! 5) | h <- fiveZeroHashSeq ]
    print code1

    let code2HashSeq = [ (read [c6], c7) | (_:_:_:_:_:c6:c7:_) <- fiveZeroHashSeq, '0' <= c6, c6 <= '7' ] :: [(Int, Char)]
    let code2 = findCode2 code2HashSeq (take 8 (repeat '.')) 
    print code2
