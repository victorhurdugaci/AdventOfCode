import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Digest.Pure.MD5
import Data.List

salt = "ahsbgdzn"

type KeyCandidate = (Int,String)

genMd5 x = show $ md5 $ BS.fromString x
genKey1 x = genMd5 $ salt ++ (show x)
genKey2 x = foldr (\_ k -> genMd5 k) x [ n | n <- [1..2016]]

findNKeys :: Int -> [KeyCandidate] -> [KeyCandidate]
findNKeys n seq = findNKeys' n seq [] []
                  where
                  findNKeys' :: Int -> [KeyCandidate] -> [(Char, Int, KeyCandidate)] -> [KeyCandidate] -> [KeyCandidate]
                  findNKeys' n (x:xs) candidates keys
                      | (length keys) >= n = take n keys
                      | p3 == 'X'          = findNKeys' n xs (updateCandidates candidates) keys
                      | otherwise          = findNKeys' n xs candidates'' (keys ++ newKeys)
                      where
                      keys' = keys ++ newKeys
                      candidates'' = candidates' ++ [(p3,1000,x)]
                      (newKeys, candidates') = takeNextKeys $ updateCandidates $ matchPatterns p5 candidates
                                               where
                                               -- a key is good if there are no other keys that can be good keys in front of it
                                               takeNextKeys :: [(Char,Int,KeyCandidate)] -> ([KeyCandidate],[(Char,Int,KeyCandidate)])
                                               takeNextKeys []  = ([],[])
                                               takeNextKeys candidates
                                                   | remaining == -1 = (candidate:keys, candidates')
                                                   | otherwise      = ([], candidates)
                                                   where 
                                                   (keys, candidates') = takeNextKeys xs
                                                   (_,remaining,candidate) = x
                                                   (x:xs) = candidates
                      (p3, p5) = find3And5Patterns h ('X',[])
                                 where
                                 find3And5Patterns :: String -> (Char, [Char]) -> (Char, [Char])
                                 find3And5Patterns (a:b:c:xs) (p3, p5) = find3And5Patterns (b:c:xs) (p3', p5')
                                     where
                                     p3' = if p3 /= 'X' || (not isSame3) then p3 else a
                                     p5' = if isSame3 && ((length xs) > 1) && ((xs !! 0) == a) && ((xs !! 1) == a) then nub $ a:p5 else p5
                                     isSame3 = (a == b) && (b == c)
                                 find3And5Patterns _ x                 = x
                      (idx,h)  = x
                      updateCandidates :: [(Char,Int,KeyCandidate)] -> [(Char,Int,KeyCandidate)]
                      updateCandidates candidates
                          | candidates == []  = []
                          | remaining == 0    = candidates'                               -- eliminated
                          | remaining == -1   = x:(updateCandidates xs)                   -- this is a valid key
                          | otherwise         = (c, (remaining-1), candidate):candidates' -- this still has chances
                          where 
                          candidates' = updateCandidates xs
                          (c,remaining,candidate) = x
                          (x:xs) = candidates
                 
                  matchPatterns :: [Char] -> [(Char,Int,KeyCandidate)] -> [(Char,Int,KeyCandidate)]
                  matchPatterns _ []              = []
                  matchPatterns [] candidates     = candidates
                  matchPatterns (c:cs) candidates = matchPatterns cs (matchPattern c candidates [])
                      where
                          matchPattern :: Char -> [(Char,Int,KeyCandidate)] -> [(Char,Int,KeyCandidate)] -> [(Char,Int,KeyCandidate)]
                          matchPattern _ [] result = result
                          matchPattern c (x:xs) result
                              | c == c'                 = matchPattern c xs (result ++ [(c',-1,candidate)])
                              | otherwise               = matchPattern c xs (result ++ [x])
                              where (c',left,candidate) = x
                                                        
main = do
    let keyCandidateSeq1 = [ (n, genKey1 n)::KeyCandidate | n <- [0..] ]
    print $ last $ findNKeys 64 keyCandidateSeq1

    let keyCandidateSeq2 = [ (idx, genKey2 k1) | (idx,k1) <- keyCandidateSeq1]
    print $ last $ findNKeys 64 keyCandidateSeq2
