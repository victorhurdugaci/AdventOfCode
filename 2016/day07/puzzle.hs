import System.IO

type IP7 = [String]
type Char3 = (Char,Char,Char)

textToIP :: String -> IP7
textToIP "" = [""]
textToIP (c:rest) 
    | c == '[' || c == ']' = "" : textToIP rest
    | otherwise            = (c : head s) : tail s 
                             where s = textToIP rest 

getPalindrome3 :: String -> [Char3]
getPalindrome3 (c1:c2:c3:rest)
    | (c1 /= c2) && (c1 == c3) = (c1,c2,c3):getPalindrome3 (c2:c3:rest)
    | otherwise                = getPalindrome3 (c2:c3:rest)
getPalindrome3 _               = [] 

hasPalindrome4 :: String -> Bool
hasPalindrome4 (c1:c2:c3:c4:rest)
    | (c1 /= c2) && (c1 == c4) && (c2 == c3) = True
    | otherwise                              = hasPalindrome4 (c2:c3:c4:rest)
hasPalindrome4 _                             = False 

evaluateTLS :: IP7 -> (Bool, Bool)
evaluateTLS (c1:c2:rest) = (b1 || hasPalindrome4 c1, b2 ||  hasPalindrome4 c2) 
                           where (b1,b2) = evaluateTLS rest
evaluateTLS (c1:_)       = (hasPalindrome4 c1, False)
evaluateTLS []           = (False, False)

supportsTLS :: IP7 -> Bool
supportsTLS ip = b1 && not b2 
                 where (b1,b2) = evaluateTLS ip


isPalindrome3Flip :: (Char3, Char3) -> Bool
isPalindrome3Flip ((a1,b1,c1),(a2,b2,c2)) = (a1 == c1) && (a2 == c2) && 
                                          (a1 /= b1) && (a2 /= b2) &&
                                          (a1 == b2) && (a2 == b1)

extractPalindrome3 :: IP7 -> ([Char3], [Char3])
extractPalindrome3 (c1:c2:rest) = ((getPalindrome3 c1) ++ p31, (getPalindrome3 c2) ++ p32)
                                  where (p31,p32) = extractPalindrome3 rest
extractPalindrome3 (c:_)        = (getPalindrome3 c, [])
extractPalindrome3 []           = ([],[])

supportsSSL :: IP7 -> Bool
supportsSSL ip =  anyMatch isPalindrome3Flip comb
                  where 
                    comb = [(x,y) | x <- c31, y <- c32] 
                    (c31,c32) = extractPalindrome3 ip

anyMatch :: (a -> Bool) -> [a] -> Bool
anyMatch f (e:rest) = if (f e) then True else (anyMatch f rest)
anyMatch _ []       = False 

countIf :: (a -> Bool) -> [a] -> Int
countIf f lst = countLoop f lst 0 where
                countLoop fc (x:xs) i = countLoop fc xs (i + if (fc x) then 1 else 0)
                countLoop fc [] i     = i

main = do
    content <- readFile "input.txt"
    let ips = [ textToIP t | t <- lines content]

    print (countIf supportsTLS ips)
    print (countIf supportsSSL ips)
