import Data.Char (chr, ord)
import Data.List
import Data.Map
import Text.Regex.Posix
import System.IO

textToRoom :: String -> (String, Int, String)
textToRoom txt = (name, read id, checksum) :: (String, Int, String)
    where  (_, _, _, [name, id, checksum]) = (txt =~ "([^0-9]+)-([0-9]+)[[](.+)[]]" :: (String,String,String,[String]))

freqComparator :: (Char, Int) -> (Char, Int) -> Ordering
freqComparator (a1, b1) (a2, b2)
    | b1 /= b2  = compare b2 b1
    | otherwise = compare a1 a2

computeChecksum :: String -> String
computeChecksum txt = 
    [ c | (c, _) <- take 5 (sortBy freqComparator freqs)] where 
        freqs = (toList $ fromListWith (+) [ (c, 1) | c <- txt, c /= '-' ])

decryptName :: (String, Int) -> String
decryptName ([], _)           = ""
decryptName (('-':rest), key) = ' ' : decryptName (rest,key)
decryptName ((c:rest), key)   = decryptedChar : decryptName (rest,key)
    where decryptedChar = chr (ord 'a' + (((ord c) - (ord 'a') + key) `mod` 26))
    
main = do
    content <- readFile "input.txt"
    let parsedRooms = [ textToRoom txt | txt <- lines content ]

    let validRooms = [ (name, id) | (name, id, checksum) <- parsedRooms, computeChecksum name == checksum ]

    let sumValidId = sum [ id | (_, id) <- validRooms ] 
    print sumValidId

    let northPoleRoom = [ r | r <- validRooms, isInfixOf "northpole" (decryptName r) ]
    print northPoleRoom

