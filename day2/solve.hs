import System.IO
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

import Combinatorics (tuples)

checksum = do
    f <- readFile "input"
    let (doubles, triples) = foldl countLine (0,0) (lines f)          
        countLine doubleTriple line =
            let (double, triple) = doubleTriple
                freqs = Map.fromListWith (+) $ zip line (repeat 1)
            in (double + (countOccurrence freqs 2), triple + (countOccurrence freqs 3)) -- trace ("freqs = " ++ (show freqs)) 
            where
                countOccurrence freqs num = Maybe.fromMaybe 0 (fmap (\_ -> 1) (List.find (\l -> snd l == num) (Map.toList freqs)))
    print (doubles * triples)
    
-- part 2
commonLetters = do
    f <- readFile "input"            
    let combinations = tuples 2 (lines f)
        sameLetters lst = map fst $ filter (\a -> (fst a) == (snd a)) (zip (head lst) (last lst))
        length' a b = compare (length a) (length b)
    print (List.maximumBy length' (map sameLetters combinations))