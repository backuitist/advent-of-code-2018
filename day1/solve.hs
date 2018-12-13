import System.IO
import qualified Data.Set as Set

import Debug.Trace (trace)

-- part 1
sumFile = do
    f <- readFile "input"    
    putStrLn (show $ sumLines (lines f))

sumLines ls = foldl sumLine 0 ls

sumLine acc line =
    let 
        (sign,stringValue) = splitAt 1 line
        value = read stringValue :: Int
    in case sign of
        "+" -> acc + value
        "-" -> acc - value
        _   -> error "illegal sign"

        
-- part 2
findFirstDuplicateFreq = do
    f <- readFile "input"
    let lines' = lines f
    putStrLn (show $ findFirstDuplicateInLines lines' lines' (Set.empty,0))

findFirstDuplicateInLines lines' currentLines frequenciesTotal =    
    let (frequencies, total) = frequenciesTotal
    in case currentLines of
        [] -> findFirstDuplicateInLines lines' lines' frequenciesTotal
        line : tail -> 
            let newTotal = sumLine total line
                newFrequenciesTotal = (Set.insert total frequencies, newTotal)
            in case (Set.member newTotal frequencies) of
                True -> newTotal
                False -> findFirstDuplicateInLines lines' tail (newFrequenciesTotal)
