import Data.List
import Data.Ord

-- compute naive and smart solution for the Maximum Segment Sum (MSS) Problem
-- testdata: simpleMSS [-4, -3, -7, 2, 1, -2, -1, -4]


-- naive solution
simpleMSS :: [Int] -> [Int]
simpleMSS = maximumBy (comparing sum) . segments

-- compute a list of contiguous subsequences (segments)
segments :: [Int] -> [[Int]]
segments = concatMap inits . tails
    where inits = takeWhile ((>1) . length) . takeWhile (not . null) . iterate init
          tails = takeWhile ((>1) . length) . takeWhile (not . null) . iterate tail

-- just for debugging
printSum :: [Int] -> [Int]
printSum = map sum . segments

-- smart solution
-- smartMSS :: [Int] -> [Int]
