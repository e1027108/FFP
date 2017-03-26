import Data.List

type Points      = Int      -- Punktwert eines Feldes, echt positive Zahl
type Dartboard   = [Points] -- Dart-Scheibe, charakterisiert durch Liste echt aufsteigender Punktwerte
type Turn        = [Points] -- Punktwerte einer Wurffolge, nur Elemente aus Dartboard (mehrfach) moeglich
type Turns       = [Turn]   -- Strom von Wurffolgen
type TargetScore = Int      -- Gewuenschte Zielpunktsumme > 0
type Throws      = Int      -- Anz. von Wuerfen einer Wurffolge > 0

--executes gen_single for all sub list lengths (e.g. [1,2,3] -> [1], [1,2], [1,2,3]
gen_all :: (Ord a, Eq a) => [a] -> [[a]] -> [[a]] -> Int -> Int -> [[a]]
gen_all as bs ret i0 iN
 | bs == [] = gen_all as startList (ret ++ startList) 1 iN
 | i0 < iN = gen_all as (gen_single as bs [] 0) (ret ++ (gen_single as bs [] 0)) (i0+1) iN
 | otherwise = ret
 where startList = map makeList as

-- adds one element of the dartboard to all the lists
-- for all elements in the dartboard
gen_single :: (Ord a) => [a] -> [[a]] -> [[a]] -> Int -> [[a]]
gen_single as bs ret i0
 | i0 < length as = gen_single as bs (ret ++ (map (addElem (as!!i0)) bs)) (i0+1)
 | otherwise = removeDupl ret

makeList :: a -> [a]
makeList a = [a]

--hilfsfunktion mappt a aus as zu jedem element in bs
addElem :: a -> [a] -> [a]
addElem a bs = a:bs

--find duplicate tuples
duplList :: (Ord a) => [a] -> [a] -> Bool
duplList as bs
 | sort as == sort bs = True
 | otherwise = False

--remove duplicates from list
removeDupl :: (Ord a) => [[a]] -> [[a]]
removeDupl [] = []
removeDupl [x] = [x]
removeDupl (x1:x2:xs) = nub (map sort (x1:x2:xs))
