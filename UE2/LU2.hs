import Data.List

type Low = Double
type High = Double
type Approx = [Double]

type Points = Int -- Punktwert einer Dart-Scheibe; echt positive Zahl
type Dartboard = [Points] -- Dart-Scheibe charakterisiert durch Liste echt aufsteigender Punktwerte
type Turn = [Points] -- Punktwerte einer Wurffolge; nur Punktwerte, die auf der Scheibe vorkommen
-- (d.h., im Dartboard-Wert vorkommen) sind moeglich, auch mehrfach moeglich.
type Turns = [Turn] -- Strom von Wurffolgen
type TargetScore = Int -- Gewuenschte Zielpunktsumme > 0
type Throws = Int -- Anzahl von Wuerfen einer Wurffolge > 0

--maxDeviation1 :: Low -> High -> Approx

--powDAC :: Integer -> Integer
--powDAC = divideAndConquer...

--maxDeviation2 :: Low -> High -> Approx

--powMemo :: Integer -> Integer
--powMemo 0 = ...
--powMemo t = ...

-- ~~~~~~~~~~~~~~~~~
-- Exercise - Part 2
-- ~~~~~~~~~~~~~~~~~

--gen_turns :: Dartboard -> Turns
gen_turns :: Dartboard -> Turns
gen_turns d = concat (map (gen_turns' d) [1..])

gen_turns' :: Dartboard -> Throws -> Turns
gen_turns' a b = nub (map sort (mapM (const a) [1 .. b]))

--filters the turns that have the right amount of throws
filter_turns_th :: Turns -> Throws -> Turns
filter_turns_th (a:as) th
 | length a == th    = [a] ++ filter_turns_th as th
 | length a > th     = []
 | otherwise         = filter_turns_th as th
filter_turns_th [] _ = []

--filters the turns that points sum up to the right value
filter_turns_ts :: Turns -> Throws -> Turns
filter_turns_ts (a:as) ts
 | sum a == ts       = [a] ++ filter_turns_ts as ts
 | length a > ts     = []
 | otherwise         = filter_turns_ts as ts
filter_turns_ts [] _ = []

--selects the turns with the minimum length
select_turns_minl :: Turns -> Turns
select_turns_minl as =
 filter (\x -> length x == minLen as) as

--transforms the turns to sorted lists
--this is already given by our implementation of gen_turns
transf_sort_turns :: Turns -> Turns
transf_sort_turns as = as

--used on a list of turns with the right target score, this returns the minimal length
minLen :: Turns -> Throws
minLen as = minimum (map (\x -> length x) as)

-- returns the turns that reach the target score
dart_ts :: Dartboard -> TargetScore -> Turns
dart_ts d ts = filter_turns_ts (gen_turns d) ts

-- returns the turns with the right number of throws and target score
dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
dart_tst d ts th = filter_turns_ts (filter_turns_th (gen_turns d) th) ts

<<<<<<< HEAD:UE2/LU2.hs
-- returns the turns that reach the target score with the minimal amount of throws
=======
--performance problems with increased input values (maybe sort list by length in previous steps?)
>>>>>>> 2df5a5145ece16065011e3bbc1d6505963c6493c:UE2/lu2_christoph.hs
dart_tsml :: Dartboard -> TargetScore -> Turns
dart_tsml d ts = select_turns_minl (dart_ts d ts)
