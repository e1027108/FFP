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

--gen_turns :: Dartboard -> Turns
gen_turns :: Dartboard -> Turns
gen_turns d = concat (map (gen_turns' d) [1..])

gen_turns' :: Dartboard -> Throws -> Turns
gen_turns' a b = nub (map sort (mapM (const a) [1 .. b]))

--filters the turns that points sum up to the right value
filter_turns_ts :: Turns -> TargetScore -> Turns
filter_turns_ts as ts = filter (\x -> sum x == ts) as

--filters the turns that have the right amount of throws
filter_turns_th :: Turns -> Throws -> Turns
filter_turns_th as th = filter (\x -> length x == th) as

--selects the turns with the minimum length
select_turns_minl :: Turns -> Turns
select_turns_minl as = filter (\x -> length x == m) as where m = minLen as

minLen :: [[a]] -> Int
minLen as = minimum (map (\x -> length x) as)

--transforms the turns to sorted lists
--this is already given by our implementation of gen_turns
transf_sort_turns :: Turns -> Turns
transf_sort_turns as = as

--stops gen_turns when the number of throws in a turn > TargetScore
ts_stop :: TargetScore -> Turns -> Turns -> Int -> Turns
ts_stop ts (a:as) turns i0
 | length a <= ts = ts_stop ts as turns (i0+1)
 | otherwise = take i0 turns

-- returns the turns with the right value of points
dart_ts :: Dartboard -> TargetScore -> Turns
dart_ts d ts = filter_turns_ts (ts_stop ts (gen_turns d) (gen_turns d) 0) ts

--dart_tst :: Dartboard -> TargetScore -> Throws -> Turns

--performance problems with increased input values (maybe sort list by length in previous steps?)
dart_tsml :: Dartboard -> TargetScore -> Turns
dart_tsml d ts = select_turns_minl (dart_ts d ts)
