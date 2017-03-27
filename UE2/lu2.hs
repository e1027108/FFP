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

--needs rework -> should work without Throws parameter
gen_turns' :: Dartboard -> Throws -> Turns
gen_turns' a b = nub (map sort (mapM (const a) [1 .. b]))

--terminating filter variants - run into non-exhaustive patterns :(
--filter_turns_ts :: Turns -> TargetScore -> Turns
--filter_turns_ts (x:xs) target
--    | sum x == target = [x] ++ filter_turns_ts xs target
--    | length x > target = [] -- sums of more than target values can't be target or lower
--    | otherwise = filter_turns_ts xs target

--filter_turns_th :: Turns -> Throws -> Turns
--filter_turns_th (x:xs) amount
--    | length x == amount = [x] ++ filter_turns_th xs amount
--    | length x > amount = []
--    | otherwise = filter_turns_th xs amount
    
--now terminate
filter_turns_ts :: Turns -> TargetScore -> Turns
filter_turns_ts input target = filter (\x -> sum x == target) (takeWhile (\x -> length x < target+1) input)

filter_turns_th :: Turns -> Throws -> Turns
filter_turns_th input amount = filter (\x -> length x == amount) (takeWhile (\x -> length x < amount+1) input)

--select_turns_minl :: Turns -> Turns

--transf_sort_turns :: Turns -> Turns

--dart_ts :: Dartboard -> TargetScore -> Turns

dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
dart_tst d target amount = filter_turns_ts (filter_turns_th (gen_turns d) amount) target

--dart_tsml :: Dartboard -> TargetScore -> Turns
