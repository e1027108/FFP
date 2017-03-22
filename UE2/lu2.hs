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

--filter_turns_ts :: Turns -> TargetScore -> Turns

--filter_turns_th :: Turns -> Throws -> Turns

--select_turns_minl :: Turns -> Turns

--transf_sort_turns :: Turns -> Turns

--dart_ts :: Dartboard -> TargetScore -> Turns

--dart_tst :: Dartboard -> TargetScore -> Throws -> Turns

--dart_tsml :: Dartboard -> TargetScore -> Turns