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

maxDeviation1 :: Low -> High -> Approx
maxDeviation1 a0 b0 = [(b0 - a0) / fromIntegral (powDAC x) | x <- [1..]]

maxDeviation2 :: Low -> High -> Approx
maxDeviation2 a0 b0 = [(b0 - a0) / fromIntegral (powMemo x) | x <- [1..]]

divideAndConquer :: (p -> Bool) -> (p -> s) -> (p -> [p]) -> (p -> [s] -> s) -> p -> s
divideAndConquer indiv solve divide combine initPb = 
	dAC initPb
        where
          dAC pb
            | indiv pb = solve pb
            | otherwise = combine pb (map dAC (divide pb))

powDAC :: Integer -> Integer
powDAC t = divideAndConquer indiv solve divide combine t
	   where
	     indiv t		= (t == 0)
	     solve t
	       | t == 0		= 1
	       | otherwise 	= error "solve: problem divisible"
	     divide t		= [t-1,t-1]
	     combine _ [l1,l2] 	= l1 + l2

powMemo :: Integer -> Integer
powMemo 0 = 1
powMemo t = powerlist !! fromIntegral (t-1) + powerlist !! fromIntegral (t-1)

powerlist = [powMemo x | x <- [0..]]

-- ~~~~~~~~~~~~~~~~~
-- Exercise - Part 2
-- ~~~~~~~~~~~~~~~~~

--gen_turns :: Dartboard -> Turns
gen_turns :: Dartboard -> Turns
gen_turns d = concat (map (gen_turns' d) [1..])

gen_turns' :: Dartboard -> Throws -> Turns
gen_turns' a b = nub (map sort (mapM (const a) [1 .. b]))

--filters the turns that have the right amount of throws - slow
--filter_turns_th :: Turns -> Throws -> Turns
--filter_turns_th (a:as) th
-- | length a == th    = [a] ++ filter_turns_th as th
-- | length a > th     = []
-- | otherwise         = filter_turns_th as th
--filter_turns_th [] _ = []

--filters the turns that points sum up to the right value - slow
--filter_turns_ts :: Turns -> Throws -> Turns
--filter_turns_ts (a:as) ts
-- | sum a == ts       = [a] ++ filter_turns_ts as ts
-- | length a > ts     = []
-- | otherwise         = filter_turns_ts as ts
--filter_turns_ts [] _ = []

--filters turns, takes all results up until the length of target/smallest db number
filter_turns_ts :: Turns -> TargetScore -> Turns
--filter_turns_ts input target = filter (\x -> sum x == target) (takeWhile (\x -> length x < target+1) input)
filter_turns_ts input target = filter (\x -> sum x == target) (takeWhile (\x -> length x < (quot target (input!!0!!0))+1) input)

filter_turns_th :: Turns -> TargetScore -> Turns
filter_turns_th input target = filter (\x -> length x == target) (takeWhile (\x -> length x < target+1) input)

--selects the turns with the minimum length
select_turns_minl :: Turns -> Turns
select_turns_minl input =
    filter (\x -> length x == min) (takeWhile (\x -> length x < min+1) input)
        where min = minLen input

--transforms the turns to sorted lists
--this is already given by our implementation of gen_turns
transf_sort_turns :: Turns -> Turns
transf_sort_turns as = as

--used on a list of turns with the right target score, this returns the minimal length
minLen :: Turns -> Throws
minLen as = minimum (map (\x -> length x) as)

-- returns the turns that reach the target score
dart_ts :: Dartboard -> TargetScore -> Turns
-- old version
-- dart_ts d ts = filter_turns_ts (gen_turns d) ts
dart_ts d ts = filter_turns_ts (gen_turns (select_useful d ts)) ts

-- removes integers from dartboard that are greater than the target score
select_useful :: Dartboard -> TargetScore -> Dartboard
select_useful d ts = filter (<=ts) d

-- returns the turns with the right number of throws and target score
dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
dart_tst d ts th = filter_turns_ts (filter_turns_th (gen_turns d) th) ts

-- returns the turns that reach the target score with the minimal amount of throws, needs sorted results always (but gen_turns does that)
-- does dart_tst only until minl as determined by first element of dart_ts
dart_tsml :: Dartboard -> TargetScore -> Turns
dart_tsml d ts = select_turns_minl (dart_tst d ts (length ((dart_ts d ts)!!0)))
