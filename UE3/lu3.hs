type Points = Int -- Punktwert einer Dart-Scheibe; echt positive Zahl
type Dartboard = [Points] -- Dart-Scheibe charakterisiert durch Liste echt aufsteigender Punktwerte
type Turn = [Points] {- Punktwerte einer Wurffolge; nur Punktwerte, die auf der Scheibe vorkommen
(d.h., im Dartboard-Wert vorkommen) sind moeglich, auch mehrfach moeglich. -}
type Turns = [Turn] -- Strom von Wurffolgen
type TargetScore = Int -- Gewuenschte Zielpunktsumme > 0
type Throws = Int -- Anzahl von Wuerfen einer Wurffolge > 0

--is this allowed? we'll see
data Node = Empty | N Turn Throws TargetScore [Node]

--bt_dart_ts :: Dartboard -> TargetScore -> Turns

--succ_ts :: Node -> [Node]
--goal_ts :: Node -> Bool

--computes all Turns of specidfied target score in specified amount of throws
bt_dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
bt_dart_tst d s t = []

{-searchDfs :: (Eq node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchDfs succ goal x = (search' (push x emptyStack) )
    where
        search' s
            | stackEmpty s = []
            | goal (top s) = top s : search' (pop s)
            | otherwise = let x = top s
                in search' (foldr push (pop s) (succ x))-}

succ_tst :: Node -> [Node]
succ_tst (N turn th _ n)
    | th <= (length turn) = n --if we haven't gone over the allowed number of throws
    | otherwise = [] --we want to stop here

goal_tst :: Node -> Bool
goal_tst (N turn th ts _)
    | (length turn) == th && (sum turn) == ts = True
    | otherwise = False

--bt_dart_tsml :: Dartboard -> TargetScore -> Turns

--succ_tsml :: Node -> [Node]
--goal_tsml :: Node -> Bool

--sort :: Turn -> Turn


--searchPfsFst :: (Ord node) => (node -> [node]) -> (node -> Bool) -> node -> [node]


--psf_low :: Dartboard -> Targetscore -> Turns

--psf_high :: Dartboard -> Targetscore -> Turns

--seems a bit redundant?
--succ_low :: Node -> [Node]
--goal_high :: Node -> Bool
--succ_low :: Node -> [Node]
--goal_high :: Node -> Bool