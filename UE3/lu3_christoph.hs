import Stack
import Data.List

type Points      = Int      -- Punktwert einer Dart-Scheibe; echt positive Zahl
type Dartboard   = [Points] -- Dart-Scheibe charakterisiert durch Liste echt aufsteigender Punktwerte
type Turn        = [Points] -- Punktwerte einer Wurffolge; nur Punktwerte, die auf der Scheibe vorkommen
                            -- (d.h., im Dartboard-Wert vorkommen) sind moeglich, auch mehrfach moeglich
type Turns       = [Turn]   -- Strom von Wurffolgen
type TargetScore = Int      -- Gewuenschte Zielpunktsumme > 0
type Throws      = Int      -- Anzahl von Wuerfen einer Wurffolge > 0

--The type Node carries information on the Turn (including this nodes Points),
--the TargetScore and the right number of Throws. Also, a list of successors is given
data Node = Nil | N Dartboard Turn TargetScore Throws deriving Show

--Equality for type Node, made up of individual (==) calls on all the components of Node
instance Eq Node where
 N d1 t1 ts1 th1 == N d2 t2 ts2 th2 = 
  d1 == d2 && t1 == t2 && ts1 == ts2 && th1 == th2
 Nil == Nil = True
 Nil == _ = False

searchDfs :: (Node -> [Node]) -> (Node -> Bool) -> Node -> [Node]
searchDfs succ goal x 
 = (search' (push x emptyStack))
 where search' s
        | stackEmpty s = []
        | goal (top s) = top s : search' (pop s)
        | otherwise = let x = top s in search' (foldr push (pop s) (succ x))

-- ****************************************************************************
-- succ and goal functions to support the solutions of the 3 different problems
-- ****************************************************************************

--computes the successors while cutting off the values that would,
--in sum with the remaining turn, exceed the target score
succ_ts :: Node -> [Node]
succ_ts Nil = [] 
succ_ts (N d t ts th) = [(N d (x:t) ts th)| x <- d , sum (x:t) <= ts]

--checks if a node has a summed up value equal to the target score
goal_ts :: Node -> Bool
goal_ts Nil = False
goal_ts (N d t ts th)
 | sum t == ts = True
 | otherwise   = False

--computes all turns that are extensions to the input node 
--(while not exceeding the number of throws) and returns a list 
succ_tst :: Node -> [Node]
succ_tst Nil = []
succ_tst (N d turn th ts)
    | th > (length turn) = [ (N d x th ts) | x <- (generate turn d) ]
    | otherwise = [] --we want to stop here

--help function, adds another throw if possible
generate :: Turn -> Dartboard -> Turns
generate t d 
    | t /= [] = [ t ++ [x] | x <- (filter (>= (maximum t)) d) ]
    | otherwise = [ [x] | x <- d ]

--checks for the node whether throws are of specified amount and score is of specified sum
goal_tst :: Node -> Bool
goal_tst Nil = False
goal_tst (N d turn th ts)
    | (length turn) == th && (sum turn) == ts = True
    | otherwise = False

-- *************
--Help functions
-- *************

--removes duplicate entries from a list of turns
removeDuplicates :: Turns -> Turns
removeDuplicates [] = []
removeDuplicates (t1:turns)
 | elem t1 turns = removeDuplicates turns
 | otherwise     = t1 : removeDuplicates turns

--returns the turn of a node
getTurn :: Node -> Turn
getTurn (N d t ts th) = t

-- ************************************************************
--functions to answer the proposed questions using backtracking
-- ************************************************************
--computes all Turns of specified target score
bt_dart_ts :: Dartboard -> TargetScore -> Turns
bt_dart_ts d ts = removeDuplicates (map sort (concat [map getTurn (searchDfs succ_ts goal_ts (N d [x] ts 0))| x <- (filter (<=ts) d)]))

--computes all Turns of specified target score in specified amount of throws
bt_dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
bt_dart_tst d ts th = concat [map getTurn (searchDfs succ_tst goal_tst (N d [x] th ts)) | x <- (sort d) ]
