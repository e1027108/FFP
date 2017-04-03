import Stack
import PQueue
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
data Node = Nil | N Dartboard Turn TargetScore Throws deriving (Show)

--Equality for type Node, made up of individual (==) calls on all the components of Node
instance Eq Node where
 N d1 t1 ts1 th1 == N d2 t2 ts2 th2 = 
  d1 == d2 && t1 == t2 && ts1 == ts2 && th1 == th2
 Nil == Nil = True
 Nil == _ = False
 
instance Ord Node where -- (<=) only is minimally acceptable implementation
    N d1 t1 ts1 th1 <= N d2 t2 ts2 th2
        | t1 == t2 = True
        | (minimum t1) < (minimum t2) = True
        | (minimum t1) == (minimum t2) && countMin t1 > countMin t2 = True
        | (minimum t1) == (minimum t2) && countMin t1 == countMin t2 = (N d1 (drop (countMin t1) t1) ts1 th1) <= (N d2 (drop (countMin t2) t2) ts2 th2)
        | otherwise = False
        where countMin x = (length . filter ( (minimum x) == )) x
    Nil <= _ = False
    _ <= Nil = False

-- **************
--EXERCISE PART 1
-- **************
 
--TODO ev. (Ord node) => (node -> ...) wegen angabe.
--gezeichnet: der Aufreger89
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

--computes successors filtering duplicates in range of given targetscore
succ_tsml :: Node -> [Node]
succ_tsml Nil = []
succ_tsml (N d t th ts) = [(N d (x:t) th ts)| x <- (filter (>= (maximum t)) d) , sum (x:t) <= ts]

--same as goal_ts
goal_tsml :: Node -> Bool
goal_tsml Nil = False
goal_tsml (N d turn th ts)
    | (sum turn) == ts = True
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

--returns list with minimum length entries
minlength :: Turns -> Turns
minlength [] = []
minlength turns = filter (\x -> length x <= minLen turns) turns 

minLen :: Turns -> Throws
minLen [] = 0
minLen as = minimum (map (\x -> length x) as)

-- ************************************************************
--functions to answer the proposed questions using backtracking
-- ************************************************************
--computes all Turns of specified target score
bt_dart_ts :: Dartboard -> TargetScore -> Turns
bt_dart_ts d ts = removeDuplicates (map sort (concat [map getTurn (searchDfs succ_ts goal_ts (N d [x] ts 0))| x <- (filter (<=ts) d)]))

--computes all Turns of specified target score in specified amount of throws
bt_dart_tst :: Dartboard -> TargetScore -> Throws -> Turns
bt_dart_tst d ts th = concat [map getTurn (searchDfs succ_tst goal_tst (N d [x] th ts)) | x <- (sort d) ]

bt_dart_tsml :: Dartboard -> TargetScore -> Turns
bt_dart_tsml d ts = minlength (concat [map getTurn (searchDfs succ_tsml goal_tsml (N d [x] 0 ts)) | x <- (filter (<= ts) d)])

-- **************
--EXERCISE PART 2
-- **************

--TODO ev. Node statt node, siehe searchDfs
searchPfsFst :: (Ord node) => (node -> [node]) -> (node -> Bool) -> node -> [node]
searchPfsFst succ goal x
  = take 1 (search' (enPQ x emptyPQ)) --take only 1, hopefully this is enough to solve the problem
    where
      search' q
        | pqEmpty q         = []
        | goal (frontPQ q) = frontPQ q : search' (dePQ q)
        | otherwise
            = let x = frontPQ q
              in search' (foldr enPQ (dePQ q) (succ x))

psf_low :: Dartboard -> TargetScore -> Turns
psf_low d ts = []
              
succ_low :: Node -> [Node]
succ_low (N d t ts th) = []

goal_low :: Node -> Bool
goal_low (N d t ts th) = False

psf_high :: Dartboard -> TargetScore -> Turns
psf_high d ts = []

succ_high :: Node -> [Node]
succ_high (N d t ts th) = []

goal_high :: Node -> Bool  
goal_high (N d t ts th) = False
