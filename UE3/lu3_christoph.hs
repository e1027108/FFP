module Stack (Stack,push,pop,top,emptyStack,stackEmpty) where 
push       :: a -> Stack a -> Stack a
pop        :: Stack a -> Stack a
top        :: Stack a -> a
emptyStack :: Stack a

data Stack a = EmptyStk | Stk a (Stack a)

push x s            = Stk x s
pop EmptyStk        = error "pop from empty stack"
pop (Stk _ s)       = s
top EmptyStk        = error "top from empty stack"
top (Stk x _)       = x
emptyStack          = EmptyStk
stackEmpty EmptyStk = True
stackEmpty _        = False


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

succ_ts :: Node -> [Node]
succ_ts Nil = [] 
succ_ts (N d t ts th) = [(N d (x:t) ts th)| x <- d , sum (x:t) <= ts]

goal_ts :: Node -> Bool
goal_ts Nil = False
goal_ts (N d t ts th)
 | sum t == ts = True
 | otherwise   = False

bt_dart_ts :: Dartboard -> TargetScore -> Turns
bt_dart_ts d ts = concat [map getTurn (searchDfs succ_ts goal_ts (N d [x] ts 0))| x <- (filter (<=ts) d)]

getTurn :: Node -> Turn
getTurn (N d t ts th) = t
