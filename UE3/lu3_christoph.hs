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

--bt_dart_ts :: Dartboard -> TargetScore -> Turns
--bt_dart_ts d ts

data Node = Nil | N Points [Node] deriving Show

instance Eq Node where
 N x xs == N y ys = x == y && xs == ys 
 Nil == Nil = True
 Nil == _ = False

--searchDfs :: (Eq Node) => (Node -> [Node]) -> (Node -> Bool) -> Node -> [Node]
--searchDfs succ goal x 
-- = (search' (push x emptyStack))
-- where search' s
--        | stackEmpty s = []
--        | goal (top s) = top s : search' (pop s)
--        | otherwise = let x = top s in search' (foldr push (pop s) (succ x))

succ_ts :: node -> [node]
-- succ Nil = error "successor on empty node"
succ_ts Nil = Nil 
succ_ts N x xs = xs

--goal :: node -> Bool
--goal Nil = False
--goal x = 
