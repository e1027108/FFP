module PQueue (PQueue,emptyPQ,pqEmpty,enPQ,dePQ,frontPQ) where
emptyPQ		:: PQueue a
pqEmpty		:: PQueue a -> Bool
enPQ		:: (Ord a) => a -> PQueue a -> PQueue a
dePQ		:: (Ord a) => PQueue a -> PQueue a
frontPQ		:: (Ord a) => PQueue a -> a

newtype PQueue a = PQ [a] deriving Show

emptyPQ             = PQ []

pqEmpty (PQ [])     = True
pqEmpty _	        = False

enPQ x (PQ q) 	    = PQ (insert x q)
  where insert x []                   = [x]
        insert x r@(e:r') | x <= e    = x:r
                          | otherwise = e:insert x r'
dePQ (PQ [])        = error "dePQ: empty priority queue"
dePQ (PQ (_:xs))    = PQ xs

frontPQ (PQ [])     = error "frontPQ: empty priority queue"
frontPQ (PQ (x:_))  = x
