import Control.Monad
import Control.Applicative

type Stream a = [a]

-- for ghci compatibility --
instance Applicative Diag where
 pure  = return
 (<*>) = ap

instance Functor Diag where
 fmap = liftM

instance Applicative Matrix where
 pure  = return
 (<*>) = ap

instance Functor Matrix where
 fmap = liftM
----------------------------

class Monad m => Bunch m where
 zero :: m a
 alt  :: m a -> m a -> m a
 wrap :: m a -> m a

-- depth search strategy --
instance Bunch [] where
 zero      = []
 alt xs ys = xs ++ ys
 wrap xs   = xs

-- diagonalization stragety --
-- TODO: not associative, group generators together pairwise (slides pp1084)
newtype Diag a = MkDiag (Stream a) deriving Show

unDiag :: Diag a -> Stream a
unDiag (MkDiag xs) = xs

diag :: Stream (Stream a) -> Stream [a]
diag [] = []
diag (xs:xss) = lzw (++) [ [x] | x <- xs] ([] : diag xss)

instance Monad Diag where
 return x = MkDiag [x]
 MkDiag xs >>= f = MkDiag (concat (diag (map (unDiag . f) xs)))

instance Bunch Diag where
 zero    = MkDiag []
 alt (MkDiag xs) (MkDiag ys)
         = MkDiag (shuffle xs ys)
 wrap xm = xm

shuffle :: [a] -> [a] -> [a]
shuffle [] ys = ys
shuffle (x:xs) ys = x : shuffle ys xs

-- breadth search strategy --
newtype Matrix a = MkMatrix (Stream [a]) deriving Show

unMatrix :: Matrix a -> Stream [a]
unMatrix (MkMatrix xm) = xm

instance Monad Matrix where
 return x = MkMatrix [[x]]
 MkMatrix xm >>= f = MkMatrix (bindm xm (unMatrix . f))

instance Bunch Matrix where
 zero     = MkMatrix []
 alt (MkMatrix xm) (MkMatrix ym)
          = MkMatrix (lzw (++) xm ym)
 wrap (MkMatrix xm)
          = MkMatrix ([]:xm)
------------------------------

data Term = Int Int | Nil | Cons Term Term | Var Variable deriving (Show, Eq)
data Variable = Named String | Generated Int deriving (Show, Eq)

var :: String -> Term
var s = Var (Named s)

list :: [Int] -> Term
list xs = foldr Cons Nil (map Int xs)

(=:=) :: Bunch m => Term -> Term -> Pred m
(t=:=u)(MkAnswer(s,n)) =
 case unify(t,u) s of
   Just s' -> return(MkAnswer(s',n))
   Nothing -> zero

(&&&) :: Bunch m => Pred m -> Pred m -> Pred m
(p &&& q) s = p s >>= q

(|||) :: Bunch m => Pred m -> Pred m -> Pred m
(p ||| q) s = alt (p s) (q s)

infixr 4 =:=
infixr 3 &&&
infixr 2 |||

-- TODO: Var -> data constructor is in scope, did you mean DataKinds?
--       ghci -XDataKinds -> non-promotable type "Term" fml..
newtype Subst = MkSubst [(Var,Term)] deriving Show

unSubst :: Subst -> [(Var,Term)]
unSubst(MkSubst s) = s

idsubst :: Subst
idsubst = MkSubst[]

extend :: Var -> Term -> Subst -> Subst
extend x t (MkSubst s) = MkSubst ((x,t):s)

apply :: Subst -> Term -> Term
apply s t =
    case deref s t of
        Cons x xs -> Cons (apply s x) (apply s xs)
        t'        -> t'

deref :: Subst -> Term -> Term
deref s (Var v) =
     case lookup v (unSubst s) of
       Just t    -> deref s t
       Nothing   -> Var v
deref s t = t

unify :: (Term, Term) -> Subst -> Maybe Subst
unify (t,u) s =
 case (deref s t, deref s u) of
   (Nil, Nil) -> Just s
   (Cons x xs, Cons y ys)  -> unify (x,y) s >>= unify (xs, ys)
   (Int n, Int m) | (n==m) -> Just s
   (Var x, Var y) | (x==y) -> Just s
   (Var x, t)              -> if occurs x t s then Nothing
                                   else Just (extend x t s)
   (t, Var x)              -> if occurs x t s then Nothing
                                   else Just (extend x t s)
   (_,_)                   -> Nothing

occurs :: Variable -> Term -> Subst -> Bool
occurs x t s =
 case deref s t of
   Var y     -> x == y
   Cons y ys -> occurs x y s || occurs x ys s
   _         -> False


append :: Bunch m => (Term, Term, Term) -> Pred m
append(p,q,r) =
 step(p =:= Nil &&& q =:= r
      ||| exists (\x -> exists (\a -> exists (\b ->
            p =:= Cons x a &&& r =:= Cons x b
            &&& append(a,q,b)))))

step :: Bunch m => Pred m -> Pred m
step p s = wrap (p s)

exists :: Bunch m => (Term -> Pred m) -> Pred m
exists p (MkAnswer (s,n)) =
  p (Var (Generated n)) (MkAnswer (s,n+1))

type Pred m = Answer -> m Answer

newtype Answer = MkAnswer (Subst, Int) deriving Show

-- supporting functions for modelling --
initial :: Answer
initial = MkAnswer (idsubst,0)

run :: Bunch m => Pred m -> m Answer
run p = p initial

-- supporting function lzw (like zip with - except non-empty part of a list gets attached) --
lzw :: (a -> a -> a) -> Stream a -> Stream a -> Stream a
lzw f [] ys         = ys
lzw f xs []         = xs
lzw f (x:xs) (y:ys) = (f x y):(lzw f xs ys)

-- supporting function test --
test :: Bunch m => Bool -> m ()
test b = if b then return () else zero

-- support function bindm for Matrix --
bindm :: Stream [a] -> (a -> Stream [b]) -> Stream [b]
bindm xm f = map concat (diag (map (concatAll . map f) xm))

concatAll :: [Stream [b]] -> Stream [b]
concatAll = foldr (lzw (++)) []

-- testing method "factor" --
-- usage e.g.: factor 24 :: [Stream,Matrix,Diag] (Int,Int)
factor :: Bunch m => Int -> m (Int,Int)
factor n = do r <- choose [1..]; s <- choose [1..];
              test (r*s==n); return (r,s)

choose :: Bunch m => Stream a -> m a
choose (x:xs) = wrap (return x `alt` choose xs)
