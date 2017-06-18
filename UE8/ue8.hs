import Control.Monad
import Control.Applicative

type Stream a = [a]

class Monad m => Bunch m where
 zero :: m a
 alt  :: m a -> m a -> m a
 wrap :: m a -> m a

instance Bunch [] where
 zero      = []
 alt xs ys = xs ++ ys
 wrap xs   = xs

newtype Diag a = MkDiag (Stream a) deriving Show

unDiag :: Diag a -> Stream a
unDiag (MkDiag xs) = xs

diag :: Stream (Stream a) -> Stream [a]
diag [] = []
diag (xs:xss) = lzw (++) [ [x] | x <- xs] ([] : diag xss)

instance Monad Diag where
 return x = MkDiag [x]
 MkDiag xs >>= f = MkDiag (concat (diag (map (unDiag . f) xs)))

-- for ghci compatibility --
instance Applicative Diag where
 pure  = return
 (<*>) = ap

instance Functor Diag where
 fmap = liftM
----------------------------

instance Bunch Diag where
 zero    = MkDiag []
 alt (MkDiag xs) (MkDiag ys)
         = MkDiag (shuffle xs ys)
 wrap xm = xm

shuffle :: [a] -> [a] -> [a]
shuffle [] ys = ys
shuffle (x:xs) ys = x : shuffle ys xs

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

-- kann man [(Variable,Term)] statt [(Var,Term)] sagen?
newtype Subst = MkSubst [(Variable, Term)] deriving Show
unSubst(MkSubst s) = s

idsubst = MkSubst[]
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

-- supporting function lzw (like zip with - except non-empty part of a list gets attached) --
lzw :: (a -> a -> a) -> Stream a -> Stream a -> Stream a
lzw f [] ys         = ys
lzw f xs []         = xs
lzw f (x:xs) (y:ys) = (f x y):(lzw f xs ys)

-- supporting function test --
test :: Bunch m => Bool -> m ()
test b = if b then return () else zero
