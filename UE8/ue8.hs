(=:=) :: Bunch m => Term -> Term -> Pred m
(t=:=u)(MkAnswer(s,n)) =
 case unify(tu) s of
   Just s' -> return(MkAnswer(s',n))
   Nothing -> zero

(&&&) :: Bunch m => Pred m -> Pred m -> Pred m
(p &&& q) s = p s >>= q

(|||) :: Bunch m => Pred m -> Pred m -> Pred m
(p ||| q) s = alt (p s) (q s)

infixr 4 =:=
infixr 3 &&&
infixr 2 |||

newtype Subst = MkSubst [(Var, Term)]
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


type Pred m = Answer -> m Answer


newtype Answer = MkAnswer (Subst, Int) deriving Show
