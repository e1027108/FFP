infixr 5 >*>

data Expr = Lit Int | Var Char | Op Ops Expr Expr deriving (Eq, Ord, Show)
data Ops = Add | Sub | Mul | Div | Mod deriving (Eq, Ord, Show)

type Parse0 a b = [a] -> [(b,[a])]

-- Parser Functions

-- The always failing parser function
none :: Parse0 a b
none _ = []

-- The always succeeding parser function
succeed :: b -> Parse0 a b
succeed val inp = [(val,inp)]

-- The parser for recognizing single objects
token :: Eq a => a -> Parse0 a a
token t = spot (==t)

-- The parser for recognizing single objects satisfying some property
spot :: (a -> Bool) -> Parse0 a a
spot p [] = []
spot p (x:xs)
  | p x = [(x,xs)]
  | otherwise = []

-- Combinators

-- Alternatives
alt :: Parse0 a b -> Parse0 a b -> Parse0 a b
alt p1 p2 inp = p1 inp ++ p2 inp

-- Sequences
(>*>) :: Parse0 a b -> Parse0 a c -> Parse0 a (b,c)
(>*>) p1 p2 inp = [((y,z),rem2) | (y,rem1) <- p1 inp, (z,rem2) <- p2 rem1 ]

-- Transformations
build :: Parse0 a b -> (b -> c) -> Parse0 a c
build p f inp = [ (f x, rem) | (x,rem) <- p inp]

-- Parser

topLevel :: Parse0 a b -> [a] -> b
topLevel p inp = case results of 
    [] -> error "parse unsuccessful"
    _ -> head results
  where
  results = [ found | (found, []) <- p inp ]

parser :: Parse0 Char Expr
parser = nameParse `alt` litParse `alt` opExpParse

nameParse :: Parse0 Char Expr
nameParse = spot isName `build` Var

isName :: Char -> Bool
isName x = ('a' <= x && x <= 'z')

litParse :: Parse0 Char Expr
litParse = ((optional (token '~')) >*> (neList (spot isDigit))) `build` (charlistToExpr . uncurry (++))

opExpParse :: Parse0 Char Expr
opExpParse = (token '(' >*> parser >*> spot isOp >*> parser >*> token ')') `build` makeExpr


-- missing helper functions
isDigit :: Char -> Bool
isDigit x = ('1' <= x && x <= '9')

isOp :: Char -> Bool
isOp x = x `elem` ['+', '-', '*', '/', '%']

list :: Parse0 a b -> Parse0 a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

-- optional p recognizes an object recognized by p or succeeds immediately.
optional :: Parse0 a b -> Parse0 a [b]
optional p = (succeed []) `alt` (p `build` (:[]))

-- neList p recognizes a non-empty list of the objects which are recognized by p.
neList :: Parse0 a b -> Parse0 a [b]
neList p = (p `build` (:[])) `alt` ((p >*> list p) `build` (uncurry (:)))

makeExpr (_,(expr1,(op,(expr2,_)))) = Op (convertOperator op) expr1 expr2

-- charlistToExpr convert String into Int
 
charlistToExpr :: [Char] -> Expr
charlistToExpr = Lit . charlistToInt

charlistToInt :: [Char] -> Int
charlistToInt ('~':num) = - (read num :: Int)
charlistToInt num = (read num :: Int)

convertOperator :: Char -> Ops
convertOperator x 
  | (x == '+') = Add
  | (x == '-') = Sub
  | (x == '*') = Mul
  | (x == '/') = Div
  | (x == '%') = Mod

-- TestInput: topLevel parser "((234+~42)*b)"
-- TestOutput: Op Mul (Op Add (Lit 234) (Lit (-42))) (Var 'b')
