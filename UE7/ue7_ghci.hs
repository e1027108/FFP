import Data.Char
import Control.Monad

-- for ghci compatibilty------ 
import Control.Applicative
------------------------------

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
litParse = ((optional' (token '~')) >*> (neList (spot isDigit))) `build` (charlistToExpr . uncurry (++))

opExpParse :: Parse0 Char Expr
opExpParse = (token '(' >*> parser >*> spot isOp >*> parser >*> token ')') `build` makeExpr


-- missing helper functions

--isDigit :: Char -> Bool
--isDigit x = ('1' <= x && x <= '9')

isOp :: Char -> Bool
isOp x = x `elem` ['+', '-', '*', '/', '%']

list :: Parse0 a b -> Parse0 a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

-- optional p recognizes an object recognized by p or succeeds immediately.
optional' :: Parse0 a b -> Parse0 a [b]
optional' p = (succeed []) `alt` (p `build` (:[]))

-- neList p recognizes a non-empty list of the objects which are recognized by p.
neList :: Parse0 a b -> Parse0 a [b]
neList p = (p `build` (:[])) `alt` ((p >*> list p) `build` (uncurry (:)))

makeExpr :: (a,(Expr,(Char,(Expr,b)))) -> Expr
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

parserWSp :: Parse0 Char Expr
parserWSp = parser . filter (/=' ')

topLevelWSp = topLevel

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Monadic Parser
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

newtype Parser a = Parse (String -> [(a,String)])

instance Monad Parser where
    p >>= f
        = Parse (\cs -> concat [(parse (f a)) cs' | (a,cs') <- (parse p) cs])
    return a = Parse (\cs -> [(a,cs)])

-- mzero - always failing parser
-- mplus, the non-deterministically selecting parser
instance MonadPlus Parser where
    mzero = Parse (\cs -> [])
    p `mplus` q =  Parse (\cs -> parse p cs ++ parse q cs)

-- for ghci compatibilty ------------------------
instance Applicative Parser where
    pure a = Parse (\cs -> [(a,cs)])
    (<*>) = ap

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance Functor Parser where
    fmap = liftM
--------------------------------------------------

parse :: (Parser a) -> (String -> [(a,String)])
parse (Parse p) = p

-- parser recognizing single characters
item :: Parser Char
item = Parse (\cs -> case cs of
    "" -> []
    (c:cs) -> [(c,cs)])

-- the deterministically selecting parser
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parse (\cs -> case parse (p `mplus` q) cs of
                        [] -> []
                        (x:xs) -> [x])

-- recognizing single objects (compare to token)
char :: Char -> Parser Char
char c = sat (c ==)

-- recognizing single objects satisfying a particular property (compare to spot)
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else mzero}

-- parse a specific string
string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

-- zero or more applications of p
many' :: Parser a -> Parser [a]
many' p = many1' p +++ return []

-- one or more applications of p
many1' :: Parser a -> Parser [a]
many1' p = do a <- p; as <- many' p; return (a:as)

-- parsing of a string with blanks and line breaks
space :: Parser String
space = many' (sat isSpace)

-- parsing of a token by means of a parser p
token' :: Parser a -> Parser a
token' p = do {a <- p; space; return a}

-- parsing of a symbol token
symb :: String -> Parser String
symb cs = token' (string cs)

-- application of a parser p with removal of initial blanks
apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

-- combinator
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where
        rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 +++ return a

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainl1` mulop

factor :: Parser Int
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit :: Parser Int
digit = do {x <- token' (sat isDigit); return (ord x - ord '0')}

addop :: Parser (Int -> Int -> Int)
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
