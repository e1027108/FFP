import Numeric
import Data.Char

type TargetValue = Integer
type Digit       = Integer
type Digits      = [Digit]
type Number      = Integer
type Binary      = Integer
type Pairing     = String
type Pairings    = [Pairing]

digits = [1..9] :: Digits

data Operator = P | T deriving (Eq,Show) -- P fuer plus, T fuer times
data Expr     = Opd Number
                 | Opr Operator Expr Expr deriving (Eq,Show)

-- mkTV generates solutions for a given digits set and target value
mkTV :: Digits -> TargetValue -> [Expr]
mkTV ds tv = fastCreateExprs ds tv

-- old version, inefficient
oldMkTV :: Digits -> TargetValue -> [Expr]
oldMkTV ds tv = filter (\x -> (evalP (evalT (x))) == tv) (createExprs ds)

-- readable output
prettyMkTV :: Digits -> TargetValue -> [String]
prettyMkTV ds tv = map flatten (mkTV ds tv)

----- help functions -----
-- multiply the factors, ignore the rest
evalT :: Expr -> Expr
evalT (Opd x)                                 = Opd x
evalT (Opr P (ex1) (ex2))                     = Opr P (ex1) (evalT ex2)
evalT (Opr T (Opd x) (Opr P (Opd ex1) (ex2))) = Opr P (Opd (ex1*x)) (evalT ex2)
evalT (Opr T (Opd x) (Opr T (Opd ex1) (ex2))) = evalT (Opr T (Opd (ex1*x)) (ex2))
evalT (Opr T (Opd x) (Opd y))                 = Opd (x*y)

-- sum up all the values
evalP :: Expr -> Integer
evalP (Opd x)               = x
evalP (Opr P (Opd x) (ex2)) = x + evalP ex2

-- print a solution in the proposed notation from the exercise
flatten :: Expr -> String
flatten (Opd x) = show x
flatten (Opr T (ex1) (ex2)) = flatten ex1 ++ "*" ++ flatten ex2
flatten (Opr P (ex1) (ex2)) = flatten ex1 ++ "+" ++ flatten ex2

-- fastCreateExprs maps fastCreateExpr to numbers list
fastCreateExprs :: Digits -> TargetValue -> [Expr]
fastCreateExprs ds tv = concat (map (\x -> fastCreateExpr x (Opd 0) tv) (getNumbers ds))

-- createExprs maps createExpr to numbers list
createExprs :: Digits -> [Expr]
createExprs ds = concat (map (\x -> createExpr x (Opd 0)) (getNumbers ds))

-- createExpr builds a binary tree (P/T) from the number list
createExpr :: Digits -> Expr -> [Expr]
createExpr [] expr = [expr]
createExpr (d:ds) expr
 | expr == (Opd 0) = createExpr ds (Opd d)
 | otherwise       = createExpr ds exprP ++ createExpr ds exprT
 where exprP = (Opr P (Opd d) (expr))
       exprT = (Opr T (Opd d) (expr))

-- fastCreateExpr builds a binary tree BUT with backtracking resp. targetValue
fastCreateExpr :: Digits -> Expr -> TargetValue -> [Expr]
fastCreateExpr [] expr tv = if evalP (evalT expr) == tv then [expr] else []
fastCreateExpr (d:ds) expr tv
 | evalP (evalT expr) >= tv = []
 | expr == (Opd 0)          = fastCreateExpr ds (Opd d) tv
 | otherwise                = fastCreateExpr ds exprP tv ++ fastCreateExpr ds exprT tv
 where exprP = (Opr P (Opd d) (expr))
       exprT = (Opr T (Opd d) (expr))

-- getNumbers
getNumbers :: Digits -> [Digits]
getNumbers ds
 | length ds == 1 = [ds]
 | otherwise      = map (\x -> concatDigits 0 x ds) (map pairingToInt (pairingDigits ds))

-- concatDigits removes the spaces between digits if necessary
concatDigits :: Integer -> Digits -> Digits -> [Integer]
concatDigits o (p:ps) (a:b:bs)
 | p == 1 && o == 0 = concatDigits (a*10+b) ps bs
 | p == 1 && o /= 0 = concatDigits (o*10+a) ps (b:bs)
 | p == 0 && o == 0 = [a] ++ concatDigits 0 ps (b:bs)
 | p == 0 && o /= 0 = [o] ++ concatDigits 0 ps (a:b:bs)
concatDigits o (p:[]) (a:b:[])
 | p == 1 && o == 0 = [a*10+b]
 | p == 1 && o /= 0 = [o*10+a,b]
 | p == 0 && o == 0 = [a,b]
 | p == 0 && o /= 0 = [o,a,b]
concatDigits o (p:[]) (b:[])
 | p == 1 && o == 0 = [b]
 | p == 1 && o /= 0 = [o*10+b]
 | p == 0 && o == 0 = [b]
 | p == 0 && o /= 0 = [o,b]
concatDigits o [] (b:[]) = [b]
concatDigits o _ [] = [o]

-- pairingDigits utilizes a binary count up to 2^(length d)-1
pairingDigits :: Digits -> [Pairing]
pairingDigits d
 | length d == 1 = []
 | length d > 1  = map (\x -> leadingZeros x ((length d)-1)) (
                    map (\x -> showIntAtBase 2 intToDigit x "") [0..(2^((length d)-1))-1]
                   )

leadingZeros :: String -> Int -> String
leadingZeros s l
 | length s == l = s
 | otherwise     = leadingZeros ('0':s) l

-- pairingToInt converts the chars of String to Integer
pairingToInt :: Pairing -> [Integer]
pairingToInt (p:ps) = [(fromIntegral (digitToInt p))] ++ pairingToInt ps
pairingToInt [] = []
--------------------------
