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

--mkTV :: Digits -> TargetValue -> [Expr]

-- TODO from merged pairing list & inserting operators -> expr ?

-- TODO change this later to the more efficient variation (equational reasoning)
-- naive procedure, generates all the possible concatenations of ordered digits.
-- TODO insert operators

-- getNumbers
getNumbers :: Digits -> [Digits]
getNumbers ds = map (\x -> concatDigits 0 x ds) (map pairingToInt (pairingDigits ds))

----- help functions -----
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
pairingDigits d = map (\x -> leadingZeros x ((length d)-1)) (
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

-- mergeLists converts the string to integer and merges the two lists in an alternating fashion
mergeLists :: Digits -> Pairing -> [Integer]
mergeLists (d:ds) (p:ps) = d:(fromIntegral (digitToInt p)):(mergeLists ds ps)
mergeLists _ [] = []
--------------------------
