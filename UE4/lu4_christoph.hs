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
pairingDigits :: Digits -> [Pairing]
pairingDigits d = map (\x -> leadingZeros x ((length d)-1)) (
                   map (\x -> showIntAtBase 2 intToDigit x "") [0..(2^((length d)-1))-1]
                  )

-- TODO insert operators

----- help functions -----
leadingZeros :: String -> Int -> String
leadingZeros s l
 | length s == l = s
 | otherwise     = leadingZeros ('0':s) l

mergeLists :: Digits -> Pairing -> [Integer]
mergeLists (d:ds) (p:ps) = d:(fromIntegral (digitToInt p)):(mergeLists ds ps)
mergeLists _ [] = []
--------------------------
