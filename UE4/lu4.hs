type TargetValue = Integer
type Digit = Integer -- ausschliesslich Werte 1,2,..,9
type Digits = [Digit]
type Number = Integer

digits = [1..9] :: Digits

data Operator = P | T deriving (Eq,Show) -- P fuer plus, T fuer times
data Expr = Opd Number
    | Opr Operator Expr Expr deriving (Eq,Show)
    
mkTV :: Digits -> TargetValue -> [Expr]
mkTV d t = [Opd 0]