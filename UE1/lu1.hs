import Prelude hiding (sqrt,repeat)

type InitialApprox = Double -- Auschliesslich Werte > 0
type Epsilon = Double -- Auschliesslich Werte > 0
type SquareArg = Double -- Auschliesslich Werte > 0
type Approx = Double -- Auschliesslich Werte > 0
type Map = Double -> Double
type Low = Double -- Untere Intervallgrenze
type High = Double -- Obere Intervallgrenze
type Area = Double
type XCoordinate = Double
type InitialH = Double -- Auschliesslich Werte > 0
type Slope = Double

--Generator function - takes a function with 1 parameter,
--creates a new function that takes the second parameter (normally masked by currying?)
--so we can add the start value to the list.

-- this influences the outcome.
repeat :: (a -> a) -> a -> [a]
repeat f x = (x:(repeat f (f x)))

--Selector functions - used to terminate the generator.
within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:rest)
 | abs(a-b) <= eps = b
 | otherwise = within eps (b:rest)

relative :: (Ord a, Num a) => a -> [a] -> a
relative eps (a:b:rest)
 | abs(a-b) <= eps * abs(b) = b
 | otherwise = relative eps (b:rest)

--approximation strategy for an integral, very naive
easyintegrate :: Map -> Low -> High -> Area
easyintegrate f a b = (f a + f b) * (b-a)/2

--improved variation of easyintegrate, better approximation
integrateNaive :: Map -> Low -> High -> [Area]
integrateNaive f a b = ((easyintegrate f a b):
                           (map addpair (zip (integrateNaive f a mid) 
                             (integrateNaive f mid b)))) 
                        where mid = (a+b)/2

--optimized version of integrateNaive, better performance
--example: testF 0 2 0.000000000001; 
--integrateNaive runs ~32s, integrateEfficient ~21s (on a laptop)
integrateEfficient :: Map -> Low -> High -> [Area]
integrateEfficient f a b = integ f a b (f a) (f b)
integ f a b fa fb = ((fa+fb)*(b-a)/2):
                        (map addpair (zip (integ f a m fa fm)
                                          (integ f m b fm fb)))
                        where m = (a+b)/2
                              fm = f m

--helpfunction, adds 2-tupel values
addpair :: (Num a) => (a,a) -> a
addpair (a,b) = a+b

--helpfunction, halves a given value
halve :: (Fractional a) => a -> a
halve x = x/2

--numerical differentiation, very naive approximation strategy
easydiff :: (Double -> Double) -> Double -> Double -> Double
easydiff f x h = (f (x+h) - f x)/h

--improved variation of easydiff, better approximation
differentiate :: Map -> XCoordinate -> InitialH -> [Slope]
differentiate f x h0 = map (easydiff f x) (repeat halve h0)

--calculates the next function value for the square root approximation
next :: Double -> Double -> Double
next n x = (x + n/x)/2

--by bringing together the generator and selector functions,
--we can get an actual result. repeatedly, the selector checks for the termination
--criteria and lets the generator calculate another value if needed.
sqrt :: InitialApprox -> Epsilon -> SquareArg -> Approx
sqrt a0 eps n = within eps (repeat (next n) a0)

--(next n) a0 == next n a0, a -> a -> a == a -> (a -> a)
relativesqrt :: InitialApprox -> Epsilon -> SquareArg -> Approx
relativesqrt a0 eps n = relative eps (repeat (next n) a0)

--integration
intgrt :: Map -> Low -> High -> Epsilon -> Area
intgrt f a b eps = within eps (integrateNaive f a b)

relativeintgrt :: Map -> Low -> High -> Epsilon -> Area
relativeintgrt f a b eps = relative eps (integrateNaive f a b)

intgrtEff :: Map -> Low -> High -> Epsilon -> Area
intgrtEff f a b eps = within eps (integrateEfficient f a b)

relativeintgrtEff :: Map -> Low -> High -> Epsilon -> Area
relativeintgrtEff f a b eps = relative eps (integrateEfficient f a b)

--helpfunction for testing the integral/differential approximations
--diff x^2 = 2x; intgrt x^2 = x^3/3 (from 0 to 2 ~ 2.6667)
testF :: (Num a) => a -> a
testF x = x^2

--differentiation
diff :: Map -> XCoordinate -> InitialH -> Epsilon -> Slope
diff f x h0 eps = within eps (differentiate f x h0)

relativediff :: Map -> XCoordinate -> InitialH -> Epsilon -> Slope
relativediff f x h0 eps = relative eps (differentiate f x h0)