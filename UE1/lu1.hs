import Prelude hiding (sqrt,repeat,sequence,null)

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

type Avalue = Double -- 0 <= a <= 4
type InitialValue = Double -- 0 <= x0 <= 1
type SequenceValue = Double

type Interval = (Double,Double)
type InitialInterval = Interval

--Generator function according to the slides
repeat :: (a -> a) -> a -> [a]
repeat f x = (x:(repeat f (f x)))

--Selector functions - used to terminate the generator
within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:rest)
 | abs(a-b) <= eps = b
 | otherwise = within eps (b:rest)

relative :: (Ord a, Num a) => a -> [a] -> a
relative eps (a:b:rest)
 | abs(a-b) <= eps * abs(b) = b
 | otherwise = relative eps (b:rest)

--Selector functions for intervalnesting
within' :: (Ord a, Num a, Fractional a) => a -> [(a,a)] -> (a,a)
within' eps ((a,c):(b,d):rest)
 | abs(a-c)/2 <= eps = (b,d)
 | otherwise = within' eps ((b,d):rest)

relative' :: (Ord a, Num a, Fractional a) => a -> [(a,a)] -> (a,a)
relative' eps ((a,c):(b,d):rest)
 | abs(a-c)/2 <= eps = (b,d)
 | otherwise = relative' eps ((b,d):rest)

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

--helpfunction, halves a given value (for repeat, which needs 2 parameter)
halve :: (Fractional a) => a -> a
halve x = x/2

--numerical differentiation, very naive approximation strategy
easydiff :: Map -> XCoordinate -> InitialH -> Slope
easydiff f x h = (f (x+h) - f x)/h

--improved variation of easydiff, better approximation
differentiate :: Map -> XCoordinate -> InitialH -> [Slope]
differentiate f x h0 = map (easydiff f x) (repeat halve h0)

--calculates the next function value for the square root approximation
next :: Double -> Double -> Double
next n x = (x + n/x)/2

--calculates the next element in the sequence
next2 :: Avalue -> InitialValue -> SequenceValue
next2 a x0 = (a * x0 * (1-x0))

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

effintgrt :: Map -> Low -> High -> Epsilon -> Area
effintgrt f a b eps = within eps (integrateEfficient f a b)

effrelativeintgrt :: Map -> Low -> High -> Epsilon -> Area
effrelativeintgrt f a b eps = relative eps (integrateEfficient f a b)

--helpfunction for testing the integral/differential approximations
--diff x^2 = 2x; intgrt x^2 = x^3/3 (from 0 to 2 ~ 2.6667)
testF :: (Num a) => a -> a
testF x = x^2

--differentiation
diff :: Map -> XCoordinate -> InitialH -> Epsilon -> Slope
diff f x h0 eps = within eps (differentiate f x h0)

relativediff :: Map -> XCoordinate -> InitialH -> Epsilon -> Slope
relativediff f x h0 eps = relative eps (differentiate f x h0)

--sequence
sequence :: InitialValue -> Epsilon -> Avalue -> SequenceValue
sequence x0 eps a = within eps (repeat (next2 a) x0)

relativesequence :: InitialValue -> Epsilon -> Avalue -> SequenceValue
relativesequence x0 eps a = relative eps (repeat (next2 a) x0)

--nextinterval, finds the next smaller interval around a zero
--intentionally ignores the case of a nonexistent zero
--ONLY use functions that fullfill the zero citeria (f(a)f(b) < 0) for some interval!
nextinterval :: Map -> Interval -> Interval
nextinterval f i0
 | f x == 0 = i0
 | f x /= 0 && hasNull && f (fst i0) * f x < 0 = (fst i0, x)
 | f x /= 0 && hasNull && f (snd i0) * f x < 0 = (x, snd i0)
 where x = ((fst i0) + (snd i0))/2
       hasNull = f (fst i0) * f (snd i0) < 0

--intervalnesting, generator - this creates a stream of 
--continuously smaller intervals around a zero
intervalnesting :: Map -> InitialInterval -> [Interval]
intervalnesting f i0 = i0:(intervalnesting f (nextinterval f i0))

--null/relativenull, glueing intervalnesting (generator) to the selectors
--DOES NOT WORK with the type signature of within/relative (a -> [a] -> a).
-- Interval is (Double,Double) but Epsilon is only Double,
-- which values need to be compared to Epsilon? 
null :: Map -> InitialInterval -> Epsilon -> Interval
null f i0 eps = within' eps (intervalnesting f i0)

relativenull :: Map -> InitialInterval -> Epsilon -> Interval
relativenull f i0 eps = relative' eps (intervalnesting f i0)
