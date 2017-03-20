--Generator function - takes a function with 1 parameter,
--creates a new function that takes the second parameter (normally masked by currying?)
--so we can add the start value to the list.

--TODO: is this start value really necessary? for instance: sqrt 1 1 5 = [1,3,2.33,...].
-- this influences the outcome.
repeat' :: (a -> a) -> a -> [a]
repeat' f x = (x:(repeat' f (f x)))

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
easyintegrate :: (Double -> Double) -> Double -> Double -> Double
easyintegrate f a b = (f a + f b) * (b-a)/2

--improved variation of easyintegrate, better approximation
integrateNaive :: (Double -> Double) -> Double -> Double -> [Double]
integrateNaive f a b = ((easyintegrate f a b):
                           (map addpair (zip (integrateNaive f a mid) 
                             (integrateNaive f mid b)))) 
                        where mid = (a+b)/2

--optimized version of integrateNaive, better performance
--example: testF 0 2 0.000000000001; 
--integrateNaive runs ~32s, integrateEfficient ~21s (on a laptop)
integrateEfficient :: (Double -> Double) -> Double -> Double -> [Double]
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
differentiate :: Double -> (Double -> Double) -> Double -> [Double]
differentiate h0 f x = map (easydiff f x) (repeat' halve h0)

--calculates the next function value for the square root approximation
next :: Double -> Double -> Double
next n x = (x + n/x)/2

--by bringing together the generator and selector functions,
--we can get an actual result. repeatedly, the selector checks for the termination
--criteria and lets the generator calculate another value if needed.
sqrt' :: Double -> Double -> Double -> Double
sqrt' a0 eps n = within eps (repeat' (next n) a0)

--(next n) a0 == next n a0, a -> a -> a == a -> (a -> a)
relativesqrt :: Double -> Double -> Double -> Double
relativesqrt a0 eps n = relative eps (repeat' (next n) a0)

--integration
intgrt :: (Double -> Double) -> Double -> Double -> Double -> Double
intgrt f a b eps = within eps (integrateNaive f a b)

relativeintgrt :: (Double -> Double) -> Double -> Double -> Double -> Double
relativeintgrt f a b eps = relative eps (integrateNaive f a b)

intgrtEff :: (Double -> Double) -> Double -> Double -> Double -> Double
intgrtEff f a b eps = within eps (integrateEfficient f a b)

relativeintgrtEff :: (Double -> Double) -> Double -> Double -> Double -> Double
relativeintgrtEff f a b eps = relative eps (integrateEfficient f a b)

--helpfunction for testing the integral/differential approximations
--diff x^2 = 2x; intgrt x^2 = x^3/3 (from 0 to 2 ~ 2.6667)
testF :: (Num a) => a -> a
testF x = x^2

--differentiation
diff :: Double -> (Double -> Double) -> Double -> Double -> Double
diff h0 f x eps = within eps (differentiate h0 f x)

relativediff :: Double -> (Double -> Double) -> Double -> Double -> Double
relativediff h0 f x eps = relative eps (differentiate h0 f x)