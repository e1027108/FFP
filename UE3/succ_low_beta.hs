import Data.List

--succ_low/high gibt den nächsten zulässigen node im sinne einer minimalen/maximalen lösung an

--findet ein set (inklusive minimum wert k_1 aus dartboard d), das alle elemente beinhaltet, die in einer gültigen lösung vorkommen können
--detail: (targetScore % k_1); wenn 0, dann ist TS nur mit k_1 erreichbar, sonst nicht -> hilfsfunktion
findComp_low :: (Integral a) => a -> [a] -> [a]
findComp_low ts (a:as)
 | mod ts a == 0 = [a]
 | mod ts a /= 0 && findComp_low' (sieve as) a (mod ts a) /= [] = a:(findComp_low' (sieve as) a (mod ts a))
 | otherwise = findComp_low ts (sieve as)

--sucht nach elementen k_n des dartboards, für die (k_n % k_1 = TS % k_1) gilt. diese können dann den benötigten rest einbringen
findComp_low' :: (Integral a) => [a] -> a -> a -> [a]
findComp_low' as a rem = [ x | x <- as, mod x a == rem]

--entfernt alle werte aus dem dartboard, die ein vielfaches eines anderen elements darstellen (k_n ^m).
sieve :: (Integral a) => [a] -> [a]
sieve as = sieveNum (minimum as) as

--hilfsfunktion für sieve, geht die liste schritt für schritt durch
sieveNum :: (Integral a) => a -> [a] -> [a]
sieveNum a (b:as) = let rest = [ x | x <- as, mod x a /= 0] in a:(sieveNum (head rest) rest)
sieveNum _ []     = []
