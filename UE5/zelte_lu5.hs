import Data.Array

data Content = Tree | Tent | Empty deriving (Eq,Ord)

-- replaced deriving with an instance like it is used in outCamp
instance Show Content where
    show Tree = ['B']
    show Tent = ['Z']
    show Empty = ['u']

{-
Array (Int, Int) Content must be constucted as follows:
array ((1,1), (n,n)) [((1,1), Content), ((1,2), Content), ...]
first argument gives the array range, second the actual values
-}

type Camp = Array (Int,Int) Content
type Row = Int -- ausschliesslich Werte von 1 bis 8
type Column = Int -- ausschliesslich Werte von 1 bis 8
type LocationsOfTrees = [(Row,Column)]

--Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des i-ten
--Elements bezeichnet Zahl der Zelte in Reihe i:
type TentsPerRow = [Int]
-- Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des j-ten
--Elements bezeichnet Zahl der Zelte in Spalte j:
type TentsPerColumn = [Int]

-- helper types
type LocationsOfTents = [(Row, Column)]

--TODO implement
simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp l r c = (array ((1,1), (8,8)) [((1,1), Tree)]) --TODO check all generated row combinations

--TODO make it do something smart
smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp l r c = simpleCamp l r c

--takes every row and puts it as a string of code chars into a list
outCamp :: Camp -> [[Char]]
outCamp arr = map (filter (/=' ')) [unwords [show (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]

-- helper functions for simpleCamp
fillRowSimple :: Row -> TentsPerRow -> LocationsOfTrees -> LocationsOfTents
fillRowSimple r t l = [] --TODO generate all legal row variants for every row

-- for a row returns false, if the columns already has a tent or a tree or an adjacent position already has a tent
checkTentLegality :: Row -> Column -> TentsPerRow -> LocationsOfTrees -> LocationsOfTents -> Bool
checkTentLegality r c t ltr lte
    | (t !! (r-1)) <= (countRowOccurences r lte) = False --no more tents allowed
    | elem (r,c) lte || (elem (r,c-1) lte) || (elem (r,c+1) lte) || elem (r,c) ltr = False
    | otherwise = True

-- counts number of tents/trees in a row
countRowOccurences :: Row -> [(Int,Int)] -> Int
countRowOccurences a l = length [(x,y) | (x,y) <- l, x == a]

-- counts number of tents/trees in a column
countColumnOccurences :: Row -> [(Int,Int)] -> Int
countColumnOccurences a l = length [(x,y) | (x,y) <- l, y == a]

--TODO check every row combination (for tents touching, column amounts)
-- > TODO check tens touching

--TODO check if every tree is adjacent to at least one