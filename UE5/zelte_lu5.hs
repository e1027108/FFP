import Data.Array

data Content = Tree | Tent | Empty deriving (Eq,Ord)

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

--TODO implement
simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp l r c = (array ((1,1), (8,8)) [((1,1), Tree)])

--TODO make it do something smart
smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp l r c = simpleCamp l r c

--takes every row and puts it as a string of code chars into a list
outCamp :: Camp -> [[Char]]
outCamp arr = map (filter (/=' ')) [unwords [show (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]

instance Show Content where
    show Tree = ['B']
    show Tent = ['Z']
    show Empty = ['u']