import Data.Array
import Data.List
import Debug.Trace

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
type EmptyLocations = [(Row,Column)]

--adds the locations of trees from input, computed locations of tents and filler positions to array
simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp trees tr tc = (array ((1,1), (8,8)) ((addContent trees Tree) ++ (addContent tents Tent) ++ (addContent filler Empty)))
                    where   tents = convertToCoordinates ((filterSolutions (combineAll trees tr) tc trees)!!0)
                            filler = fillMatrix tents trees

--(TODO make it do something smart)
smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp l r c = simpleCamp l r c

--takes every row and puts it as a string of code chars into a list
outCamp :: Camp -> [[Char]]
outCamp arr = map (filter (/=' ')) [unwords [show (arr ! (y, x)) | x <- [1..8]] | y <- [1..8]]

--helper functions naive
--adds content information to pair
addContent :: [(Row,Column)] -> Content -> [((Row,Column), Content)]
addContent ps c = [ (x,c) | x<-ps ]  

--gets all positions that have neither tree nor tent
fillMatrix :: LocationsOfTents -> LocationsOfTrees -> EmptyLocations
fillMatrix tents trees = [ (x,y) | x <- [1..8], y <- [1..8], not (elem (x,y) tents), not (elem (x,y) trees) ]

--transforms trees columns into matrix/array input
convertToCoordinates :: [[Column]] -> LocationsOfTents
convertToCoordinates cs = [ (x,y) | x <- [1..8], y <- [1..8], (elem y (cs!!(x-1))) ]

--filters out illegal combinations
filterSolutions :: [[[Column]]] -> TentsPerColumn -> LocationsOfTrees -> [[[Column]]]
filterSolutions tents tc trees = [ x | x <- tents, (columnsValid x tc), (rowsCompatible x), (treesHaveTents trees (convertToCoordinates x)) ]

--checks if tents touch from row to row
rowsCompatible :: [[Column]] -> Bool
rowsCompatible tents
    | (length tents) <= 1 = True
    | (length (tents!!0)) == 0 = rowsCompatible (drop 1 tents)
    | otherwise = ((length (intersect (tents!!0) (tents!!1))) == 0) && ((length (intersect (tents!!0) (map succ (tents!!1)))) == 0)
                    && ((length (intersect (tents!!0) (map pred (tents!!1)))) == 0) && rowsCompatible (drop 1 tents)

--checks if all columns have the right amount of tents in them
columnsValid :: [[Column]] -> TentsPerColumn -> Bool
columnsValid tents tc = not (elem False ([ (count x (concat tents)) == (tc!!(x-1)) | x <- [1..8] ] ))

-- counts occurrences of x
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

--generates tents for all rows, combines everything
combineAll :: LocationsOfTrees -> TentsPerRow -> [[[Column]]]
combineAll trees tr = [ [r1,r2,r3,r4,r5,r6,r7,r8] | r1 <- (generateVars 1 tr trees), r2 <- (generateVars 2 tr trees), r3 <- (generateVars 3 tr trees),
                            r4 <- (generateVars 4 tr trees), r5 <- (generateVars 5 tr trees), r6 <- (generateVars 6 tr trees),
                            r7 <- (generateVars 7 tr trees), r8 <- (generateVars 8 tr trees) ]

-- generates all legal variants of a row (ignoring other rows)
generateVars :: Row -> TentsPerRow -> LocationsOfTrees -> [[Column]]
generateVars r tents trees = [ x | x <- (getSubLists (tents!!(r-1)) (getOptions r trees)), not (tentsAdjacent x) ]

-- checks if any two tents are adjacent (input is ordered ascendingly)
tentsAdjacent :: [Column] -> Bool
tentsAdjacent tents
    | (length tents) <= 1 = False
    | (abs ((tents!!0)-(tents!!1))) == 1 = True
    | otherwise = tentsAdjacent (drop 1 tents)

-- gets all sublists of a given length from a source list
getSubLists :: Int -> [Column] -> [[Column]]
getSubLists t c = [ x | x <- (subsequences c), (length x) == t ]

--gets all positions on a row that are not containing a tree, produces duplicates that need to be filtered
getOptions :: Row -> LocationsOfTrees -> [Column]
getOptions r t = [ y | y <- [1..8], not (elem (r,y) t)]

--checking if every tree is adjacent to at least one tent
treesHaveTents :: LocationsOfTrees -> LocationsOfTents -> Bool
treesHaveTents (x:xs) tents
    | xs == [] = (treeHasTent x tents)
    | otherwise = (treeHasTent x tents) && (treesHaveTents xs tents)

--checks a certain tree if it has at least one adjacent tent
treeHasTent :: (Row,Column) -> LocationsOfTents -> Bool
treeHasTent (r,c) tents = (elem (r+1,c) tents) || (elem (r-1,c) tents) || (elem (r,c+1) tents) || (elem (r,c-1) tents)

--checking for every tent if it conflicts with tents on other rows (the single row is checked on building it)
checkAllTentsOtherRows :: LocationsOfTents -> Bool
checkAllTentsOtherRows (x:xs) = (checkTentOtherRows x xs) && (checkAllTentsOtherRows xs)

--check if a tent conflicts with other rows
checkTentOtherRows :: (Row,Column) -> LocationsOfTents -> Bool
checkTentOtherRows (r,c) tents = not( (elem (r+1,c-1) tents) || (elem (r+1,c) tents) || (elem (r+1,c+1) tents)
                                        || (elem (r-1,c+1) tents) || (elem (r-1,c) tents) || (elem (r-1,c-1) tents) )