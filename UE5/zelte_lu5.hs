import Data.Array
import Data.List

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

--(TODO make it do something smart)
smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp l r c = simpleCamp l r c

--takes every row and puts it as a string of code chars into a list
outCamp :: Camp -> [[Char]]
outCamp arr = map (filter (/=' ')) [unwords [show (arr ! (x, y)) | x <- [1..8]] | y <- [1..8]]

--TODO combine all row combinations 

--TODO filter row variants for legal ones

-- generates all variants of a row (with correct amounts of tents, but not accounting for placement legality)
generateRowVariants :: Row -> TentsPerRow -> LocationsOfTrees -> [[Column]]
generateRowVariants r tents trees = getSubLists (tents!!(r-1)) (getOptions r trees)

-- gets all sublists of a given length from a source list
getSubLists :: Int -> [Column] -> [[Column]]
getSubLists t c = [ x | x <- (subsequences c), (length x) == t ]

--gets all positions on a row that are not containing a tree, produces duplicates that need to be filtered
getOptions :: Row -> LocationsOfTrees -> [Column]
getOptions r t = [ y | y <- [1..8], not (elem (r,y) t)]

-- for a row returns false, if the columns already has a tent or a tree or an adjacent position already has a tent
checkTentRowLegality :: Row -> Column -> TentsPerRow -> LocationsOfTrees -> LocationsOfTents -> Bool
checkTentRowLegality r c t ltr lte
    | (t !! (r-1)) <= (countRowOccurences r lte) = False --no more tents allowed
    | elem (r,c) lte || (elem (r,c-1) lte) || (elem (r,c+1) lte) || elem (r,c) ltr = False
    | otherwise = True

-- counts number of tents/trees in a row
countRowOccurences :: Row -> [(Int,Int)] -> Int
countRowOccurences a l = length [(x,y) | (x,y) <- l, x == a]

-- counts number of tents/trees in a column
countColumnOccurences :: Row -> [(Int,Int)] -> Int
countColumnOccurences a l = length [(x,y) | (x,y) <- l, y == a]

--checking if every tree is adjacent to at least one tent
treesHaveTents :: LocationsOfTrees -> LocationsOfTents -> Bool
treesHaveTents (x:xs) tents = (treeHasTent x tents) && (treesHaveTents xs tents)

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