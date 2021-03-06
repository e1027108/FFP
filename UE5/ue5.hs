import Data.List
import Data.Ord
import Data.Array
import Debug.Trace

-- compute naive and smart solution for the Maximum Segment Sum (MSS) Problem
-- testdata: simpleMSS [-4, -3, -7, 2, 1, -2, -1, -4]


-- naive solution
simpleMSS :: [Int] -> [Int]
simpleMSS = maximumBy (comparing sum) . segments

-- compute a list of contiguous subsequences (segments)
segments :: [Int] -> [[Int]]
segments = concatMap inits . tails
    where inits = takeWhile ((>1) . length) . takeWhile (not . null) . iterate init
          tails = takeWhile ((>1) . length) . takeWhile (not . null) . iterate tail

-- just for debugging
printSum :: [Int] -> [Int]
printSum = map sum . segments

-- smart solution
-- smartMSS :: [Int] -> [Int]

------------------------------------------

data Content = Tree | Tent | Empty deriving (Eq,Ord)

-- replaced deriving with an instance like it is used in outCamp
instance Show Content where
    show Tree = ['B']
    show Tent = ['Z']
    show Empty = ['u']

type Camp = Array (Int,Int) Content
type Row = Int -- ausschliesslich Werte von 1 bis 8
type Column = Int -- ausschliesslich Werte von 1 bis 8
type LocationsOfTrees = [(Row,Column)]

--Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des i-ten Elements bezeichnet Zahl der Zelte in Reihe i:
type TentsPerRow = [Int]
-- Liste der Laenge 8, ausschliesslich Werte von 0 bis 4; Wert des j-ten Elements bezeichnet Zahl der Zelte in Spalte j:
type TentsPerColumn = [Int]

-- helper types
type LocationsOfTents = [(Row, Column)]
type EmptyLocations = [(Row,Column)]

--generates all possible solutions, then check if something is correct
simpleCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
simpleCamp trees tr tc = (array ((1,1), (8,8)) (concat (head [ x | x <- generateAllBoardStates, (checkState x trees tr tc)])))

--adds the locations of trees from input, computed locations of tents and filler positions to array
smartCamp :: LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Camp
smartCamp trees tr tc = (array ((1,1), (8,8)) ((addContent trees Tree) ++ (addContent tents Tent) ++ (addContent filler Empty)))
                    where   tents = convertToCoordinates ((filterSolutions (combineAll trees tr) tc trees)!!0)
                            filler = fillMatrix tents trees

--takes every row and puts it as a string of code chars into a list
outCamp :: Camp -> [[Char]]
outCamp arr = map (filter (/=' ')) [unwords [show (arr ! (y, x)) | x <- [1..8]] | y <- [1..8]]



----------------------------------------helper functions simple----------------------------------------
--combines all possible states for rows into all possible states for puzzle solution boards
generateAllBoardStates :: [[[((Row, Column), Content)]]]
generateAllBoardStates = [ [a,b,c,d,e,f,g,h] | a <- (rs 1), b <- (rs 2), c <- (rs 3), d <- (rs 4), e <- (rs 5), f <- (rs 6), g <- (rs 7), h <- (rs 8) ]
    where rs r = generateAllRowStates r

--combines all possible point states into all possible row states of a given row
generateAllRowStates :: Row -> [[((Row, Column), Content)]]
generateAllRowStates r = [ [((r,1),a),((r,2),b),((r,3),c),((r,4),d),((r,5),e),((r,6),f),((r,7),g),((r,8),h)] | a <- cs, b <- cs, c <- cs,
    d <- cs, e <- cs, f <- cs, g <- cs, h <- cs ] 
        where cs = [Empty, Tree, Tent]

--takes a state and checks whether the trees are in the right positions, border a tent, tents dont touch each other
--and there are the right amounts of tents
checkState :: [[((Row, Column), Content)]] -> LocationsOfTrees -> TentsPerRow -> TentsPerColumn -> Bool
checkState state trees tr tc = (checkTreePositions trl trees) && (treesHaveTents trl tel) && (noTentsAdjacent tel)
                                && (checkTentOccurrences tel tr tc)
                                    where   trl = extractLocations state Tree
                                            tel = extractLocations state Tent

--takes all positions for a given content type for a state
extractLocations :: [[((Row, Column), Content)]] -> Content -> [(Row, Column)]
extractLocations locs content = [ (fst y) | x <- locs, y <- x, (snd y) == content ]

--checks whether all trees in the state are actual trees that should be there and that the amounts of compared trees are equal
checkTreePositions :: [(Row, Column)] -> LocationsOfTrees -> Bool
checkTreePositions state trees = not (elem False [ (elem x trees) | x <- state ] ) && (length state) == (length trees)

--checks for all tents if there is any adjacency
noTentsAdjacent :: LocationsOfTents -> Bool
noTentsAdjacent tents = (length [ (x,y) | (x,y) <- tents, (occurs (x,y)) ]) == 0
    where occurs (x,y) = (elem (x+1,y) tents) || (elem (x-1,y) tents) || (elem (x,y-1) tents) || (elem (x,y+1) tents ) || (elem (x+1,y+1) tents) ||
                            (elem (x-1,y-1) tents) || (elem (x-1,y+1) tents) || (elem (x+1,y-1) tents)

--checking if every tree is adjacent to at least one tent
treesHaveTents :: LocationsOfTrees -> LocationsOfTents -> Bool
treesHaveTents (x:xs) tents
    | xs == [] = (treeHasTent x tents)
    | otherwise = (treeHasTent x tents) && (treesHaveTents xs tents)

--checks a certain tree if it has at least one adjacent tent
treeHasTent :: (Row,Column) -> LocationsOfTents -> Bool
treeHasTent (r,c) tents = (elem (r+1,c) tents) || (elem (r-1,c) tents) || (elem (r,c+1) tents) || (elem (r,c-1) tents)

--checks if all rows/columns have the right occurences
checkTentOccurrences :: LocationsOfTents -> TentsPerRow -> TentsPerColumn -> Bool
checkTentOccurrences tents tr tc = (not (elem False [ (countRowOccurences x tents) == (tr!!(x-1)) | x <- [1..8] ]))
                        && (not (elem False [ (countColumnOccurences y tents) == (tc!!(y-1)) | y <- [1..8] ]))

-- counts number of tents in a row
countRowOccurences :: Row -> LocationsOfTents -> Int
countRowOccurences a l = length [(x,y) | (x,y) <- l, x == a]

-- counts number of tents in a column
countColumnOccurences :: Row -> LocationsOfTents -> Int
countColumnOccurences a l = length [(x,y) | (x,y) <- l, y == a]



----------------------------------------helper functions smart----------------------------------------
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
