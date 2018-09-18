import System.IO
import Data.Tuple
import Data.List
import Data.Maybe

type Board = ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [(Int, Bone)])
type Coordinate = (Int, Int) -- (x, y)
type Bone = (Int, Int) -- (leftPip, rightPip)

--getInput :: ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int])
--getInput = ([5, 0, 3, 5, 4, 5, 5],
--            [4, 6, 2, 3, 0, 2, 5],
--            [3, 0, 6, 6, 4, 2, 3],
--            [6, 1, 5, 2, 1, 4, 6],
--            [5, 2, 0, 3, 0, 4, 1],
--            [3, 3, 4, 2, 0, 1, 2],
--            [4, 1, 2, 0, 4, 6, 3],
--            [6, 1, 0, 6, 1, 5, 1])

getInput :: Board -- Works
getInput = ([5, 4, 3, 6, 5, 3, 4, 6],
            [0, 6, 0, 1, 2, 3, 1, 1],
            [3, 2, 6, 5, 0, 4, 2, 0],
            [5, 3, 6, 2, 3, 2, 0, 6],
            [4, 0, 4, 1, 0, 0, 4, 1],
            [5, 2, 2, 4, 4, 1, 6, 5],
            [5, 5, 3, 6, 1, 2, 3, 1],
            getBones [] 0)

mainF :: Board -> ()
mainF board = getSolution [(board, getEmptyBoard)]

getSolution :: [(Board, Board)] -> ()
getSolution boards = if (length updatedBoards > 0) then getSolution updatedBoards else ()
                     where updatedBoards = updateBoardList boards

updateBoardList :: [(Board, Board)] -> [(Board, Board)]
updateBoardList [] = []
updateBoardList boards = updateBoard (fst (head updatedBoards)) (snd (head updatedBoards)) ++ updateBoardList (tail updatedBoards)
                         where updatedBoards = removeBoards boards

removeBoards :: [(Board, Board)] -> [(Board, Board)]
removeBoards [] = []
removeBoards boards = if isSolved (head boards) then handleRemovedBoards (head boards) else boards ++ removeBoards boards 

handleRemovedBoards :: (Board, Board) -> [(Board, Board)]
handleRemovedBoards boards = do if isSolved boards then boardToString (snd boards) else []
                                []

isSolved :: (Board, Board) -> Bool
isSolved boards = if boardIsEmpty (fst boards) then True else False

printSolution :: (Board, Board) -> IO ()
printSolution boards = putStr("Solution found: \n" ++ boardToString (snd boards))

updateBoard :: Board -> Board -> [(Board, Board)] -- (boardToSolve, boardToFill)
updateBoard boardToSolve boardToFill = if occurrences == 0 then [] else solve boardToSolve boardToFill coors boneNumber
                                       where boneWithLowestOccurrence = getBoneWithLowestOccurrence boardToSolve
                                             occurrences = fst boneWithLowestOccurrence
                                             bone = snd (snd boneWithLowestOccurrence)
                                             boneNumber = fst (snd boneWithLowestOccurrence)
                                             coors = findBoneOnBoard bone boardToSolve

solve :: Board -> Board -> [(Coordinate, Coordinate)] -> Int -> [(Board, Board)] -- Input: boardToSolve, boardToFill, coordinates and boneNumber
solve _ _ [] _ = []
solve boardToSolve boardToFill coors boneNumber = [(emptyCellInBoardUsingCoordinates boardToSolve (head coors), updateCellInBoardUsingCoordinates boardToFill boneNumber (head coors))] ++ solve boardToSolve boardToFill (tail coors) boneNumber

getBoneWithLowestOccurrence :: Board -> (Int, (Int, Bone)) -- (frequency, (boneNumber, Bone))
getBoneWithLowestOccurrence board = (!!) boneOccurrences (fst (minimumWithIndex (occurrencesToList (boneOccurrences))))
                                          where boneOccurrences = calculateBoneOccurrences board

minimumWithIndex :: [Int] -> (Int, Int) -- (index, value)
minimumWithIndex xs = (fromJust (elemIndex min xs), min)
                       where min = minimum xs 

occurrencesToList :: [(Int, (Int, Bone))] -> [Int]
occurrencesToList [] = []
occurrencesToList xs = [fst (head xs)] ++ occurrencesToList (tail xs)  

calculateBoneOccurrences :: Board -> [(Int, (Int, Bone))] -- works -- [(frequency, (boneNumber, Bone))]
calculateBoneOccurrences (a, b, c, d, e, f, g, []) = []
calculateBoneOccurrences (a, b, c, d, e, f, g, xs) = [(calculateBoneOccurrence (a, b, c, d, e, f, g, xs) (snd (head xs)) , (head xs))] ++ calculateBoneOccurrences (a, b, c, d, e, f, g, tail xs)

calculateBoneOccurrence :: Board -> Bone -> Int -- works
calculateBoneOccurrence board bone = length (findBoneOnBoard bone board)


findBoneOnBoard :: Bone -> Board -> [(Coordinate, Coordinate)] -- works
findBoneOnBoard bone board = findBoneOnBoardHorizontal bone board ++ findBoneOnBoardVertical bone board

findBoneOnBoardHorizontal :: Bone -> Board -> [(Coordinate, Coordinate)]
findBoneOnBoardHorizontal bone board = convertRowIndicesToCoordinates (findBoneInList bone (getRow board 0) 0) 0 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 1) 0) 1 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 2) 0) 2 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 3) 0) 3 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 4) 0) 4 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 5) 0) 5 ++
                                       convertRowIndicesToCoordinates (findBoneInList bone (getRow board 6) 0) 6

findBoneOnBoardVertical :: Bone -> Board -> [(Coordinate, Coordinate)]
findBoneOnBoardVertical bone board = convertColIndicesToCoordinates (findBoneInList bone (getColumn board 0) 0) 0 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 1) 0) 1 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 2) 0) 2 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 3) 0) 3 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 4) 0) 4 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 5) 0) 5 ++
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 6) 0) 6 ++ 
                                     convertColIndicesToCoordinates (findBoneInList bone (getColumn board 7) 0) 7

convertRowIndicesToCoordinates :: [(Int, Int)] -> Int -> [(Coordinate, Coordinate)]
convertRowIndicesToCoordinates [] c = []
convertRowIndicesToCoordinates xs c = [((fst (head xs), c),(snd (head xs), c))] ++ convertRowIndicesToCoordinates (tail xs) c


convertColIndicesToCoordinates :: [(Int, Int)] -> Int -> [(Coordinate, Coordinate)]
convertColIndicesToCoordinates [] c = []
convertColIndicesToCoordinates xs r = [((r, fst (head xs)),(r, snd (head xs)))] ++ convertColIndicesToCoordinates (tail xs) r

findBoneInList :: Bone -> [Int] -> Int -> [(Int, Int)]
findBoneInList bone xs index = if index < (length xs - 1) then ((if (valuesInList == bone || valuesInList == swap bone) then [(index, index + 1)] else []) ++ findBoneInList bone xs (index + 1)) else [] 
                                                                     where valuesInList = (xs !! index, xs !! (index + 1))

pairConsecutiveElementsInList :: [a] -> [(a,a)] -- works
pairConsecutiveElementsInList xs = zip xs $ tail xs

getBones :: [(Int, Bone)] -> Int -> [(Int, Bone)] -- works
getBones [] n = getBones [(1, (0, 0))] 1 
getBones bones n = if n < 28 then getBones (bones ++ [nextBone (last bones)]) (n + 1) else bones


nextBone :: (Int, Bone) -> (Int, Bone) -- works
nextBone (n ,(l, r)) = if (r < 6) then (n + 1, (l, r + 1)) else (n + 1, (l + 1, l + 1))

getEmptyBoard :: Board -- Works
getEmptyBoard = (replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), getBones[] 0)

replicateInt :: Int -> Int -> [Int] -- works
replicateInt n x = [x | n' <- [1..n]]

printList :: [Int] -> String -- works
printList [] = ""
printList xs = e ++ " " ++ printList (tail xs)
               where e = if (head xs) < 0 then "X" else show (head xs)

boardToString :: Board -> String -- works
boardToString (a, b, c , d, e, f, g, xs) = printList a ++ "\n" ++ printList b ++ "\n" ++ printList c ++ "\n" ++ printList d ++ "\n" ++ printList e ++ "\n" ++ printList f ++ "\n" ++ printList g ++ "\n"

printBoard :: Board -> IO () -- works
printBoard board = putStr(boardToString board) 

boardIsEmpty :: Board -> Bool -- works
boardIsEmpty (a, b, c , d, e, f, g, xs) = rowIsEmpty a && rowIsEmpty b && rowIsEmpty c && rowIsEmpty d && rowIsEmpty e && rowIsEmpty f && rowIsEmpty g

-- For empty cells a value of -1 is used
rowIsEmpty :: [Int] -> Bool -- works
rowIsEmpty [] = True
rowIsEmpty xs = if (head xs >= 0) then False else rowIsEmpty (tail xs)

getRow :: Board -> Int -> [Int] -- works
getRow (a, b, c , d, e, f, g, xs) row
   | row == 0 = a
   | row == 1 = b
   | row == 2 = c
   | row == 3 = d
   | row == 4 = e
   | row == 5 = f
   | row == 6 = g
   | otherwise = error "Wrong input for getRow function"

getColumn :: Board -> Int -> [Int] -- works
getColumn board n = [getRow board 0 !! n] ++
                    [getRow board 1 !! n] ++
                    [getRow board 2 !! n] ++
                    [getRow board 3 !! n] ++
                    [getRow board 4 !! n] ++
                    [getRow board 5 !! n] ++
                    [getRow board 6 !! n]
   
emptyCellInBoard :: Board -> Int -> Int -> Board -- works
emptyCellInBoard board row col = replaceRowInBoard board row (emptyCellInRow (getRow board row) col)

replaceRowInBoard :: Board -> Int -> [Int] -> Board -- works
replaceRowInBoard (a,b,c,d,e,f,g,xs) rowNumber newRow
   | rowNumber == 0 = (newRow, b, c, d, e, f, g, xs)
   | rowNumber == 1 = (a, newRow, c, d, e, f, g, xs)
   | rowNumber == 2 = (a, b, newRow, d, e, f, g, xs)
   | rowNumber == 3 = (a, b, c, newRow, e, f, g, xs)
   | rowNumber == 4 = (a, b, c, d, newRow, f, g, xs)
   | rowNumber == 5 = (a, b, c, d, e, newRow, g, xs)
   | rowNumber == 6 = (a, b, c, d, e, f, newRow, xs)
   | otherwise = error "Wrong input for replaceRowInBoard function"

emptyCellInRow :: [Int] -> Int -> [Int] -- works
emptyCellInRow col index = updateCellInRow col (-1) index

updateCellInRow :: [Int] -> Int -> Int -> [Int] -- works
updateCellInRow col value index = take index col ++ [value] ++ drop (index + 1) col

updateCellInBoard :: Board -> Int -> Int -> Int -> Board -- works
updateCellInBoard board value row col = replaceRowInBoard board row (updateCellInRow (getRow board row) value col)

updateCellInBoardUsingCoordinates :: Board -> Int -> (Coordinate, Coordinate) -> Board -- works
updateCellInBoardUsingCoordinates board value coors = updateCellInBoard newBoard value (snd (snd coors)) (fst (snd coors))
                                                         where newBoard = updateCellInBoard board value (snd (fst coors)) (fst (fst coors))

emptyCellInBoardUsingCoordinates :: Board -> (Coordinate, Coordinate) -> Board
emptyCellInBoardUsingCoordinates board coors = updateCellInBoardUsingCoordinates board (-1) coors

