import System.IO
import Data.Tuple
import Data.List
import Data.Maybe

type Board = ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [(Int, Bone)])
type Coordinate = (Int, Int) -- (x, y)
type Bone = (Int, Int) -- (leftPip, rightPip)

getInput1 :: Board
getInput1 = ([5, 4, 3, 6, 5, 3, 4, 6],
             [0, 6, 0, 1, 2, 3, 1, 1],
             [3, 2, 6, 5, 0, 4, 2, 0],
             [5, 3, 6, 2, 3, 2, 0, 6],
             [4, 0, 4, 1, 0, 0, 4, 1],
             [5, 2, 2, 4, 4, 1, 6, 5],
             [5, 5, 3, 6, 1, 2, 3, 1],
             initBones)

getInput2 :: Board
getInput2 = ([4, 2, 5, 2, 6, 3, 5, 4],
             [5, 0, 4, 3, 1, 4, 1, 1],
             [1, 2, 3, 0, 2, 2, 2, 2],
             [1, 4, 0, 1, 3, 5, 6, 5],
             [4, 0, 6, 0, 3, 6, 6, 5],
             [4, 0, 1, 6, 4, 0, 3, 0],
             [6, 5, 3, 6, 2, 1, 5, 3],
             initBones)

getInput3 :: Board
getInput3 = ([6, 6, 2, 6, 5, 2, 4, 1],
             [1, 3, 2, 0, 1, 0, 3, 4],
             [1, 3, 2, 4, 6, 6, 5, 4],
             [1, 0, 4, 3, 2, 1, 1, 2],
             [5, 1, 3, 6, 0, 4, 5, 5],
             [5, 5, 4, 0, 2, 6, 0, 3],
             [6, 0, 5, 3, 4, 2, 0, 3],
             initBones)

getEmptyBoard :: Board
getEmptyBoard = (emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, emptyRow, initBones)
                where emptyRow = replicate 8 (-1)
main :: IO ()
main = do putStrLn "Select an input (1, 2 or 3):"
          answer <- getLine
          case answer of
           "1" -> start getInput1
           "2" -> start getInput2
           "3" -> start getInput3
           _ -> putStrLn "Invalid input. Terminating..."

start :: Board -> IO ()
start board = do putStr("Input: \n")
                 printBoard board
                 putStr("\nResult(s): \n")
                 printSolutions (iterateBoards[(board, getEmptyBoard)] [] [])

iterateBoards :: [(Board, Board)] -> [(Board, Board)] -> [(Board, Board)] -> [(Board, Board)]
iterateBoards [] [] [] = []
iterateBoards [] [] xs = xs
iterateBoards [] boardsToFill xs = iterateBoards boardsToFill [] xs
iterateBoards boardsToProcess boardsToFill xs = iterateBoards (tail boardsToProcess) (boardsToFill ++ if solved then [] else updateBoard (head boardsToProcess)) (if solved then xs ++ [head boardsToProcess] else xs)
                                          where solved = boardIsEmpty (fst (head boardsToProcess))

updateBoard :: (Board,Board) -> [(Board, Board)] -- (boardToSolve, boardToFill)
updateBoard (boardToSolve, boardToFill) = if occurrences == 0 then [] else solve boardToSolve boardToFill coors boneNumber
                                       where boneWithLowestOccurrence = getBoneWithLowestOccurrence boardToSolve
                                             occurrences = fst boneWithLowestOccurrence
                                             bone = snd (snd boneWithLowestOccurrence)
                                             boneNumber = fst (snd boneWithLowestOccurrence)
                                             coors = findBoneOnBoard bone boardToSolve

solve :: Board -> Board -> [(Coordinate, Coordinate)] -> Int -> [(Board, Board)] -- Input: boardToSolve, boardToFill, coordinates and boneNumber
solve _ _ [] _ = []
solve boardToSolve boardToFill coors boneNumber = [(emptyCellInBoardUsingCoordinates updatedBoard (head coors), updateCellInBoardUsingCoordinates boardToFill boneNumber (head coors))] ++ solve boardToSolve boardToFill (tail coors) boneNumber
                                                  where updatedBoard = removeBoneFromBoard boardToSolve boneNumber

printSolutions :: [(Board, Board)] -> IO()
printSolutions [] = return ()
printSolutions xs = do putStr(boardToString (snd (head xs)) ++ "\n")
                       printSolutions (tail xs)

removeBoneFromBoard :: Board -> Int -> Board
removeBoneFromBoard (a,b,c,d,e,f,g,xs) boneNumber = (a,b,c,d,e,f,g, removeBoneFromBones xs boneNumber)

removeBoneFromBones :: [(Int, Bone)] -> Int -> [(Int, Bone)]
removeBoneFromBones [] _ = []
removeBoneFromBones xs boneNumber = (if fst (head xs) == boneNumber then [] else [head xs]) ++ removeBoneFromBones (tail xs) boneNumber

getBoneWithLowestOccurrence :: Board -> (Int, (Int, Bone)) -- (frequency, (boneNumber, Bone))
getBoneWithLowestOccurrence board = (!!) boneOccurrences (fst (minimumWithIndex (occurrencesToList (boneOccurrences))))
                                          where boneOccurrences = calculateBoneOccurrences board
                                                minimumWithIndex xs = (fromJust (elemIndex (minimum xs) xs), minimum xs)

occurrencesToList :: [(Int, (Int, Bone))] -> [Int]
occurrencesToList [] = []
occurrencesToList xs = [fst (head xs)] ++ occurrencesToList (tail xs)  

calculateBoneOccurrences :: Board -> [(Int, (Int, Bone))] -- [(frequency, (boneNumber, Bone))]
calculateBoneOccurrences (a, b, c, d, e, f, g, []) = []
calculateBoneOccurrences (a, b, c, d, e, f, g, xs) = [(calculateBoneOccurrence (a, b, c, d, e, f, g, xs) (snd (head xs)) , (head xs))] ++ calculateBoneOccurrences (a, b, c, d, e, f, g, tail xs)
                                                     where calculateBoneOccurrence board bone = length (findBoneOnBoard bone board)

findBoneOnBoard :: Bone -> Board -> [(Coordinate, Coordinate)]
findBoneOnBoard bone board = findBoneOnBoardHorizontal bone board ++ findBoneOnBoardVertical bone board

findBoneOnBoardHorizontal :: Bone -> Board -> [(Coordinate, Coordinate)]
findBoneOnBoardHorizontal bone board = findBoneOnBoardHorizontal' bone board 0
                                        where findBoneOnBoardHorizontal' _ _ 7 = []
                                              findBoneOnBoardHorizontal' bone board x = findBoneInRow bone board x ++ findBoneOnBoardHorizontal' bone board (x + 1)

findBoneOnBoardVertical :: Bone -> Board -> [(Coordinate, Coordinate)]
findBoneOnBoardVertical bone board = findBoneOnBoardVertical' bone board 0
                                      where findBoneOnBoardVertical' _ _ 8 = []
                                            findBoneOnBoardVertical' bone board x = findBoneInCol bone board x ++ findBoneOnBoardVertical' bone board (x + 1)

findBoneInRow :: Bone -> Board -> Int -> [(Coordinate, Coordinate)]
findBoneInRow bone board x = convertRowIndicesToCoordinates (findBoneInList bone (getRow board x)) x

findBoneInCol :: Bone -> Board -> Int -> [(Coordinate, Coordinate)]
findBoneInCol bone board x = convertColIndicesToCoordinates (findBoneInList bone (getColumn board x)) x

convertRowIndicesToCoordinates :: [(Int, Int)] -> Int -> [(Coordinate, Coordinate)]
convertRowIndicesToCoordinates [] c = []
convertRowIndicesToCoordinates xs c = [((fst (head xs), c),(snd (head xs), c))] ++ convertRowIndicesToCoordinates (tail xs) c


convertColIndicesToCoordinates :: [(Int, Int)] -> Int -> [(Coordinate, Coordinate)]
convertColIndicesToCoordinates [] c = []
convertColIndicesToCoordinates xs r = [((r, fst (head xs)),(r, snd (head xs)))] ++ convertColIndicesToCoordinates (tail xs) r

findBoneInList :: Bone -> [Int] -> [(Int, Int)]
findBoneInList bone xs = findBoneInList' bone xs 0
                         where findBoneInList' bone xs index = if index < (length xs - 1) then ((if (valuesInList == bone || valuesInList == swap bone) then [(index, index + 1)] else []) ++ findBoneInList' bone xs (index + 1)) else [] 
                                                                     where valuesInList = (xs !! index, xs !! (index + 1))

pairConsecutiveElementsInList :: [a] -> [(a,a)]
pairConsecutiveElementsInList xs = zip xs $ tail xs

getBones :: [(Int, Bone)] -> Int -> [(Int, Bone)] -- (BoneNumber, Bone)
getBones [] n = getBones [(1, (0, 0))] 1 
getBones bones n = if n < 28 then getBones (bones ++ [nextBone (last bones)]) (n + 1) else bones

nextBone :: (Int, Bone) -> (Int, Bone)
nextBone (n ,(l, r)) = if (r < 6) then (n + 1, (l, r + 1)) else (n + 1, (l + 1, l + 1))

initBones :: [(Int, Bone)]
initBones = getBones [] 0

printList :: [Int] -> String
printList [] = ""
printList xs = e ++ " " ++ printList (tail xs)
               where e = if (head xs) < 0 then " X" else spacer ++ show (head xs)
                     spacer = if (head xs) < 10 then " " else ""

boardToString :: Board -> String
boardToString (a, b, c , d, e, f, g, xs) = printList a ++ "\n" ++ printList b ++ "\n" ++ printList c ++ "\n" ++ printList d ++ "\n" ++ printList e ++ "\n" ++ printList f ++ "\n" ++ printList g ++ "\n"

printBoard :: Board -> IO ()
printBoard board = putStr(boardToString board) 

boardIsEmpty :: Board -> Bool
boardIsEmpty (a, b, c , d, e, f, g, xs) = rowIsEmpty a && rowIsEmpty b && rowIsEmpty c && rowIsEmpty d && rowIsEmpty e && rowIsEmpty f && rowIsEmpty g

-- For empty cells a value of -1 is used
rowIsEmpty :: [Int] -> Bool
rowIsEmpty [] = True
rowIsEmpty xs = if (head xs >= 0) then False else rowIsEmpty (tail xs)

getRow :: Board -> Int -> [Int]
getRow (a, b, c , d, e, f, g, xs) row
   | row == 0 = a
   | row == 1 = b
   | row == 2 = c
   | row == 3 = d
   | row == 4 = e
   | row == 5 = f
   | row == 6 = g
   | otherwise = error "Wrong input for getRow function"

getColumn :: Board -> Int -> [Int]
getColumn board n = getColumn' board n 0
                     where getColumn' _ _ 7 = []
                           getColumn' board n x = [getRow board x !! n] ++ getColumn' board n (x + 1)
   
emptyCellInBoard :: Board -> Int -> Int -> Board
emptyCellInBoard board row col = replaceRowInBoard board row (emptyCellInRow (getRow board row) col)

replaceRowInBoard :: Board -> Int -> [Int] -> Board
replaceRowInBoard (a,b,c,d,e,f,g,xs) rowNumber newRow
   | rowNumber == 0 = (newRow, b, c, d, e, f, g, xs)
   | rowNumber == 1 = (a, newRow, c, d, e, f, g, xs)
   | rowNumber == 2 = (a, b, newRow, d, e, f, g, xs)
   | rowNumber == 3 = (a, b, c, newRow, e, f, g, xs)
   | rowNumber == 4 = (a, b, c, d, newRow, f, g, xs)
   | rowNumber == 5 = (a, b, c, d, e, newRow, g, xs)
   | rowNumber == 6 = (a, b, c, d, e, f, newRow, xs)
   | otherwise = error "Wrong input for replaceRowInBoard function"

emptyCellInRow :: [Int] -> Int -> [Int]
emptyCellInRow col index = updateCellInRow col (-1) index

updateCellInRow :: [Int] -> Int -> Int -> [Int]
updateCellInRow col value index = take index col ++ [value] ++ drop (index + 1) col

updateCellInBoard :: Board -> Int -> Int -> Int -> Board
updateCellInBoard board value row col = replaceRowInBoard board row (updateCellInRow (getRow board row) value col)

updateCellInBoardUsingCoordinates :: Board -> Int -> (Coordinate, Coordinate) -> Board
updateCellInBoardUsingCoordinates board value coors = updateCellInBoard newBoard value (snd (snd coors)) (fst (snd coors))
                                                         where newBoard = updateCellInBoard board value (snd (fst coors)) (fst (fst coors))

emptyCellInBoardUsingCoordinates :: Board -> (Coordinate, Coordinate) -> Board
emptyCellInBoardUsingCoordinates board coors = updateCellInBoardUsingCoordinates board (-1) coors