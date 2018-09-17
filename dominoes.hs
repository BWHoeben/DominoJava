import System.IO

type Board = ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [(Int, Bone)])
type Coordinate = (Int, Int) -- (x, y)
type Bone = (Int, Int) -- (leftPip, rightPip)

getInput :: ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int])
getInput = ([5, 0, 3, 5, 4, 5, 5],
            [4, 6, 2, 3, 0, 2, 5],
            [3, 0, 6, 6, 4, 2, 3],
            [6, 1, 5, 2, 1, 4, 6],
            [5, 2, 0, 3, 0, 4, 1],
            [3, 3, 4, 2, 0, 1, 2],
            [4, 1, 2, 0, 4, 6, 3],
            [6, 1, 0, 6, 1, 5, 1])

getInput2 :: Board
getInput2 = ([5, 4, 3, 6, 5, 3, 4, 6],
             [0, 6, 0, 1, 2, 3, 1, 1],
             [3, 2, 6, 5, 0, 4, 2, 0],
             [5, 3, 6, 2, 3, 2, 0, 6],
             [4, 0, 4, 1, 0, 0, 4, 1],
             [5, 2, 2, 4, 4, 1, 6, 5],
             [5, 5, 3, 6, 1, 2, 3, 1],
             getBones [] 0)

--solve :: Board -> [Board]

--updateBoard :: Board -> Board -> (Board, Board) -- (boardToSolve, boardToFill)
--updateBoard boardToSolve boardToFill = 

--getCombinations :: Board -> (Int, [(Coordinate, Coordinate)])
--getCombinations board = 

findBoneInList :: Bone -> [Int] -> Int -> [(Int, Int)]
findBoneInList (a, b) xs index = if (index + 1) > length xs then [] else (if (xs !! index, xs !! (index + 1)) == (a,b) || (xs !! index, xs !! (index + 1)) == (b, a) then [(index, index + 1)] else []) ++ findBoneInList (a,b) xs (index + 1)

pairConsecutiveElementsInList :: [a] -> [(a,a)]
pairConsecutiveElementsInList xs = zip xs $ tail xs

getBones :: [(Int, Bone)] -> Int -> [(Int, Bone)]
getBones [] n = getBones [(1, (0, 0))] 1 
getBones bones n = if n < 28 then getBones (bones ++ [nextBone (last bones)]) (n + 1) else bones


nextBone :: (Int, Bone) -> (Int, Bone)
nextBone (n ,(l, r)) = if (r < 6) then (n + 1, (l, r + 1)) else (n + 1, (l + 1, l + 1))

getEmptyBoard :: Board
getEmptyBoard = (replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), replicateInt 8 (-1), getBones[] 0)

replicateInt :: Int -> Int -> [Int]
replicateInt n x = [x | n' <- [1..n]]

printList :: [Int] -> String
printList [] = ""
printList xs = e ++ " " ++ printList (tail xs)
               where e = if (head xs) < 0 then "X" else show (head xs)


printBoard :: Board -> IO ()
printBoard (a, b, c , d, e, f, g, xs) = putStr(printList a ++ "\n" ++ printList b ++ "\n" ++ printList c ++ "\n" ++ printList d ++ "\n" ++ printList e ++ "\n" ++ printList f ++ "\n" ++ printList g ++ "\n")

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
getColumn board n = [getRow board 0 !! n] ++
                    [getRow board 1 !! n] ++
                    [getRow board 2 !! n] ++
                    [getRow board 3 !! n] ++
                    [getRow board 4 !! n] ++
                    [getRow board 5 !! n] ++
                    [getRow board 6 !! n]
   
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

