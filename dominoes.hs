import System.IO

type Board = ([Int], [Int], [Int], [Int], [Int], [Int], [Int])

helloWorld :: String
helloWorld = "Hello World"

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
             [5, 5, 3, 6, 1, 2, 3, 1])

printList :: [Int] -> String
printList [] = ""
printList xs = e ++ " " ++ printList (tail xs)
               where e = if (head xs) < 0 then "X" else show (head xs)


printBoard :: Board -> IO ()
printBoard (a, b, c , d, e, f, g) = putStr(printList a ++ "\n" ++ printList b ++ "\n" ++ printList c ++ "\n" ++ printList d ++ "\n" ++ printList e ++ "\n" ++ printList f ++ "\n" ++ printList g ++ "\n")