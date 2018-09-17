
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

printBoard :: ([Int], [Int], [Int], [Int], [Int], [Int], [Int], [Int]) -> String
printBoard xss 