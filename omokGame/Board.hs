-- Jenifer Villalba && Cynthia Sustaita 
-- Haskell Omok Game / CS 3360
-- Last Updated: 4/30/2020

module Board where
    ----------------------  Part 1 Creating a board and accessing its elements ------------------------------------

    -- Return an empty nxn board, where n is a positive number.
    mkBoard:: Int -> [[Int]]
    mkBoard n = replicate n (replicate n 0)
    
    -- Create and return the first player.
    mkPlayer:: Int
    mkPlayer = 1

    -- Create and return the second player
    mkOpponent:: Int
    mkOpponent = 2

    -- Return the size of a board bd, n for an nxn board.
    size:: [[Int]] -> Int
    size [[]] = 0
    size bd = length bd

    -- Return a row y of a board bd, where y is a 1-based index. It returns a list of size n, where n is the size of bd.
    row:: Int -> [[Int]] -> [Int]
    row 0 _ = []
    row 1 (h : t) = h 
    row y (h : t) = row (y - 1) t

    -- Return a column x of a board bd, where x is a 1-based index. It returns a list of size n, where n is the size of bd.
    column:: Int -> [[Int]] -> [Int]
    column 0 _ = []
    column _ [] = []
    column x (h : t) = (h !! (x - 1)) : column x t

    ----------------------- Part 2 Checking places and placing stones ------------------------------------

    -- mark x y bd p
    -- Mark a place (x,y) in a board bd by a player p, where x and y are 1-based column and row indices.
    mark:: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x 1 (h : t) p = m : t
        where m = markRow x h p
    mark x y (h : t) p = h : mark x (y - 1) t p 

    -- markRow | mark helper method
    markRow:: Int -> [Int] -> Int -> [Int]
    markRow 1 (h : t) p = m : t 
        where m = if h == 0 then p else h 
    markRow n (h : t) p = h : markRow (n - 1) t p

    -- Is a place (x,y) of a board bd unmarked or a stone not placed? The x and y are 1-based column and row indices.  
    isEmpty:: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd = ((row y bd) !! (x - 1) == 0)

    -- Does a place (x,y) of a board bd have a stone placed? The x and y are 1-based column and row indices.  
    isMarked:: Int -> Int -> [[Int]] -> Bool   
    isMarked x y bd = ((row y bd) !! (x - 1) /= 0)

    -- Does a place (x,y) of a board bd have a stone placed by a player p? The x and y are 1-based column and row indices.     
    isMarkedBy:: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd p = ((row y bd) !! (x - 1) == p)

    -- Return the player of the stone placed on a place (x,y) of a board bd. The x and y are 1-based column and row indices.
    marker:: Int -> Int -> [[Int]] -> Int
    marker x y bd = (row y bd) !! (x - 1)

    -- Are all places of board bd marked
    isFull:: [[Int]] -> Bool
    isFull [] = True
    isFull bd = product [minimum x | x <-bd] /= 0

    ----------------------- Part 3 Determining the outcome ------------------------------------

    -- Is the game played on a board bd won by a player p?
    isWonBy:: [[Int]] -> Int -> Bool
    isWonBy bd p = m
        where m = (isWonByRow bd p || isWonByCol bd p 1 || isWonByDiagonalLR bd p || isWonByDiagonalRL bd p)

    ----------------------- isWonBy helper methods --------------------------------

    -- Checks for winning sequence in rows
    isWonByRow:: [[Int]] -> Int -> Bool
    isWonByRow [] _ = False
    isWonByRow (h : t) p = m
        where m = if (check h p 0) then True else isWonByRow t p

    -- Checks for winning sequence in columns
    isWonByCol:: [[Int]] -> Int -> Int -> Bool
    isWonByCol _ _ 16 = False
    isWonByCol bd p x = m
        where m = if (check (column x bd) p 0) then True else isWonByCol bd p (x + 1)

    -- Returns a list of diagonals represented as rows and checks winning sequence -> bottom left to top right
    isWonByDiagonalHelper:: [[Int]] -> [[Int]]
    isWonByDiagonalHelper bd =
        [[(bd !! x) !! y | x <- [0 .. ((size bd) - 1)], y <- [0 .. ((size bd) - 1)], x + y == k] | k <- [0 .. 2 * ((size bd) - 1)]]

    -- Reverses the board in order to use a list of diagonals represented as rows to make use of the same algorithm as bottom left to top right
    isWonByDiagonalLR:: [[Int]] -> Int -> Bool
    isWonByDiagonalLR bd p = m
        where m = isWonByRow (isWonByDiagonalHelper (reverse bd)) p 

    -- Checks for winning sequence in the representation on diagonals as rows
    isWonByDiagonalRL:: [[Int]] -> Int -> Bool
    isWonByDiagonalRL bd p = m
        where m = isWonByRow (isWonByDiagonalHelper bd) p

    -- Checks for winning sequence. If 5 consecutive pieces, then it is a winning sequence
    check:: [Int] -> Int -> Int -> Bool
    check _ _ 5 = True
    check [] _ _ = False
    check (h : t) p c = m 
        where m = if h == p then check t p (c + 1) else check t p 0
    
    -- Checks is board is full, then it is a draw
    isDraw:: [[Int]] -> Bool
    isDraw [[]] = True
    isDraw bd = isFull bd

    -- If the game has come to a draw or someone has won the game, then the game is over
    isGameOver:: [[Int]] -> Bool
    isGameOver bd  = m
        where m = isWonBy bd mkPlayer || isWonBy bd mkOpponent || isDraw bd

 ----------------------- Part 4 Converting to a string for printing ------------------------------------

    boardToStr:: (Int -> String) -> [[Int]] -> Int ->  String
    boardToStr _ [] _ = " " 
    boardToStr f (h : t) i =
        if (i == 1) then printx (h : t) i ++ printy (h : t) i ++ (show i ++ "|  " ) ++ m 
        else if  (i > 9) then (show (i-10) ++ "|  " ) ++ m
        else (show i ++ "|  " ) ++ m
        where m =  c h ++ boardToStr f t (i + 1) 

    --- Helper methods to print board with number coordinates
    printx:: [[Int]] -> Int -> String  
    printx bd n = 
        if (n == 1) then " x   " ++ (show n) ++ "  " ++ (printx bd (n + 1)) 
        else if (n <= (size bd)) then  if (n > 9) then (show (n - 10)) ++ "  " ++ (printx bd (n + 1)) else (show (n)) ++ "  " ++ (printx bd (n + 1)) 
        else  "\n"

    printy:: [[Int]] -> Int -> String  
    printy bd n = 
        if (n == 1) then "y  " ++ "---" ++ (printy bd (n+1)) 
        else if (n <= (size bd)) then "---" ++ (printy bd (n+1)) 
        else  "\n"

    playerToChar:: Int -> String
    playerToChar p
        | p == 1 = " O "
        | p == 2 = " X "
        | otherwise = " . "

    c:: [Int] -> String
    c [] = "\n" 
    c (h : t) = m 
        where m =  (playerToChar h) ++ c t