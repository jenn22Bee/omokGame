-- Jenifer Villalba 
-- Partner Cynthia Sustaita 
-- Haskell Omok Gane
-- PL 
-- Last Updated: 4/22/2020
-- Due Date: 4/30/2020 
-- PART 1
--import Main
module Boardd where

----------------------  Part 1 Creating a board and accessing its elements
-- return empty nxn board 
mkBoard:: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0) 
-- board not save

-- create and return player 
mkPlayer:: Int 
mkPlayer = 1

mkOpponent:: Int
mkOpponent = 2

--  Return the size of a board bd, n for an nxn board.
size:: [[Int]] -> Int
size n = length n

-- Return a row y of a board bd, where y is a 1-based index. It returns
--a list of size n, where n is the size of bd.
row:: Int -> [[Int]] -> [Int]
row 0 _ = []
row 1 (h : t) = h 
row y (h : t) = row (y-1) t

-- Return a column x of a board bd, where x is a 1-based index. It
-- returns a list of size n, where n is the size of bd.
column:: Int -> [[Int]] -> [Int]
column 0 _ = []
column _ [] = []
--column 1 (h : t) = head (h) : column 1 t
column n (h : t) = (h !! (n-1)) : column n t


----------------------- part 2 Checking places and placing stones
mark:: Int -> Int -> [[Int]] -> Int -> [[Int]]
mark x 1 (h : t) p = m : t
 where m = markRow x h p
mark x y (h : t) p = h : mark x (y-1) t p 
 

markRow:: Int -> [Int] -> Int -> [Int]
markRow 1 (h : t) p = m : t
 where m = if h == 0 then p else h
markRow n (h : t) p = h : markRow (n - 1) t p 


isEmpty:: Int -> Int -> [[Int]] -> Bool
isEmpty x y bd = ((row y bd) !! (x - 1) == 0)


isMarked::Int -> Int -> [[Int]] -> Bool
isMarked x y bd = ((row y bd) !! (x - 1) /= 0)

isMarkedBy:: Int -> Int -> [[Int]] -> Int -> Bool
isMarkedBy x y bd p = ((row y bd) !! (x - 1) == p)

maker:: Int -> Int -> [[Int]] -> Int
maker x y bd = (row y bd) !! (x - 1) 

--- change to [[]]
isFull:: [[Int]] -> Bool
isFull [] = True
isFull bd = product [minimum x | x <-bd] /= 0

  ----- PART 3  Determining the outcome
  -- Check if 5 places in a row or in column are the same p
  --- have to check if there is a win by row or by column or diagonal 

isWonBy:: [[Int]] -> Int -> Bool
isWonBy bd p = m
 where m = (isWonByRow bd p || isWonByCol bd p 1)

-- check if there are # of the same p together 
-- is use by isWonByRow and  isWonByColumn 
check:: [Int] -> Int -> Int -> Bool
check _ _ 3 = True -- for a small example 
check [] _ _ = False
check (h : t) p c = m 
 where m = if h == p then check t p (c+1) else check t p 0

-- check ny row 
isWonByRow:: [[Int]] -> Int -> Bool
isWonByRow [] _ = False
isWonByRow (h : t) p = m
 where m = if (check h p 0) then True else isWonByRow t p

-- check by column if there is a win
isWonByCol::  [[Int]] -> Int -> Int -> Bool
isWonByCol _ _ 4 = False -- the base case would be the size of the game meaning it cheack all the board and no win by column 
isWonByCol bd p x = m
    where m = if (check (column x bd) p 0) then True else isWonByCol bd p (x+1)


isGameOver:: [[Int]] -> Bool
isGameOver bd  = m
 where m = isWonBy bd mkPlayer || isWonBy bd mkOpponent 

 ---- PART 4 Converting to a string for printing
---- board works just it need to put putStrLn in command 
-- line then call the method 
-- for example putStrLn (boardToStr playerToChar [[0,1,0],[2,2,0],[0,0,0]])
boardToStr:: (Int -> String) -> [[Int]] -> String
boardToStr _ [] = " " 
boardToStr f (h : t) = m 
  where m = c h ++ boardToStr f t

-- do not know how to have this on main and use it here 
playerToChar:: Int -> String
playerToChar p
 | p == 1 = " O "
 | p == 2 = " X "
 | otherwise = " . "

-- method make each a single row to Strign
c:: [Int] -> String
c [] =  "\n" 
c (h : t)  = m 
 where m =  (playerToChar h) ++ c t

