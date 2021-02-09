module MarkRow where

size:: [[Int]] -> Int
size n = length n

mkBoard:: Int -> [[Int]]
mkBoard n = replicate n (replicate n 0) 

--  Return the size of a board bd, n for an nxn board.
boardToStr:: (Int -> String) -> [[Int]] -> Int ->  String
boardToStr _ [] _ = " " 
boardToStr f (h : t) i =
--if (i > 9) then 
   if (i == 1) then printt (h : t) i ++ printy (h : t) i ++ (show i ++ "|  " ) ++ m 
    -- where m =  c h ++ boardToStr f t (i+1) 
    else (show i ++ "|  " ) ++ m
      where m =  c h ++ boardToStr f t (i+1) 


printt:: [[Int]] -> Int -> String  
printt bd n = 
    if (n == 1) then " x   " ++ (show n) ++ "  " ++ (printt bd (n+1)) 
   else if (n <= (size bd)) then (show n) ++ "  " ++ (printt bd (n+1)) 
   else  "\n"

printy:: [[Int]] -> Int -> String  
printy bd n = 
    if (n == 1) then "y  " ++ "---" ++ (printy bd (n+1)) 
   else if (n <= (size bd)) then "---" ++ (printy bd (n+1)) 
   else  "\n"



-- do not know how to have this on main and use it here 
playerToChar:: Int -> String
playerToChar p
 | p == 1 = " O "
 | p == 2 = " X "
 | otherwise = " . "

-- method make each a single row to Strign
c:: [Int] -> String
c []  =  "\n" 
c (h : t) = m 
 where m =  (playerToChar h) ++ c t
