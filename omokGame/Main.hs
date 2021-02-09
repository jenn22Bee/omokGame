module Main where

    import System.IO
    import Board

    -- Making a 15x15 board and making player 1 as initial player
    main = do
        putStrLn (boardToStr playerToChar (mkBoard 15) 1)
        readXY (mkBoard 15) mkPlayer

    -- User inputs x and y separately; If input is -1, the user exits the game; If input is out of the board boundaries, user is requested to input the coordinates once again
    readXY:: [[Int]] -> Int -> IO ()
    readXY bd p = do
        putStrLn ((playerToChar p) ++ "'s turn: Enter X (1-15) - Enter -1 to exit")
        line <- getLine
        let x = (read line :: Int) in
            if x > 0 && x <= 15 then do
                putStrLn ((playerToChar p) ++ "'s turn: Enter Y (1-15) - Enter -1 to exit")
                line <- getLine
                let y = (read line :: Int) in
                    if y > 0 && y <= 15 then
                        if isMarked x y bd then do
                            putStrLn "Place already occupied :c"
                            readXY bd p
                        else do
                            let gameBoard = mark x y bd p
                            putStrLn (boardToStr playerToChar gameBoard 1)
                            if isWonBy gameBoard p then
                                putStrLn ((playerToChar p) ++ " has Won the game")
                                --putStrLn "The game is over"
                            else if isDraw gameBoard then
                                putStrLn "The game has come to a draw"
                            else
                                if p == 1 then
                                    readXY gameBoard mkOpponent
                                else
                                    readXY gameBoard mkPlayer
                    else if y == -1 then do
                        putStrLn "Thanks for playing c:"
                        return ()
                    else
                        readXY'
            else if x == -1 then do
                putStrLn "Thanks for playing c:"
                return ()
            else 
                readXY'                                   
        where
            readXY' = do
                putStrLn "Invalid Input :c"
                readXY bd p