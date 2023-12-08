import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case args of 
        firstCard : secondCard : numPlayers : communityCards -> do
            putStrLn $ "Hand: " ++ firstCard ++ ", " ++ secondCard ++ concat [", " ++ c | c <- communityCards]
        _ -> putStrLn $ "Please enter a hand and a number of players. Optionally provide community cards."
