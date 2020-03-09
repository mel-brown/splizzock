module Main where

import Data.Bool
import Data.Char
import Splizzock.Internal
import System.IO
import System.Random

main :: IO ()
main = putStrLn "\n. - •   S P L I Z Z O C K   • - .\n" >> runGame

runGame :: IO ()
runGame = do
    putStr "Pick a hand to play:  (r)ock, (p)aper, (s)cissors, (l)izard, spoc(k)\nYour hand: "
    hFlush stdout
    hand <- getLine
    case map toUpper hand of
        "R" -> putStrLn =<< winnerDialog Rock     <$> handAI
        "P" -> putStrLn =<< winnerDialog Paper    <$> handAI
        "S" -> putStrLn =<< winnerDialog Scissors <$> handAI
        "L" -> putStrLn =<< winnerDialog Lizard   <$> handAI
        "K" -> putStrLn =<< winnerDialog Spock    <$> handAI
        _   -> putStrLn "I don't understand.\n" >> runGame
    hFlush stdout
    putStr "\nPlay again? (y/n) "
    hFlush stdout
    bool (pure ()) runGame . (=="Y") . map toUpper =<< getLine

winnerDialog :: Hand -> Hand -> String
winnerDialog h1 h2 = (("\nYou played: " ++ show h1 ++ ", I played: " ++ show h2 ++ "\n\n") ++) . bool "I win!" "You win!!!!" $ h1 >= h2

handAI :: IO Hand
handAI = fst . random <$> newStdGen