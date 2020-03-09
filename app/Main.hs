module Main where


import Control.Monad
import Data.Bool
import Data.Char
import Splizzock.Internal
import System.IO
import System.Random

main :: IO ()
main = putStrLn "\n. - •   S P L I Z Z O C K   • - ." >> runGame 1 0 0

runGame :: Int -> Int -> Int -> IO ()
runGame n player ai = do
    putStr "\nPick a hand to play:  (r)ock, (p)aper, (s)cissors, (l)izard, spoc(k)\nYour hand: "
    aiHand <- handAI
    hFlush stdout
    hand <- getLine
    when (null hand || map toUpper hand `notElem` ["R","P","S","L","K"])
        $ putStrLn "I don't understand.\n" >> hFlush stdout >> runGame n player ai
    let playerHand = case map toUpper hand of
            "R" -> Rock
            "P" -> Paper
            "S" -> Scissors
            "L" -> Lizard
            "K" -> Spock
    putStrLn $ ("\nYou played: " ++ show playerHand ++ ", I played: " ++ show aiHand ++ "\n\n") ++ winnerDialogH playerHand aiHand
    hFlush stdout
    let (newn, newp, newa) = case compare playerHand aiHand of
            LT -> (n+1, player, ai+1)
            GT -> (n+1, player+1, ai)
            EQ -> (n, player, ai)
    when (newp > 0 || newa > 0) . putStrLn $ "YOU: " ++ show newp ++ " points\nAI : " ++ show newa ++ " points\n"
    putStr $ if newn == 1 then "Try again? (y/n) " else bestOf newn newp newa
    hFlush stdout
    bool (putStrLn $ "\n" ++ winnerDialog newp newa) (runGame newn newp newa) . (=="Y") . map toUpper =<< getLine

bestOf :: Int -> Int -> Int -> String
bestOf n p1 p2 = if   max p1 p2 < n - 1
                 then "Best " ++ show (n - min p1 p2) ++ " out of " ++ show (2*(n - min p1 p2) - 1) ++ "? (y/n) "
                 else "Best " ++ show n ++ " out of " ++ show (2*n - 1) ++ "? (y/n) "

winnerDialog :: Int -> Int -> String
winnerDialog h1 h2 = case compare h1 h2 of
    LT -> "I win!"
    GT -> "You win!!!"
    EQ -> "We tied!"
winnerDialogH :: Hand -> Hand -> String
winnerDialogH h1 h2 = case compare h1 h2 of
    LT -> "I win!"
    GT -> "You win!!!"
    EQ -> "We tied!"

handAI :: IO Hand
handAI = fst . random <$> newStdGen