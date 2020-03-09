module Splizzock.Internal (Hand(..)) where

import Control.Arrow
import System.Random

data Hand = Rock | Paper | Scissors | Lizard | Spock deriving (Eq, Show, Enum, Bounded)

instance Ord Hand where
    (<=) = \case
        Rock     -> flip elem [Paper, Spock]
        Paper    -> flip elem [Scissors, Lizard]
        Scissors -> flip elem [Rock, Spock]
        Lizard   -> flip elem [Rock, Scissors]
        Spock    -> flip elem [Paper, Lizard]

instance Random Hand where
    randomR (l,h) = let lo = fromEnum l
                        hi = fromEnum h
                    in  first toEnum . randomR (lo,hi)
    random = first toEnum . randomR (0,4)