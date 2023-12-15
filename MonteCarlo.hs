module MonteCarlo where

import Control.Monad
import Control.MonadMC
import Data.List
import Data.Map( Map )
import qualified Data.Map as Map
import System.Environment
import Text.Printf
import Hand


deal :: (MonadMC m) => m Hand
deal = sampleSubset 2

-- per experiment, need to remove the two given cards from the deck and then
-- select a random set of 5 - #comm + 2*#players cards. Then split the list on 5
-- - #comm, 2, 2, 2, 
-- construct all of the hands, run the experiment, accumulate the results, then
-- we're done




deck :: [Card] -> [Card]
deck bad = [Card s r | s <- suits, r <- ranks, ok s r]
    where
        ok x y = all $ map (/=(Card x y)) bad 



