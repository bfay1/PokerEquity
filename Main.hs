{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.CPUTime
import Data.List
import Data.Ord
import System.Random
import Control.Applicative
import Control.Parallel.Strategies
import Control.Monad.Par
import Control.DeepSeq
import Control.Concurrent.Async
import GHC.Generics (Generic)
import Control.Monad

-- import Control.Monad.MonteCarlo
-- import System.Random.TF

chunkSize :: Int
chunkSize = 100

forceToNF :: NFData a => a -> b -> b
forceToNF x y = x `deepseq` y

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Generic, Show, Eq)


data Rank =  Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    deriving (Generic, Show, Ord, Eq, Enum, Bounded)

ranks :: [Rank]
ranks = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]

suits :: [Suit]
suits = [Spades, Diamonds, Hearts, Clubs]

data Card = Card { suit :: Suit, rank :: Rank }


instance NFData Rank
instance NFData Suit

instance NFData Card where
  rnf (Card r s) = rnf r `seq` rnf s

instance Eq Card where
     x == y = rank x == rank y

instance Ord Card where
    x `compare` y = rank x `compare` rank y

instance Show Card where
    show (Card suit rank) = show rank ++ " of " ++ show suit


data HandRank = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
    deriving (Eq, Show, Ord, Enum, Bounded)


type Hand = [Card]
type Deck = [Card]
type Table = [[Card]]


share :: [Hand] -> Float
share (x:xs)
    | or $ map ((< ranking x) . ranking) xs = 0
    | otherwise = 1 / (fromIntegral $ (length . filter ((== ranking x) . ranking)) (x:xs))


ranking :: Hand -> (HandRank, [Rank])
ranking = (,) <$> classify' <*> f
    where f = map snd . sort . map ((,) <$> negate . length <*> rank . head) . groupBy (==) . sort


classify' :: Hand -> HandRank
classify' hand =
    case groups of
        [1,1,1,1,1] -> case undefined of
                        _ | straight && flush   -> StraightFlush
                        _ | straight            -> Straight
                        _ | flush               -> Flush
                        _ | otherwise           -> HighCard
        [1,1,1,2]                               -> Pair
        [1,2,2]                                 -> TwoPair
        [1,1,3]                                 -> ThreeOfAKind
        [2,3]                                   -> FullHouse
        [1,4]                                   -> FourOfAKind
    where
        xs = (sort . map rank) hand
        (s:ss) = map suit hand
        straight = all (\(x, y) -> succ x == y) (pairwise xs)
        flush = all (== s) ss
        groups = (sort . map length . group) xs
        pairwise ls = zip ls (tail ls)

shuffle :: StdGen -> Deck -> Deck
shuffle gen deck = fst $ foldl shuffleStep ([], gen) deck
    where
        shuffleStep (shuffled, g) cardIndex =
            let (index, newGen) = randomR (0, length shuffled) g
                (front, back) = splitAt index shuffled
            in (front ++ [cardIndex] ++ back, newGen)



deal :: StdGen -> Hand -> [Card] -> Int -> Table
deal gen user community n = deal' shuffled 
    where
        shuffled = community ++ shuffle gen deck
        deck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]] \\ complement
        complement = community ++ user
        deal' (a:b:c:d:e:fs) = [a,b,c,d,e] : user : (opponentCards n fs)
        opponentCards 0 _ = []
        opponentCards m (x:y:zs) = [x, y] : (opponentCards (m - 1) zs)



deal2 :: Hand -> [Card] -> Int -> [Card] -> Table
deal2 user community n shuffled = deal2' shuffled'
    where
        shuffled' = community ++ (shuffled \\ complement)
        complement = community ++ user
        deal2' (a:b:c:d:e:fs) = [a,b,c,d,e] : user : (opponentCards n fs)
        opponentCards 0 _ = []
        opponentCards m (x:y:zs) = [x, y] : (opponentCards (m - 1) zs)


userHand :: Hand
userHand = [Card Diamonds Ace, Card Hearts Ace]

bestHand :: [Card] -> Hand
bestHand cards = minimumBy (comparing ranking) $ filter ((==5) . length) (subsequences cards)

scoreRound :: Table -> Float
scoreRound (community:players) = share $ (map (bestHand . (++ community))) players

playRound :: Hand -> [Card] -> Int -> IO Float
playRound user community players = do
    gen <- newStdGen
    let (gen', _) = random gen :: (Int, StdGen) in
        return $ scoreRound (deal gen user community players)

playRound2 :: Hand -> [Card] -> Int -> [Card] -> Float
playRound2 user community players shuffled = do
    scoreRound (deal2 user community players shuffled)

-- sequential

monteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo n user community players pot = do
    let experiments = replicate n (playRound user community players)
    results <- sequence experiments
    return $ pot * (sum results / (fromIntegral n))

-- control.parallel

monteCarlo2 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo2 n user community players pot = do
    let experiments = replicate n (playRound user community players)
    results <- sequence (runEval $ parList rpar experiments)
    return $ pot * (sum results / fromIntegral n)

--
-- monteCarlo3 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
-- monteCarlo3 n user community players pot = do
--     let experiments = replicate n (playRound user community players)
--         results = parMap rpar (\experiment -> runEval experiment) experiments
--     return $ pot * (sum results / fromIntegral n)


-- chunking

-- monteCarlo3 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
-- monteCarlo3 n user community players pot = do
--     let experiments = replicate n (playRound user community players)
--         chunkedExperiments = chunkList 100 experiments  -- Adjust the chunk size as needed
--     results <- map (runEval . parList rseq) chunkedExperiments
--     return $ pot * (sum results / fromIntegral n)
--
-- chunkList :: Int -> [a] -> [[a]]
-- chunkList _ [] = []
-- chunkList n xs = take n xs : chunkList n (drop n xs)

-- async

monteCarlo4 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo4 n user community players pot = do
    results <- mapConcurrently (\_ -> playRound user community players) [1..n]
    return $ pot * (sum results / fromIntegral n)

-- async, pregenerate RNGs
-- monteCarlo5 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
-- monteCarlo5 n user community players pot = do
--     gen <- newStdGen  -- Initial RNG
--     let seeds = take n $ randoms gen  -- Generate a list of seeds
--     results <- mapConcurrently (\seed -> playRound2 (mkStdGen seed) user community players) seeds
--     return $ pot * (sum results / fromIntegral n)


-- monteCarlo6 :: Int -> Hand -> [Card] -> Int -> Float -> [Card] -> IO Float
-- monteCarlo6 n user community players pot shuffled = do
--     results <- mapM (\chunk -> mapConcurrently (\_ -> playRound2 user community players shuffled) chunk) $ chunksOf 100 [1..n]
--     return $ pot * (sum (concat results) / fromIntegral n)


monteCarlo7 :: Float -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo7 n user community players pot = do
    gen <- newStdGen  -- Initialize a new random number generator
    let initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]] 
    let shuffledDecks = map (\_ -> forceToNF (shuffle gen initialDeck) (shuffle gen initialDeck)) [1..n] `using` parList rdeepseq
    -- let experiments = foldM (\acc x -> do
    --                                 res <- playRound2 user community players x
    --                                 return $ acc + res
    --                         ) 0.0 shuffledDecks
    let experiments = map (\deck -> playRound2 user community players deck) shuffledDecks
    -- results <- sequence experiments
    return $ (sum experiments) / n



monteCarlo8 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float 
monteCarlo8 n user community players pot = do
    gen <- newStdGen  -- Initialize a new random number generator
    let generators = take n $ iterate (\(_, g) -> random g :: (Int, StdGen)) (0, gen)
    let initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]]
    let shuffledDecks = map (\(_, g) -> forceToNF (shuffle g initialDeck) (shuffle g initialDeck)) generators `using` parList rdeepseq
    -- let experiments = foldM (\acc x -> do
    --                                 res <- playRound2 user community players x
    --                                 return $ acc + res
    --                         ) 0.0 shuffledDecks
    let experiments = map (\deck -> playRound2 user community players deck) shuffledDecks
    -- results <- sequence experiments
    return $ (sum experiments) / fromIntegral n

monteCarlo9 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float 
monteCarlo9 n user community players pot = do
    gen <- newStdGen  -- Initialize a new random number generator
    let generators = take n $ iterate (\(_, g) -> random g :: (Int, StdGen)) (0, gen)
    let initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]]
    let shuffledDecks = map (\(_, g) -> forceToNF (shuffle g initialDeck) (shuffle g initialDeck)) generators `using` parList rdeepseq
    -- let experiments = concatMap (\chunk -> runEval $ parList rdeepseq (map (\deck -> !(playRound2 user community players deck)) chunk)) (chunksOf chunkSize shuffledDecks)
    let experiments = concatMap (\chunk -> runEval $ parList rdeepseq (map (\deck -> let result = playRound2 user community players deck in result `seq` result) chunk)) (chunksOf chunkSize shuffledDecks)
    return $ (sum experiments `using` rdeepseq) / fromIntegral n


one :: Hand
one = [Card Spades King, Card Clubs King, Card Hearts Two, Card Hearts Three, Card Hearts Four]

two :: Hand
two = [Card Diamonds King, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]

three :: Hand
three = [Card Diamonds Ace, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]

-- forceEvaluation :: NFData a => a -> a
-- forceEvaluation x = x `deepseq` x

main :: IO ()
main = do
    -- result <- monteCarlo7 100000 userHand [Card Diamonds Five] 3 100
    -- result <- monteCarlo 100000 userHand [Card Diamonds Five] 3 100
    result <- monteCarlo9 100000 userHand [Card Diamonds Five] 3 100
    putStrLn $ show result




