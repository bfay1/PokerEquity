module Main where

import Data.List
import Data.Ord
import System.Random
import Control.Applicative
import Control.Parallel.Strategies
import Control.Monad.Par
import Control.Concurrent.Async

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show, Eq)


data Rank =  Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    deriving (Show, Ord, Eq, Enum, Bounded)

ranks :: [Rank]
ranks = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]


suits :: [Suit]
suits = [Spades, Diamonds, Hearts, Clubs]

data Card = Card { suit :: Suit, rank :: Rank }

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

playRound2 :: StdGen -> Hand -> [Card] -> Int -> IO Float
playRound2 gen user community players = return $ scoreRound (deal gen user community players)

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
monteCarlo5 :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo5 n user community players pot = do
    gen <- newStdGen  -- Initial RNG
    let seeds = take n $ randoms gen  -- Generate a list of seeds
    results <- mapConcurrently (\seed -> playRound2 (mkStdGen seed) user community players) seeds
    return $ pot * (sum results / fromIntegral n)


one :: Hand
one = [Card Spades King, Card Clubs King, Card Hearts Two, Card Hearts Three, Card Hearts Four]

two :: Hand
two = [Card Diamonds King, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]

three :: Hand
three = [Card Diamonds Ace, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]


main :: IO ()
main = do
    ans <- monteCarlo2 50000 userHand [Card Hearts Nine] 3 500
    putStrLn $ show ans




