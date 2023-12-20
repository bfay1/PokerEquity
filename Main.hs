{-# LANGUAGE DeriveGeneric #-}
module Main where

-- for building and running instructions
-- https://github.com/bfay1/PokerEquity

import Data.List
import Data.Ord
import System.Random
import Control.Parallel.Strategies
import GHC.Generics
import System.Environment

stor :: String -> Rank
stor "Two" = Two
stor "Three" = Three
stor "Four" = Four
stor "Five" = Five
stor "Six" = Six
stor "Seven" = Seven
stor "Eight" = Eight
stor "Nine" = Nine
stor "Jack" = Jack
stor "Queen" = Queen
stor "King" = King
stor "Ace" = Ace
stor _ = Ace 

stos :: String -> Suit
stos "Hearts" = Hearts
stos "Diamonds" = Diamonds
stos "Clubs" = Clubs
stos "Spades" = Spades
stos _ = Spades

stoh :: (String, String) -> Card
stoh (s, r) = Card (stos s) (stor r)

data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Generic, Show, Eq)

data Rank =  Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
    deriving (Generic, Show, Ord, Eq, Enum, Bounded)

data HandRank = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
    deriving (Eq, Show, Ord, Enum, Bounded)

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
    show (Card s r) = show r ++ " of " ++ show s


type Hand = [Card]
type Deck = [Card]
type Table = [[Card]]


share :: [Hand] -> Float
share (x:xs)
    | or $ map ((< ranking x) . ranking) xs = 0
    | otherwise = 1 / (fromIntegral $ (length . filter ((== ranking x) . ranking)) (x:xs))
share _ = 0.0


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
        _                                       -> HighCard
    where
        xs = (sort . map rank) hand
        (s:ss) = map suit hand
        straight = all (\(x, y) -> succ x == y) (pairwise xs)
        flush = all (== s) ss
        groups = (sort . map length . group) xs
        pairwise ls = zip ls (tail ls)


 -- shuffles a deck into a random order
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
        deal' _ = []
        opponentCards 0 _ = []
        opponentCards m (x:y:zs) = [x, y] : (opponentCards (m - 1) zs)
        opponentCards _ _ = []


userHand :: Hand
userHand = [Card Diamonds Ace, Card Hearts Ace]

bestHand :: [Card] -> Hand
bestHand cards = minimumBy (comparing ranking) $ filter ((==5) . length) (subsequences cards)

scoreRound :: Table -> Float
scoreRound (community:players) = share $ (map (bestHand . (++ community))) players
scoreRound _ = 0.0


playRound :: StdGen -> Hand -> [Card] -> Int -> Float
playRound gen user community players = 
    scoreRound (deal gen user community players)


makeGenerators :: Int -> [StdGen]
makeGenerators n = runEval $ parList rseq (map mkStdGen [50..(50 + n - 1)])


monteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> Float
monteCarlo n user community players pot =
    let results = (map (\g -> playRound g user community players) (makeGenerators n)) in
    pot * (sum results / (fromIntegral n))


parallelMonteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> Float
parallelMonteCarlo n user community players pot =
    let results = runEval $ parList rseq (map (\g -> playRound g user community players) (makeGenerators n)) in
    pot * (sum results / (fromIntegral n))


recursiveMonteCarlo :: StdGen -> Int -> Hand -> [Card] -> Int -> Float -> [Float]
recursiveMonteCarlo _ 0 _ _ _ _ = []
recursiveMonteCarlo gen n user community players pot = 
    let (gen1, gen2) = split gen
    in (runEval $ rseq $ playRound gen1 user community players) : (runEval $ rseq $ recursiveMonteCarlo gen2 (n - 1) user community players pot)

recursiveChunkMonteCarlo :: StdGen -> Int -> Hand -> [Card] -> Int -> Float -> [Float]
recursiveChunkMonteCarlo _ 0 _ _ _ _ = []
recursiveChunkMonteCarlo gen n user community players pot = 
    let (gen1, gen2) = split gen
    in ( {- runEval $ parList rseq $ -} replicate 10 (runEval $ rseq $ playRound gen2 user community players)) ++ (runEval $ rseq $ recursiveChunkMonteCarlo gen1 (n - 10) user community players pot)

-- Create a fixed number of RNGs
makeFixedRNGs :: Int -> [StdGen]
makeFixedRNGs m = map mkStdGen [50..(49 + m)]

-- Function to divide experiments into chunks
divideIntoChunks :: Int -> [a] -> [[a]]
divideIntoChunks _ [] = []
divideIntoChunks n xs = take n xs : divideIntoChunks n (drop n xs)

-- Parallel Monte Carlo function using a fixed number of RNGs
parallelMonteCarloFixedRNGs :: Int -> Int -> Hand -> [Card] -> Int -> Float -> Float
parallelMonteCarloFixedRNGs m n user community players pot =
    let rngs = cycle (makeFixedRNGs m)  -- Creates a repeating list of RNGs
        results = runEval $ parList rseq [playRound gen user community players | gen <- take n rngs]
    in pot * ((sum $ runEval $ parList rseq results) / fromIntegral n)

-- Function to run a chunk of experiments
runChunk :: (StdGen, [(Hand, [Card], Int)]) -> [Float]
runChunk (gen, experiments) = 
    map (\(user, community, players) -> runEval $ rseq (playRound gen user community players)) experiments

initialDeck :: [Card]
initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]]


sequential :: Int -> Hand -> Int ->  Float
sequential e hand players = monteCarlo e hand [] players 100

naive :: Int -> Hand -> Int -> Float
naive e hand players = parallelMonteCarlo e hand [] players 100

chunk :: Int -> Hand -> Int -> Float
chunk e hand players = parallelMonteCarloFixedRNGs 30 e hand [] players 100

recursive :: Int -> Hand -> Int -> Float
recursive e hand players = 
    let gen = mkStdGen 42
        total = sum $ runEval $ parList rseq (recursiveMonteCarlo gen e hand [] players 100) in
    (total / fromIntegral e)

recursiveChunk :: Int -> Hand -> Int -> Float
recursiveChunk e hand players = 
    let gen = mkStdGen 42
        total = sum $ runEval $ parList rseq (recursiveChunkMonteCarlo gen e hand [] players 100) in
    (total / fromIntegral e)


findMethod :: String -> Int -> Hand -> Int -> Float 
findMethod "sequential" e hand players = sequential e hand players
findMethod "naive" e hand players = naive e hand players
findMethod "chunk" e hand players = chunk e hand players
findMethod "recursive" e hand players = recursive e hand players
findMethod "recursiveChunk" e hand players = recursiveChunk e hand players
findMethod _ _ _ _ = 0.0

main :: IO ()
main = do
    args <- getArgs
    if length args /= 7
        then putStrLn $ "Usage : ./Main <suit> <rank> <suit> <rank> <numplayers> <numexperiments> <method>"
    else do
        let [s1, r1, s2, r2, players, experiments, method] = args
        putStrLn $ show $ findMethod method (read experiments) [(stoh (s1, r1)), (stoh (s2, r2))] (read players)





