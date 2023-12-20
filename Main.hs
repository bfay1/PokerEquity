{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.CPUTime
import Data.List
import Data.Ord
import System.Random
import Control.Applicative
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Concurrent
import GHC.Generics
import Control.Monad

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


playRound :: StdGen -> Hand -> [Card] -> Int -> Float
playRound gen user community players = 
    scoreRound (deal gen user community players)

makeGenerators :: Int -> [StdGen]
makeGenerators n = runEval $ parList rpar (map mkStdGen [50..(50 + n - 1)])

monteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> Float
monteCarlo n user community players pot =
    let results = (map (\g -> playRound g user community players) (makeGenerators n)) in
    pot * (sum results / (fromIntegral n))

-- -- working parallelization
-- parallelMonteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> Float
-- parallelMonteCarlo n user community players pot =
--     let results = runEval $ parList rpar (map (\g -> playRound g user community players) (makeGenerators n)) in
--     pot * (sum results / (fromIntegral n))


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
    let rngs = cycle (makeFixedRNGs m) 
        results = runEval $ parList rseq [playRound gen user community players | gen <- take n rngs]
    in pot * (sum results / fromIntegral n)

-- Function to run a chunk of experiments
runChunk :: (StdGen, [(Hand, [Card], Int)]) -> [Float]
runChunk (gen, experiments) = 
    map (\(user, community, players) -> playRound gen user community players) experiments


initialDeck :: [Card]
initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]]

parseLine :: String -> (String, String)
parseLine line = case words line of
    [suit, rank] -> (suit, rank)
    _            -> error "Invalid line format"

readHandsFromFile :: FilePath -> IO [(String, String)]
readHandsFromFile filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map parseLine linesOfFile

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

listOfHands :: [Card] -> [[Card]]
listOfHands (x:y:xs) = [x, y] : listOfHands xs
listOfHands _ = []

mcHand :: Hand -> Float
mcHand hand = monteCarlo 1000 hand [] 3 100

runMc :: [Hand] -> [Float]
runMc ls = runEval $ parList rseq (map mcHand ls)

main :: IO ()
main = do

    putStrLn $ show $ parallelMonteCarloFixedRNGs 150 10000 userHand [] 3 100