{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.List
import Data.Ord
import System.Random
import Control.Parallel.Strategies
import Control.DeepSeq
import GHC.Generics

-- import Data.Array.Repa
-- import Data.Array.Repa.Algorithms.Randomish
-- import Control.Monad.Par
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
    show (Card s r) = show r ++ " of " ++ show s


data HandRank = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
    deriving (Eq, Show, Ord, Enum, Bounded)


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


deal2 :: Hand -> [Card] -> Int -> [Card] -> Table
deal2 user community n shuffled = deal2' shuffled'
    where
        shuffled' = community ++ (shuffled \\ complement)
        complement = community ++ user
        deal2' (a:b:c:d:e:fs) = [a,b,c,d,e] : user : (opponentCards n fs)
        deal2' _ = []
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


--
-- playRound :: Hand -> [Card] -> Int -> Float
-- playRound user community players =
--     let gen = newStdGen :: IO StdGen
--         (gen', _) = random gen in
--         -- (gen', _) = random gen :: (Int, StdGen) in
--     scoreRound (deal gen' user community players)


playRound :: StdGen -> Hand -> [Card] -> Int -> Float
playRound gen user community players = 
    scoreRound (deal gen user community players)


makeGenerators :: Int -> [StdGen]
makeGenerators n = runEval $ parList rseq (map mkStdGen [50..(50 + n - 1)])


playRound2 :: Hand -> [Card] -> Int -> [Card] -> Float
playRound2 user community players shuffled = do
    scoreRound (deal2 user community players shuffled)


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
    in (runEval $ rseq $ playRound gen1 user community players) : (runEval $ rpar $ recursiveMonteCarlo gen2 (n - 1) user community players pot)
    -- in (playRound gen user community players) : (recursiveMonteCarlo gen2 (n - 1) user community players pot)

recursiveChunkMonteCarlo :: StdGen -> Int -> Hand -> [Card] -> Int -> Float -> [Float]
recursiveChunkMonteCarlo _ 0 _ _ _ _ = []
recursiveChunkMonteCarlo gen n user community players pot = 
    let (gen1, gen2) = split gen
    in ( {- runEval $ parList rseq $ -} replicate 10 (runEval $ rseq $ playRound gen2 user community players)) ++ (runEval $ rseq $ recursiveChunkMonteCarlo gen1 (n - 10) user community players pot)
    -- in (playRound gen user community players) : (recursiveMonteCarlo gen2 (n - 1) user community players pot)

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
    map (\(user, community, players) -> playRound gen user community players) experiments

initialDeck :: [Card]
initialDeck = [Card s r | r <- [minBound..maxBound], s <- [Hearts,Diamonds,Clubs,Spades]]

parseLine :: String -> (String, String)
parseLine line = case words line of
    [s, r] -> (s, r)
    _            -> error "Invalid line format"

readHandsFromFile :: FilePath -> IO [(String, String)]
readHandsFromFile filePath = do
    content <- readFile filePath
    let linesOfFile = lines content
    return $ map parseLine linesOfFile


main :: IO ()
main = do
    -- putStrLn $ show $ parallelMonteCarloFixedRNGs 30 10000 userHand [] 3 100
    let gen = mkStdGen 42
        total = sum $ runEval $ parList rseq (recursiveMonteCarlo gen 10000 userHand [] 3 100) in
        putStrLn $ show $ (total / 10000.0)
    -- let gen = mkStdGen 42
    --     total = sum $ runEval $ parList rseq (recursiveChunkMonteCarlo gen 10000 userHand [] 3 100) in
    --     putStrLn $ show $ (total / 10000.0)
    -- putStrLn $ show $ parallelMonteCarlo 10000 userHand [] 3 100
    -- putStrLn $ show $ monteCarlo 10000 userHand [] 3 100



