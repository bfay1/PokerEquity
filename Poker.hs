module Poker where

import Data.List
import Data.Ord
import System.Random
import Control.Applicative

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


one :: Hand
one = [Card Spades King, Card Clubs King, Card Hearts Two, Card Hearts Three, Card Hearts Four]

two :: Hand
two = [Card Diamonds King, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]

three :: Hand
three = [Card Diamonds Ace, Card Spades King, Card Spades Two, Card Spades Three, Card Spades Four]

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
userHand = [Card Diamonds Two, Card Hearts Four]

bestHand :: [Card] -> Hand
bestHand cards = minimumBy (comparing ranking) $ filter ((==5) . length) (subsequences cards)

scoreRound :: Table -> Float
scoreRound (community:players) = share $ (map (bestHand . (++ community))) players

playRound :: StdGen -> Hand -> [Card] -> Int -> Float -> Float
playRound gen user community players pot = pot * (scoreRound (deal gen user community players))


monteCarlo :: Int -> Hand -> [Card] -> Int -> Float -> IO Float
monteCarlo n user community players pot = do
    gen <- getStdGen
    let (gen', _) = random gen :: (Int, StdGen) in
        return ((sum $ map (\_ -> playRound gen user community players pot) [1..n]) / (fromIntegral n))


main :: IO ()
main = do 
    gen <- getStdGen
    putStrLn $ "Deal: " ++ (show (deal gen userHand [] 2))







