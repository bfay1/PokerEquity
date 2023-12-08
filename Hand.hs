

import Data.Map (fromListWith, toList)
import Data.List (sort)


data Suit =
      Spades
    | Hearts
    | Diamonds
    | Clubs
    deriving (Show, Ord, Eq, Enum, Bounded)


data Rank = 
      Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two
    deriving (Show, Ord, Eq, Enum, Bounded)


rankMap :: Rank -> Int
rankMap Two     = 2
rankMap Three   = 3
rankMap Four    = 4
rankMap Five    = 5
rankMap Six     = 6
rankMap Seven   = 7
rankMap Eight   = 8
rankMap Nine    = 9
rankMap Ten     = 10
rankMap Jack    = 11
rankMap Queen   = 12
rankMap King    = 13
rankMap Ace     = 14



data Card = Card { suit :: Suit
                , rank :: Rank }

instance Eq Card where
     x == y = rank x == rank y

instance Ord Card where
    x `compare` y = rank x `compare` rank y

instance Show Card where
    show (Card suit rank) = show suit ++ ", " ++ show rank


data HandRank =
      StraightFlush
    | FourOfAKind
    | FullHouse
    | Flush
    | Straight
    | ThreeOfAKind
    | TwoPair
    | Pair
    | HighCard
    deriving (Eq, Show, Ord, Enum, Bounded)


type Hand = [Card]


compareHands :: Hand -> Hand -> Bool
compareHands x y = (classifyHand x, hash x) < (classifyHand y, hash y)

countRanks :: Hand -> [(Rank, Integer)]
countRanks hand = toList $ fromListWith (+) [(rank card, 1) | card <- hand]


countSuits :: Hand -> [(Suit, Integer)]
countSuits hand = toList $ fromListWith (+) [(suit card, 1) | card <- hand]

flush :: Hand -> Bool
flush hand = or [elem x [i | (_, i) <- countSuits hand] | x <- [5..7]]

foak :: Hand -> Bool
foak hand = elem (4 :: Integer) [i | (_, i) <- countRanks hand]

fullHouse :: Hand -> Bool
fullHouse hand = threeAndTwo || threeAndThree
    where
        threeAndThree   = length [i | (_, i) <- countRanks hand, i == 3] >= 2
        threeAndTwo     = length twos >= 2 && elem 3 twos 
        twos            = [i | (_, i) <- countRanks hand, i >= 2]

toak :: Hand -> Bool
toak hand = or [elem x [i | (_, i) <- countRanks hand] | x <- [3,4]]

twoPair :: Hand -> Bool
twoPair hand = length (filter (>= 2) [i | (_, i) <- countRanks hand]) >= 2

pair :: Hand -> Bool
pair hand = length (filter (>= 2) [i | (_, i) <- countRanks hand]) >= 1


straight :: Hand -> Bool
straight hand = or [all (\(x, y) -> succ x == y) (pairwise ls) | ls <- hands]
    where
        hands = [take 5 hand, take 5 (drop 1 hand), drop 2 hand]
        rankSort = sort . map rank
        pairwise ls = zip (rankSort ls) (tail (rankSort ls))



classifyHand :: Hand -> HandRank
classifyHand hand
    | flush hand && straight hand   = StraightFlush
    | foak hand                     = FourOfAKind
    | fullHouse hand                = FullHouse
    | toak hand                     = ThreeOfAKind
    | twoPair hand                  = TwoPair
    | pair hand                     = Pair
    | otherwise                     = HighCard


high :: Hand -> Rank
high = maximum . map rank

hash :: Hand -> [Rank]
hash hand = hash' hand (classifyHand hand)

hash' :: Hand -> HandRank -> [Rank]
hash' hand handRank = case handRank of
    StraightFlush       -> reverse . sort $ map rank hand
    Flush               -> reverse . sort $ map rank hand
    HighCard            -> reverse . sort $ map rank hand
    Straight            -> reverse . sort $ map rank hand
    Pair                -> reverse . sort $ map rank hand
    FourOfAKind         -> fnd 4 rankCount ++ fnd 1 rankCount
    FullHouse           -> fnd 3 rankCount ++ fnd 2 rankCount
    ThreeOfAKind        -> fnd 3 rankCount ++ fnd 1 rankCount
    TwoPair             -> fnd 2 rankCount ++ fnd 1 rankCount
    where
        rankCount = countRanks hand
        fnd ct rc = [r | (r, i) <- rc, i == ct]






