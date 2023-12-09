

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


sort' :: Hand -> HandRank
sort' hand
    | flush && straight             = StraightFlush
    | foak                          = FourOfAKind
    | fullHouse                     = FullHouse
    | flush                         = Flush
    | straight                      = Straight
    | toak                          = ThreeOfAKind
    | twoPair                       = TwoPair
    | pair                          = Pair
    | otherwise                     = HighCard
    where
        flush :: Bool
        flush = or [elem x [i | (_, i) <- suitCount] | x <- ([5..7] :: [Integer])]
        suitCount = toList $ fromListWith (+) [(suit card, 1) | card <- hand]

        straight :: Bool
        straight = or [all (\(x, y) -> succ x == y) (pairwise ls) | ls <- hands]

        hands = [take 5 hand, take 5 (drop 1 hand), drop 2 hand]
        rankSort = sort . map rank
        pairwise ls = zip (rankSort ls) (tail (rankSort ls))

        foak :: Bool
        foak = elem (4 :: Integer) [i | (_, i) <- rankCount]

        fullHouse :: Bool
        fullHouse = threeAndTwo || threeAndThree

        threeAndThree   = length [i | (_, i) <- rankCount, i == 3] >= 2
        threeAndTwo     = length twos >= 2 && elem 3 twos 
        twos            = [i | (_, i) <- rankCount, i >= 2]

        toak :: Bool
        toak = or [elem x [i | (_, i) <- rankCount] | x <- [3,4]]

        twoPair :: Bool
        twoPair = length (filter (>= 2) [i | (_, i) <- rankCount]) >= 2

        pair :: Bool
        pair = length (filter (>= 2) [i | (_, i) <- rankCount]) >= 1

        rankCount = toList $ fromListWith (+) [(rank card, 1) | card <- hand]

hash :: Hand -> [Rank]
hash hand = case (sort' hand) of
    StraightFlush       -> reverse . sort $ map rank hand
    Flush               -> reverse . sort $ map rank hand
    HighCard            -> reverse . sort $ map rank hand
    Straight            -> reverse . sort $ map rank hand
    Pair                -> reverse . sort $ map rank hand
    FourOfAKind         -> find' 4 rankCount ++ find' 1 rankCount
    FullHouse           -> find' 3 rankCount ++ find' 2 rankCount
    ThreeOfAKind        -> find' 3 rankCount ++ find' 1 rankCount
    TwoPair             -> find' 2 rankCount ++ find' 1 rankCount
    where
        rankCount = toList $ fromListWith (+) [(rank card, 1) | card <- hand]
        find' :: Integer -> [(Rank, Integer)] -> [Rank]
        find' ct rc = [r | (r, i) <- rc, i == ct]



shareOfPot :: Hand -> [Hand] -> Float
shareOfPot hand hands
    | or $ map (compare' hand) hands            = 0
    | otherwise                                 = 1 / (foldr (\x acc -> acc + (equal' x hand)) 0 hands)
    where
        compare' x y    = (sort' x, hash x) < (sort' y, hash y)
        equal' x y
             | (sort' x, hash x) == (sort' y, hash y)   = 1
             | otherwise                                = 0



