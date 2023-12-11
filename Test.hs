module Test where

import Hand
import Control.Monad (unless)


-- Include your Suit, Rank, Card, HandRank, and Hand definitions here
-- Include the sort' function here

-- Helper function to create a card
makeCard :: Suit -> Rank -> Card
makeCard s r = Card { suit = s, rank = r }

-- Test cases
testHandRanking :: IO ()
testHandRanking = do
    -- -- Test for Straight Flush
    -- let straightFlushHand = [makeCard Hearts Ten, makeCard Hearts Jack, makeCard Hearts Queen, makeCard Hearts King, makeCard Hearts Ace]
    -- assert "Straight Flush" (sort' straightFlushHand == StraightFlush)

    -- -- Test for Four of a Kind
    -- let foakHand = [makeCard Spades Ten, makeCard Hearts Ten, makeCard Diamonds Ten, makeCard Clubs Ten, makeCard Hearts Two]
    -- assert "Four of a Kind" (sort' foakHand == FourOfAKind)

    -- -- Test for Full House
    -- let fullHouseHand = [makeCard Spades Three, makeCard Hearts Three, makeCard Diamonds Three, makeCard Clubs Two, makeCard Hearts Two]
    -- assert "Full House" (sort' fullHouseHand == FullHouse)

    -- -- Test for Flush
    -- let flushHand = [makeCard Spades Two, makeCard Spades Five, makeCard Spades Nine, makeCard Spades Jack, makeCard Spades Ace]
    -- assert "Flush" (sort' flushHand == Flush)

    -- -- Test for Straight
    -- let straightHand = [makeCard Hearts Two, makeCard Spades Three, makeCard Diamonds Four, makeCard Clubs Five, makeCard Hearts Six]
    -- assert "Straight" (sort' straightHand == Straight)

    -- Test for Three of a Kind
    let toakHand = [makeCard Spades Four, makeCard Hearts Four, makeCard Diamonds Four, makeCard Clubs Two, makeCard Hearts Three]
    let handRank = sort' toakHand
    putStrLn $ "Expected: ThreeOfAKind, Got: " ++ show handRank
    unless (handRank == ThreeOfAKind) $ error "Test failed: Three Of A Kind"


    -- -- Test for Two Pair
    -- let twoPairHand = [makeCard Spades Five, makeCard Hearts Five, makeCard Diamonds Six, makeCard Clubs Six, makeCard Spades Two]
    -- assert "Two Pair" (sort' twoPairHand == TwoPair)

    -- -- Test for One Pair
    -- let onePairHand = [makeCard Spades Seven, makeCard Hearts Seven, makeCard Diamonds Eight, makeCard Clubs Nine, makeCard Spades Ten]
    -- assert "One Pair" (sort' onePairHand == Pair)

    -- -- Test for High Card
    -- let highCardHand = [makeCard Spades Three, makeCard Hearts Five, makeCard Diamonds Seven, makeCard Clubs Nine, makeCard Spades Queen]
    -- assert "High Card" (sort' highCardHand == HighCard)

    putStrLn "All tests passed."

-- Assertion function
assert :: String -> Bool -> IO ()
assert testname condition = do
    if condition
    then putStrLn $ "Test passed: " ++ testname
    else error $ "Test failed: " ++ testname

-- Main function to run all tests
main :: IO ()
main = testHandRanking
