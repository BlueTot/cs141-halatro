module CourseworkOne where

-- import Halatro.Constants
import Halatro.Types
import Halatro.Constants
import Data.Set (toList, fromList)
import Data.List (sort)

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- Function to count the number of occurrences of a given rank in a hand
numOccurrences :: Hand -> Rank -> Int
numOccurrences hand cardRank = length $ filter (\(Card r _) -> r == cardRank) hand

-- Function to count the occurrences of each rank in a hand
countRanks :: Hand -> [Int]
countRanks hand = [numOccurrences hand r | r <- [Two ..]]

-- Get number of instances of a given occurrence in the list of occurrences of a hand
numRanks :: Hand -> Int -> Int
numRanks hand count = length $ filter (==count) $ countRanks hand

-- Function to get suites from a hand
suites :: Hand -> [Suit]
suites hand = toList $ fromList $ map (\(Card _ s) -> s) hand

ranks :: Hand -> [Rank]
ranks = map (\(Card r _) -> r)

-- Function to check if a hand is ascending
isAscending :: Hand -> Bool
isAscending hand = 
    let
        scores = sort $ map rankScore $ ranks hand
    in 
        case scores of
            [2, 3, 4, 5, 11] -> True
            _ -> and [b - a == 1 | (a, b) <- zip scores (tail scores)]

-- Main function for Exercise 1
contains :: Hand -> HandType -> Bool
contains hand handType = case handType of
    None -> null hand
    HighCard -> not $ null hand
    Pair -> numRanks hand 2 >= 1
    TwoPair -> numRanks hand 2 >= 2
    ThreeOfAKind -> numRanks hand 3 >= 1
    Straight -> isAscending hand
    Flush -> length (suites hand) == 1
    FullHouse -> numRanks hand 3 == 1 && numRanks hand 2 == 1
    FourOfAKind -> numRanks hand 4 >= 1
    StraightFlush -> isAscending hand && length (suites hand) == 1
    RoyalFlush -> sort (ranks hand) == [Ace, King, Queen, Jack, Ten]

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

bestHandType :: Hand -> HandType
bestHandType = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 3: score a played hand

whichCardsScore :: Hand -> [Card]
whichCardsScore = error "Not implemented"

scoreHand :: Hand -> Int
scoreHand = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

highestScoringHand :: [Card] -> Hand
highestScoringHand = error "Not implemented"

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

simpleAI :: [Move] -> [Card] -> Move
simpleAI = error "Not implemented"

sensibleAI :: [Move] -> [Card] -> Move
sensibleAI = error "Not implemented"

myAI :: [Move] -> [Card] -> Move
myAI = error "Not implemented"