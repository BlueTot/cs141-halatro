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
countRanks hand = filter (> 0) [numOccurrences hand r | r <- [Two ..]]

-- -- Get number of instances of a given occurrence in the list of occurrences of a hand
-- numRanks :: Hand -> Int -> Int
-- numRanks hand count = length $ filter (==count) $ countRanks hand

-- Check if there are at least k instances of a given occurrence or higher in the list of occurrences of a hand
enoughRankCount :: Hand -> Int -> Int -> Bool
enoughRankCount hand count target = length (filter (>=count) $ countRanks hand) == target

-- Function to get unique suites from a hand
uniqueSuites :: Hand -> [Suit]
uniqueSuites hand = toList $ fromList $ map (\(Card _ s) -> s) hand

-- Function to get ranks from a hand
ranks :: Hand -> [Rank]
ranks = map (\(Card r _) -> r)

-- Function to get unique ranks from a hand
uniqueRanks :: Hand -> [Rank]
uniqueRanks hand = toList $ fromList (ranks hand)

-- Function to check if a hand is ascending
isAscending :: Hand -> Bool
isAscending hand = 
    let
        scores = sort $ map rankScore $ ranks hand
    in 
        case scores of
            [2, 3, 4, 5, 11] -> True
            _ -> and [b - a == 1 | (a, b) <- zip scores (tail scores)]

isStraight :: Hand -> Bool
isStraight hand = 
    let
        handRanks = sort $ ranks hand
    in
        handRanks == [Two, Three, Four, Five, Ace] ||
        handRanks == [Ten, Jack, Queen, King, Ace] || isAscending hand

isHighCard :: Hand -> Bool
isHighCard hand = not (isStraight hand) && length (uniqueRanks hand) == 5

isFlush :: Hand -> Bool
isFlush hand = length (uniqueSuites hand) == 1

isRoyal :: Hand -> Bool
isRoyal hand = sort (ranks hand) == [Ten, Jack, Queen, King, Ace]

-- Main function for Exercise 1
contains :: Hand -> HandType -> Bool
contains hand handType
    | handType == RoyalFlush && isRoyal hand && isFlush hand = True
    | handType == StraightFlush && isStraight hand && isFlush hand = True
    | handType == FourOfAKind && enoughRankCount hand 4 1 = True
    | handType == FullHouse && sort (countRanks hand) == [2, 3] = True
    | handType == Flush && isFlush hand = True
    | handType == Straight && isStraight hand = True
    | handType == ThreeOfAKind && enoughRankCount hand 3 1 = True
    | handType == TwoPair && enoughRankCount hand 2 2 = True
    | handType == Pair && enoughRankCount hand 2 1 = True
    | handType == HighCard && isHighCard hand = True
    | handType == None && null hand = True
    | otherwise = False

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