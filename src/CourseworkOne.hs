module CourseworkOne where

-- import Halatro.Constants
import Halatro.Types
import Halatro.Constants ( rankScore, handTypeValues )
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

-- Function to get the best hand type from a list of possible hand types
bestHandTypeFrom :: Hand -> [HandType] -> HandType
bestHandTypeFrom _ [] = None
bestHandTypeFrom hand (x:xs) = if contains hand x then x else bestHandTypeFrom hand xs

-- Main function for Exericse 2
bestHandType :: Hand -> HandType
bestHandType hand = bestHandTypeFrom hand $ reverse [None ..]

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Function to count number of times each rank occurs in the hand
counterRank :: Hand -> [(Rank, Int)]
counterRank hand = [(r, numOccurrences hand r) | r <- [Two ..]]

-- Function to get the ranks with a certain count in the hand
rankWithCount :: Hand -> Int -> [Rank]
rankWithCount hand count = map fst $ filter (\(_, c) -> c == count) $ counterRank hand

-- Function to get the score of the highest rank in the hand
highestRankScore :: Hand -> Int
highestRankScore hand = maximum (map rankScore (ranks hand))

-- Main function for Exercise 3
whichCardsScore :: Hand -> [Card]
whichCardsScore hand =
    let
        pairs = rankWithCount hand 2
        triplets = rankWithCount hand 3
        quads = rankWithCount hand 4
    in case bestHandType hand of
        None -> []
        HighCard -> [maximum hand]
        Pair -> filter (\(Card r _) -> r `elem` pairs) hand
        TwoPair -> filter (\(Card r _) -> r `elem` pairs) hand
        ThreeOfAKind -> filter (\(Card r _) -> r `elem` triplets) hand
        Straight -> hand
        Flush -> hand
        FullHouse -> hand
        FourOfAKind -> filter (\(Card r _) -> r `elem` quads) hand
        StraightFlush -> hand
        RoyalFlush -> hand

-- Main function for Exercise 4
scoreHand :: Hand -> Int
scoreHand hand =
    let
        scoreCards = whichCardsScore hand
        best = bestHandType hand
        base = fst (handTypeValues best)
        mult = snd (handTypeValues best)
        cardVals = sum (map (rankScore . (\(Card r _) -> r)) scoreCards)
    in
        (base + cardVals) * mult

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

-- Function to check if a card is less than all of the first card in the list if it exists
isLessThanStarting :: [Card] -> Card -> Bool
isLessThanStarting [] _ = True
isLessThanStarting (x:_) y = y < x

-- Function to get all possible combinations of 5 cards out of n >= 5 cards
combinations :: [Card] -> Int -> [Hand]
combinations _ 0 = [[]]
combinations xs k = [x : y | x <- xs, y <- combinations xs (k-1), isLessThanStarting y x]

-- Main function for Exercise 5
highestScoringHand :: [Card] -> Hand
highestScoringHand [] = []
highestScoringHand xs =
    let
        outputLength = min 5 (length xs)
        allCombinations = concat [combinations xs l | l <- [1 .. outputLength]]
        maxScore = maximum $ map scoreHand allCombinations
        maxHands = filter (\c -> scoreHand c == maxScore) allCombinations
        minLength = minimum $ map length maxHands
        choices = filter (\c -> length c == minLength) maxHands
    in
        case minLength of
            1 -> [maximum $ map head choices]
            _ -> head choices

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

-- Main function for Exercise 6
simpleAI :: [Move] -> [Card] -> Move
simpleAI _ cards = Move Play $ take 5 $ reverse $ sort cards

-- Main function for Exercise 7
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ cards = Move Play $ highestScoringHand cards

-- Main function for Exercise 8
myAI :: [Move] -> [Card] -> Move
myAI = error "Not implemented"