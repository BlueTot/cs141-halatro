module CourseworkOne where

import Halatro.Types
import Halatro.Constants ( rankScore, handTypeValues )
import Data.Set (toList, fromList)
import Data.List (sort, (\\), sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Maybe (catMaybes)

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- Function to count the number of occurrences of a given rank in a hand
numOccurrencesRank :: Hand -> Rank -> Int
numOccurrencesRank hand cardRank = length $ filter (\(Card r _) -> r == cardRank) hand

-- Function to count the number of occurrences of a given suit in a hand
numOccurrencesSuit :: Hand -> Suit -> Int
numOccurrencesSuit hand cardSuit = length $ filter (\(Card _ s) -> s == cardSuit) hand

-- Function to count the occurrences of each rank in a hand
countRanks :: Hand -> [Int]
countRanks hand = filter (> 0) [numOccurrencesRank hand r | r <- [Two ..]]

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
        scores = sort $ map fromEnum $ ranks hand
    in
        case scores of
            [2, 3, 4, 5, 11] -> True
            _ -> and [b - a == 1 | (a, b) <- zip scores (tail scores)]

-- Function to check if a hand is straight
isStraight :: Hand -> Bool
isStraight hand =
    let
        handRanks = sort $ ranks hand
    in
        length hand == 5 && (handRanks == [Two, Three, Four, Five, Ace] ||
        handRanks == [Ten, Jack, Queen, King, Ace] || isAscending hand)

-- Function to check if a hand is a high card
isHighCard :: Hand -> Bool
isHighCard hand = not (null hand) && not (isStraight hand) && length (uniqueRanks hand) == length hand

-- Function to check if a hand is flush
isFlush :: Hand -> Bool
isFlush hand = length (uniqueSuites hand) == 1 && length hand == 5

-- Function to check if a hand is royal (equal to ranks 10, J, Q, K, A)
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
counterRank hand = [(r, numOccurrencesRank hand r) | r <- [Two ..]]

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

-- Function to check if an item is less than or equal to another item
isLessThanStarting :: Ord a => [a] -> a -> Bool
isLessThanStarting [] _ = True
isLessThanStarting (x:_) y = y <= x

-- Function to remove duplicates from a list
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates xs = toList $ fromList xs

-- Function to get all possible combinations of r items from n >= r items
combinations :: Ord a => [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs k = removeDuplicates [x : y | x <- xs, y <- combinations (xs \\ [x]) (k-1), isLessThanStarting y x]

-- -- -- Main function for Exercise 5
-- highestScoringHand :: [Card] -> Hand
-- highestScoringHand [] = []
-- highestScoringHand xs =
--     let
--         outputLength = min 5 (length xs)
--         allCombinations = combinations xs outputLength
--         maxScore = maximum $ map scoreHand allCombinations
--         maxHands = filter (\c -> scoreHand c == maxScore) allCombinations
--         choice = head maxHands
--     in case bestHandType choice of
--         HighCard ->
--             let largest = maximum $ map maximum maxHands
--             in head $ filter (\h -> maximum h == largest) maxHands
--         _ -> choice

-- List of all of the suits
allSuits :: [Suit]
allSuits = [(minBound :: Suit) ..]

-- Function to check if a list contains a sublist in any order
containsList :: Eq a => [a] -> [a] -> Bool
containsList sublist xs = and [x `elem` xs | x <- sublist]

-- Function that returns a royal flush of a given suit
royalFlushSuit :: Suit -> [Card]
royalFlushSuit s = [Card Ace s, Card King s, Card Queen s, Card Jack s, Card Ten s]

-- Function that returns the best royal flush hand and score if exist, and nothing otherwise
bestRoyalFlush :: [Card] -> Maybe (Hand, Int)
bestRoyalFlush hand = 
    let poss = [sample | s <- allSuits, let sample = royalFlushSuit s, containsList sample hand]
    in case poss of
            [] -> Nothing
            xs -> let choice = head xs
                    in Just (choice, scoreHand choice)

-- Function to get straights from a list of cards
straightsFrom :: [Card] -> [Hand]
straightsFrom [] = []
straightsFrom (_:xs)
    | l < 5 = []
    | otherwise = let curr = take 5 xs 
        in if isStraight curr
            then curr : straightsFrom xs
            else straightsFrom xs
    where l = length xs

-- Function to get straight flushes from a list of cards
straightFlushesFrom :: [Card] -> [Hand]
straightFlushesFrom cards = concat [straightsFrom $ filter (\c -> suit c == s) cards | s <- allSuits]

-- Function to get the maximum key and its score from a list of keys and a scoring function
maxKey :: (Ord a, Ord b) => [a] -> (a -> b) -> (a, b)
maxKey xs f = swap $ maximum [(f x, x) | x <- xs]

-- Function that returns the best straight flush and score if exist, and nothing otherwise
bestStraightFlush :: [Card] -> Maybe (Hand, Int)
bestStraightFlush hand = 
    let poss = straightFlushesFrom (sort hand)
    in case poss of
        [] -> Nothing
        _ -> Just $ maxKey poss scoreHand

-- Function that gets the ranks with a count of at least a given amount
rankCountAtLeast :: [Card] -> Int -> [Rank]
rankCountAtLeast cards minCount = map fst $ filter (\(_, c) -> c >= minCount) $ counterRank cards

-- Function that gets the cards that have a rank that occur at least a given amount of times
cardsWithRankCountAtLeast :: [Card] -> Int -> [[Card]]
cardsWithRankCountAtLeast cards minCount = [take minCount (filter (\c -> rank c == currRank) cards) | currRank <- rankCountAtLeast cards minCount]

-- Function that gets the cards that have a rank that occur equal to a given amount of time
cardsWithRankCount :: [Card] -> Int -> [[Card]]
cardsWithRankCount cards count = [take count (filter (\c -> rank c == currRank) cards) | currRank <- rankWithCount cards count]

-- Function that returns the best N of a kind and score if exist, and nothing otherwise
bestNOfAKind :: [Card] -> Int -> Maybe (Hand, Int)
bestNOfAKind cards n = 
    case cardsWithRankCountAtLeast cards n of
        [] -> Nothing
        xs -> let choice = fst $ maxKey xs (rank . head)
                in Just (choice, scoreHand choice)

-- Function that returns the best full house and score if exist, and nothing otherwise
bestFullHouse :: [Card] -> Maybe (Hand, Int)
bestFullHouse cards =
    let
        triples = cardsWithRankCount cards 3
        pairs = cardsWithRankCount cards 2
    in case (triples, pairs) of
        (_:_, _:_) -> let 
            tripleChoice = fst $ maxKey triples (rank . head)
            pairChoice = fst $ maxKey pairs (rank . head)
            choice = tripleChoice ++ pairChoice
            in Just (choice, scoreHand choice)
        _ -> Nothing

-- Function that returns the best straights and score if exist, and nothing otherwise
bestStraights :: [Card] -> Maybe (Hand, Int)
bestStraights cards =
    case straightsFrom cards of
        [] -> Nothing
        xs -> let choice = fst $ maxKey xs scoreHand
                in Just (choice, scoreHand choice)

-- Function that returns the best two pairs and score if exist, and nothing otherwise
bestTwoPair :: [Card] -> Maybe (Hand, Int)
bestTwoPair cards
    | length poss < 2 = Nothing
    | otherwise = let 
        sorted = sortBy (comparing (rank . head)) poss
        choice = concat $ take 2 (reverse sorted)
                    in Just (choice, scoreHand choice)
    where poss = cardsWithRankCountAtLeast cards 2

bestHandAndScoreFrom :: [Card] -> (Hand, Int)
bestHandAndScoreFrom cards = 
    let
        outputLength = min 5 (length cards)
        allCombinations = combinations cards outputLength
    in
        maxKey allCombinations scoreHand

-- Function that returns the best flush and score if exist, and nothing otherwise
bestFlushes :: [Card] -> Maybe (Hand, Int)
bestFlushes cards =
    let poss = [bestHandAndScoreFrom xs | s <- allSuits, let xs = filter (\c -> suit c == s) cards, length xs >= 5]
    in case poss of
        [] -> Nothing
        _ -> Just (swap $ maximum (map swap poss))

-- Main Function for Exercise 5
highestScoringHand :: [Card] -> Hand
highestScoringHand [] = []
highestScoringHand hand = 
    let options = [bestRoyalFlush hand
                  ,bestStraightFlush hand
                  ,bestNOfAKind hand 4
                  ,bestFullHouse hand
                  ,bestFlushes hand
                  ,bestStraights hand
                  ,bestNOfAKind hand 3
                  ,bestTwoPair hand
                  ,bestNOfAKind hand 2
                  ,bestNOfAKind hand 1]
        actualOptions = catMaybes options
    in snd $ maximum (map swap actualOptions)

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

-- Main function for Exercise 6
simpleAI :: [Move] -> [Card] -> Move
simpleAI _ cards = Move Play $ take 5 $ reverse $ sort cards

-- Main function for Exercise 7
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ cards = Move Play $ highestScoringHand cards

-- Function to get best hand type from the hand of eight
bestHandTypeFromHand :: [Card] -> HandType
bestHandTypeFromHand hand = maximum $ map bestHandType (combinations hand 5)

-- Function to get list of ordered ranks from hand of eight
orderedRanks :: Hand -> [Rank]
orderedRanks hand = sort $ toList $ fromList $ ranks hand

-- Function to get number of consecutive elements above current position
numAbove :: [Rank] -> Int
numAbove [] = 0
numAbove (x:xs) = case xs of
    [] -> 0
    (y:_) -> case abs (fromEnum y - fromEnum x) of
                1 -> 1 + numAbove xs
                _ -> 0

-- Function to get number of consecutive elements above and below an index (inc. itself) in a list of ordered ranks
numAboveBelow :: Int -> [Rank] -> Int
numAboveBelow index sortedRanks = 
    let
        upper = drop index sortedRanks
        lower = reverse $ take (index+1) sortedRanks
    in
        numAbove upper + numAbove lower + 1

-- Function to get the number of consecutive ranks a card is part of in a hand of eight
numConsecutive :: Hand -> Card -> Int
numConsecutive hand card =
    let
        sortedRanks = orderedRanks hand
        cardRank = rank card
        maybeIndex = elemIndex cardRank sortedRanks
    in case maybeIndex of
        Nothing -> 0
        (Just i) -> numAboveBelow i sortedRanks

-- Function to evaluate each card, the greater the value the better it is
evaluateCard :: Hand -> Card -> Float
evaluateCard hand card =
    let
        countRankComponent = 2 * fromIntegral (numOccurrencesRank hand (rank card))
        countSuitComponent =  6 * fromIntegral (numOccurrencesSuit hand (suit card))
        cardValue = 0.25 * fromIntegral (fromEnum $ rank card)
        consecutiveComponent = 1 * fromIntegral (numConsecutive hand card)
    in
        countSuitComponent + countRankComponent + cardValue + consecutiveComponent

-- Function to check if a move is a discard
isDiscard :: Move -> Bool
isDiscard move = case move of
    Move Play _ -> False
    Move Discard _ -> True

-- Function to check for the number of discards in the move history
numDiscards :: [Move] -> Int
numDiscards moveHistory = 3 - length (filter isDiscard moveHistory)

-- Main function for Exercise 8
myAI :: [Move] -> [Card] -> Move
myAI moveHistory cards
    | numDiscards moveHistory > 0 && bestScore < 200 = Move Discard $ take 5 (sortBy (comparing $ evaluateCard cards) cards)
    | otherwise = Move Play $ highestScoringHand cards
    where bestScore = scoreHand $ highestScoringHand cards
-- myAI = sensibleAI