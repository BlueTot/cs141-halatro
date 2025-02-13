module CourseworkOne where

import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Ord qualified as Ord
import Data.Set qualified as Set
import Data.Tuple qualified as Tuple
import Halatro.Constants
import Halatro.Types

--------------------------------------------------------------------------------
-- Part 1: check whether a played hand is a certain hand type

-- Function to count the number of occurrences of a given rank/suit in a hand
-- Filter for rank/suit using the function f, and take the length.
numOccurrences :: (Eq a) => (Card -> a) -> a -> Hand -> Int
numOccurrences f target = length . filter (\c -> f c == target)

-- Constant to store all the ranks
-- Rank is bounded so we can use ENUM syntax starting from minBound
allRanks :: [Rank]
allRanks = [(minBound :: Rank) ..]

-- Constant to store all the suites
-- Suit is bounded so we can use ENUM syntax starting from minBound
allSuites :: [Suit]
allSuites = [(minBound :: Suit) ..]

-- Function to count the occurrences of each rank in a hand
-- get all counts of each possible rank in the hand, and filter against a function f
countRanksBy :: (Int -> Bool) -> Hand -> [Int]
countRanksBy f hand = filter f [numOccurrences rank r hand | r <- allRanks]

{-Check if there are at least k instances of a given occurrence or higher
    in the list of occurrences of a hand -}
-- We filter by (at least count or more) and check if the length is equal to the target
enoughRankCount :: Hand -> Int -> Int -> Bool
enoughRankCount hand count target = length (countRanksBy (>= count) hand) == target

-- Function to remove duplicates from a list
-- Turn the list into a set, and back.
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = Set.toList . Set.fromList

-- Function to get ranks from a hand
-- Map the rank function over the input (eta-reduced)
ranks :: Hand -> [Rank]
ranks = map rank

-- Function to get suits from a hand
-- Map the suit function over the input (eta-reduced)
suits :: Hand -> [Suit]
suits = map suit

-- Function to get unique suites from a hand
-- Compose suits function with remove duplicates
uniqueSuites :: Hand -> [Suit]
uniqueSuites = removeDuplicates . suits

-- Function to get unique ranks from a hand
-- Compose ranks function with remove duplicates
uniqueRanks :: Hand -> [Rank]
uniqueRanks = removeDuplicates . ranks

-- Function to check if a hand is ascending
{- We check if the absolute difference between each enum value and the next is 1, 
   for all values in the list. -}
isAscending :: Hand -> Bool
isAscending hand =
  let scores = List.sort $ map fromEnum $ ranks hand
   in and [b - a == 1 | (a, b) <- zip scores (tail scores)]

-- Function to check if a hand is straight
-- A hand is straight if it is ascending or it is [A,2,3,4,5] - the edge case.
isStraight :: Hand -> Bool
isStraight hand =
  let handRanks = List.sort $ ranks hand
   in length hand == 5 && (handRanks == [Two, Three, Four, Five, Ace] || isAscending hand)

-- Function to check if a hand is flush
-- A hand is flush if the hand is of size 5, and there is only one type of suit.
isFlush :: Hand -> Bool
isFlush hand = length (uniqueSuites hand) == 1 && length hand == 5

-- Main function for Exercise 1
contains :: Hand -> HandType -> Bool
contains hand handType
  {- Main checking -}
  | handType == RoyalFlush && isRoyal hand && isFlush hand
      || handType == StraightFlush && isStraight hand && isFlush hand
      || handType == FourOfAKind && enoughRankCount hand 4 1
      || handType == FullHouse && isFullHouse hand
      || handType == Flush && isFlush hand
      || handType == Straight && isStraight hand
      || handType == ThreeOfAKind && enoughRankCount hand 3 1
      || handType == TwoPair && enoughRankCount hand 2 2
      || handType == Pair && enoughRankCount hand 2 1
      || handType == HighCard && isHighCard hand
      || handType == None && null hand =
      True
  | otherwise = False
  where
    {- Local functions used in Exercise 1 only -}

    -- Function to check if a hand is full house
    -- Check if we have exactly a 2 & 3 count in the rank count list
    isFullHouse :: Hand -> Bool
    isFullHouse cards = List.sort (countRanksBy (> 0) cards) == [2, 3]

    -- Function to check if a hand is royal (equal to ranks 10, J, Q, K, A)
    isRoyal :: Hand -> Bool
    isRoyal cards = List.sort (ranks cards) == [Ten, Jack, Queen, King, Ace]

    -- Function to check if a hand is a high card
    isHighCard :: Hand -> Bool
    isHighCard cards =
      not (null cards)
        && not (isStraight cards) -- the hand is not empty
        && length (uniqueRanks cards) == length cards -- the hand is not straight
        && not (isFlush cards) -- there are no duplicate ranks
        -- the hand is not a flush

--------------------------------------------------------------------------------
-- Part 2: identify the highest value hand type in a played hand

-- Function to get the best hand type from a list of possible hand types
{- A recursive function that checks if we have a given hand type in a list of 
  possible hand types from left to right, stopping when we find the first match -}
bestHandTypeFrom :: Hand -> [HandType] -> HandType
bestHandTypeFrom _ [] = None
bestHandTypeFrom hand (x : xs) = if contains hand x then x else bestHandTypeFrom hand xs

-- Main function for Exericse 2
{- We reverse the hand types (so we check the best one first), 
   and use the recursive function defined above -}
bestHandType :: Hand -> HandType
bestHandType hand = bestHandTypeFrom hand $ reverse [(minBound :: HandType) ..]

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Function to count number of times each rank occurs in the hand
-- Produce a list of pairs, first value is the rank, second value is the number of occurrences.
counterRank :: Hand -> [(Rank, Int)]
counterRank hand = [(r, numOccurrences rank r hand) | r <- allRanks]

-- Function to get the ranks with a certain count in the hand
{- Use the counterRank function and filter for a given count, 
  and take the first value of all pairs. -}
rankWithCount :: Hand -> Int -> [Rank]
rankWithCount hand count = map fst $ filter (\(_, c) -> c == count) $ counterRank hand

-- Function to get the ranks with at least a certain count in the hand
{- Use the counterRank function and filter for a given count or greater, 
  and take the first value of all pairs. -}
rankWithCountAtLeast :: Hand -> Int -> [Rank]
rankWithCountAtLeast hand count = map fst $ filter (\(_, c) -> c >= count) $ counterRank hand

-- Main function for Exercise 3
whichCardsScore :: Hand -> [Card]
whichCardsScore hand =
  let pairs = rankWithCount hand 2
      triplets = rankWithCount hand 3
      {- Here we have to check for rank count of 4 or higher, because of a custom
      test I made for Ex. 5. Sometimes the test case generates a hand with 5 of the same rank,
      which is impossible in the game but occurs in the test case. -}
      quads = rankWithCountAtLeast hand 4
   in case bestHandType hand of
        None -> []
        HighCard -> [maximum hand] -- get the highest card
        Pair -> filter (\(Card r _) -> r `elem` pairs) hand -- find all pairs
        TwoPair -> filter (\(Card r _) -> r `elem` pairs) hand -- find all pairs
        ThreeOfAKind -> filter (\(Card r _) -> r `elem` triplets) hand -- find all triplets
        Straight -> hand -- already 5 cards, so give it back
        Flush -> hand -- same
        FullHouse -> hand -- same
        FourOfAKind -> take 4 $ filter (\(Card r _) -> r `elem` quads) hand -- find all quads
        StraightFlush -> hand -- same
        RoyalFlush -> hand -- same

-- Main function for Exercise 4
scoreHand :: Hand -> Int
scoreHand hand =
  let scoreCards = whichCardsScore hand
      best = bestHandType hand -- get the best hand type
      base = fst (handTypeValues best) -- get the base score
      mult = snd (handTypeValues best) -- get the multiplier
      cardVals = sum (map (rankScore . rank) scoreCards) -- sum up the score values of each card
   in (base + cardVals) * mult -- final calculation

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

{- Original combinations method for Exercise 5, now only used to testing.
  The only functions that are in use are isLessThanStarting, and combinations, 
  which are used once in the new Exercise 5 code.  -}

-- Function to check if an item is less than or equal to another item
isLessThanStarting :: (Ord a) => [a] -> a -> Bool
isLessThanStarting [] _ = True
isLessThanStarting (x : _) y = y <= x

-- Function to get all possible combinations of r items from n >= r items
-- Base case: r = 0 -> we return [[]]
{- Recursive case: we build up the combinations in ascending order, accounting
   for duplicates in the original list with <=, and removing extra duplicates at the end -}
combinations :: (Ord a) => [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs k =
  removeDuplicates
    [ x : y | x <- xs, 
      y <- combinations (xs List.\\ [x]) (k - 1), 
      isLessThanStarting y x ]

-- Original function for Ex. 5, no longer used anymore, only used for testing.
-- This is used in the "correctness" test in Ex. 5, which runs 100k times.
-- This function is notably slower than the new function due to the extensive use of combinations.
highestScoringHand' :: [Card] -> Hand
highestScoringHand' [] = []
highestScoringHand' xs =
  let outputLength = min 5 (length xs)
      allCombinations = combinations xs outputLength
      maxScore = maximum $ map scoreHand allCombinations
      maxHands = filter (\c -> scoreHand c == maxScore) allCombinations
      choice = head maxHands
   in case bestHandType choice of
        HighCard ->
          let largest = maximum $ map maximum maxHands
           in head $ filter (\h -> maximum h == largest) maxHands
        _ -> choice

{- New method for Exercise 5, tested against the original method in the correctness test -}

-- Function to check if a list contains a sublist in any order
-- Check if x is in xs for every x in the sublist.
containsList :: (Eq a) => [a] -> [a] -> Bool
containsList sublist xs = and [x `elem` xs | x <- sublist]

-- Function that returns a royal flush of a given suit
royalFlushSuit :: Suit -> [Card]
royalFlushSuit s = [Card Ace s, Card King s, Card Queen s, Card Jack s, Card Ten s]

-- Function that returns the best royal flush hand and score if exist, and nothing otherwise
bestRoyalFlush :: [Card] -> Maybe (Hand, Int)
bestRoyalFlush hand =
  -- calculate all 4 royal flushes possible.
  let poss = [sample | s <- allSuites, 
              let sample = royalFlushSuit s, 
              containsList sample hand]
   in case poss of
        [] -> Nothing
        xs ->
          let choice = head xs
           in Just (choice, scoreHand choice)

-- Function to get straights from a list of cards
-- Base case: if the list length is less than 5, we return []
{- Recursive case: we do a sliding window on the next set of 5, 
  and add to the list if it is straight -}
straightsFrom :: [Card] -> [Hand]
straightsFrom [] = []
straightsFrom xs
  | l < 5 = []
  | otherwise =
      let curr = take 5 xs
       in if isStraight curr
            then curr : straightsFrom (tail xs)
            else straightsFrom (tail xs)
  where
    l = length xs

-- Constant for the special straight hand that needs to be considered
specialStraight :: [Rank]
specialStraight = [Two, Three, Four, Five, Ace]

-- Function to get all straights from a list of cards
-- Function that wraps around the straightsFrom function to handle the [A,2,3,4,5] case as well.
allStraightsFrom :: [Card] -> [Hand]
allStraightsFrom cards =
  ( if containsList specialStraight cardRanks
      then uniqueRankHand $ filter (\c -> rank c `elem` specialStraight) cards
      else []
  )
    : straightsFrom cards
  where
    cardRanks = uniqueRanks cards

-- Function to get straight flushes from a list of cards
-- Divide by flush and calculate straights.
straightFlushesFrom :: [Card] -> [Hand]
straightFlushesFrom cards =
  concat [allStraightsFrom $ filter (\c -> suit c == s) cards | s <- allSuites]

-- Function to get the maximum key and its score from a list of keys and a scoring function
-- Put the scoring value first in the tuple, sort by the tuple and swap them back.
maxKey :: (Ord a, Ord b) => [a] -> (a -> b) -> (a, b)
maxKey xs f = Tuple.swap $ maximum [(f x, x) | x <- xs]

-- Function that returns the best straight flush and score if exist, and nothing otherwise
-- Calculate all straight flushes using sliding window, and score them and take the best one.
bestStraightFlush :: [Card] -> Maybe (Hand, Int)
bestStraightFlush hand =
  case straightFlushesFrom (List.sort hand) of
    [] -> Nothing
    xs -> Just $ maxKey xs scoreHand

-- Function that gets the ranks with a count of at least a given amount
rankCountAtLeast :: [Card] -> Int -> [Rank]
rankCountAtLeast cards minCount =
  map fst $ -- and take the first value
  filter (\(_, c) -> c >= minCount) $ -- filter for at least a given count
  counterRank cards -- get counter of the card ranks

-- Function that gets the cards that have a rank that occur at least a given amount of times
-- Use the rankCountAtLeast function and filter for each rank in the list generated.
cardsWithRankCountAtLeast :: [Card] -> Int -> [[Card]]
cardsWithRankCountAtLeast cards minCount =
  [ take minCount (filter (\c -> rank c == currRank) cards)
    | currRank <- rankCountAtLeast cards minCount
  ]

-- Function that gets the cards that have a rank that occur equal to a given amount of time
-- Same function as above, but for an exact rank count.
cardsWithRankCount :: [Card] -> Int -> [[Card]]
cardsWithRankCount cards count =
  [ take count (filter (\c -> rank c == currRank) cards)
    | currRank <- rankWithCount cards count
  ]

-- Function that returns the best N of a kind and score if exist, and nothing otherwise
-- Generate all possible >=N counts in the list, score them and take the best one
-- Used for High Card, Pair, Three of a Kind, Four of a Kind
bestNOfAKind :: [Card] -> Int -> Maybe (Hand, Int)
bestNOfAKind cards n =
  case cardsWithRankCountAtLeast cards n of
    [] -> Nothing
    xs ->
      let choice = fst $ maxKey xs (rank . head)
       in Just (choice, scoreHand choice)

-- Function that returns the best full house and score if exist, and nothing otherwise
-- Generates all triples and pairs, and finds all possible full houses.
bestFullHouse :: [Card] -> Maybe (Hand, Int)
bestFullHouse cards =
  let triples = cardsWithRankCountAtLeast cards 3
      pairs = cardsWithRankCountAtLeast cards 2
   in case triples of
        [] -> Nothing -- if there are no triples, we have no full houses.
        _ ->
          let 
            -- get the best triple
            tripleChoice = fst $ maxKey triples (rank . head)
            {- get all valid pairs, which is where the rank of a pair is different to the 
               rank of the best triple. This is used to prevent using the same pair and triple
               in the full house. -}
            validPairs = filter (\xs -> rank (head xs) /= rank (head tripleChoice)) pairs
           in case validPairs of
                [] -> Nothing -- if we have no valid pairs, we have no full houses.
                _ ->
                  let pairChoice = fst $ maxKey validPairs (rank . head)
                      choice = tripleChoice ++ pairChoice -- choose the best pair and combine them.
                   in Just (choice, scoreHand choice)

-- Function to remove duplicate ranked cards from a hand of cards
uniqueRankHand :: [Card] -> [Card]
uniqueRankHand cards = [head poss | r <- allRanks, -- iterate through all ranks, and take the head.
                        let poss = filter (\c -> rank c == r) cards, -- filter for a given rank
                        not $ null poss] -- non-empty

-- Function that returns the best straights and score if exist, and nothing otherwise
-- Sort the unique ranks, and use the allStraightsFrom function to get the best straights.
-- Then get the best one if it exists, and return it.
bestStraights :: [Card] -> Maybe (Hand, Int)
bestStraights cards =
  case allStraightsFrom $ List.sort $ uniqueRankHand cards of
    [] -> Nothing
    xs ->
      let choice = fst $ maxKey xs scoreHand
       in Just (choice, scoreHand choice)

-- Function that returns the best two pairs and score if exist, and nothing otherwise
-- If we have less than 2 possible candidates, we do not have a two pair.
-- Otherwise we choose the two highest pairs and return them.
bestTwoPair :: [Card] -> Maybe (Hand, Int)
bestTwoPair cards
  | length poss < 2 = Nothing
  | otherwise =
      let sorted = List.sortBy (Ord.comparing (rank . head)) poss
          choice = concat $ take 2 (reverse sorted)
       in Just (choice, scoreHand choice)
  where
    poss = cardsWithRankCountAtLeast cards 2

-- Function that returns the best hand and score from a hand of cards
-- The only place where combinations is used in the new code.
-- We use scoreHand to get the best combination and its score.
bestHandAndScoreFrom :: [Card] -> (Hand, Int)
bestHandAndScoreFrom cards =
  let outputLength = min 5 (length cards)
      allCombinations = combinations cards outputLength
   in maxKey allCombinations scoreHand

-- Function that returns the best flush and score if exist, and nothing otherwise
{- Divide by the suit, and use bestHandAndScoreFrom to calculate this.
   I know there is a better method, which is to get the best 5 cards from the set of cards with same suit.
   But this code was already written so I might as well use it. This does not harm the complexity too much
   because it is rare that there are more than 5 cards with the same suit in a hand. -}
bestFlushes :: [Card] -> Maybe (Hand, Int)
bestFlushes cards =
  let poss =
        [ bestHandAndScoreFrom xs | s <- allSuites, -- iterate through all suits
          let xs = filter (\c -> suit c == s) cards,  -- filter for a given suit
          length xs >= 5 ] -- check if we have at least five cards
   in case poss of
        [] -> Nothing
        _ -> Just (Tuple.swap $ maximum $ map Tuple.swap poss) -- use same technique

-- Main Function for Exercise 5
{- FINALLY, this function calculates the highest scoring hand in the hand of 8.
   We calculate the best hands (if exist) for each hand type and find the best overall.
   This is done because a lower hand type may score better than a higher hand type. -}
highestScoringHand :: [Card] -> Hand
highestScoringHand [] = []
highestScoringHand hand =
  let options =
        [ bestRoyalFlush hand,
          bestStraightFlush hand,
          bestNOfAKind hand 4,
          bestFullHouse hand,
          bestFlushes hand,
          bestStraights hand,
          bestNOfAKind hand 3,
          bestTwoPair hand,
          bestNOfAKind hand 2,
          bestNOfAKind hand 1
        ]
      actualOptions = Maybe.catMaybes options
      scoringCards = snd $ maximum (map Tuple.swap actualOptions)
  -- this last line is used to make the sensibleAI score 400.
  -- we sort the remaining cards and take the lowest ones to fill up the hand of 5.
  -- this is not in the coursework spec but might as well score 400 to match the halatro binary.
   in scoringCards ++ take (5 - length scoringCards) (List.sort $ hand List.\\ scoringCards)

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

-- Main function for Exercise 6
-- We sort the cards in reverse order and take the 5 best ones.
simpleAI :: [Move] -> [Card] -> Move
simpleAI _ cards = Move Play $ take 5 $ List.sortBy (Ord.comparing Ord.Down) cards

-- Main function for Exercise 7
-- We use Exercise 5 to get the highest scoring hand and play it.
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ cards = Move Play $ highestScoringHand cards

{- Exercise 8 commences here. 
   OUTLINE OF AI STRATEGY

   - this AI is a mixed strategy build, i.e. it does not focus on one thing entirely.
   - If we have a straight or above, we just play it. the only exception is when we have 
     1 play left and still have discards, so we want to keep the good cards and discard to 
     see if we can get anything better.
   - Otherwise we want to discard. what we discard determines on what we have in the hand currently.
   - The discarding strategy follows this hierarchy:
      - If we have a 3 of a kind, we keep it and play for a 4 of a kind or full house.
      - If we have a partial flush of 4, we keep it and play for a flush.
      - If we have a partial straight of 4, we keep it and play for a straight. 
      - If we have a partial flush of 3, we keep it and play for a flush. 
      - Otherwise, we try to keep cards of the same suit to play for flushes, so i.e. a 2+1 suit combo. 
      - The EVAL function is responsible for choosing the best quality cards to play for.
  
   - SCORING: please read!
    - According to my testing, this AI scores 744 over 100k runs. Please run this AI a lot of times
      to stabilise the score.
-}

-- Function to get list of ordered ranks from hand of eight
-- Remove duplicates from the ranks and sort them
orderedRanks :: Hand -> [Rank]
orderedRanks hand = List.sort $ removeDuplicates $ ranks hand

-- Function to get number of consecutive elements above current position
-- Recursive function that continues if the absolute difference in the enum values is 1.
numAbove :: [Rank] -> Int
numAbove [] = 0
numAbove (x : xs) = case xs of
  [] -> 0
  (y : _) -> case abs (fromEnum y - fromEnum x) of
    1 -> 1 + numAbove xs
    _ -> 0

{- Function to get number of consecutive elements above and below an index (inc. itself)
 in a list of ordered ranks -}
{- To calculate upper, we remove the first `index` elements of the list.
   To calculate below, we take the first `index+1` elements of the list. the +1 
   is used to make sure the middle value index is still included, and then we reverse it.
   Apply numAbove for both and sum them up, + 1 to account for the middle value. -}
numAboveBelow :: Int -> [Rank] -> Int
numAboveBelow index sortedRanks =
  let upper = drop index sortedRanks
      lower = reverse $ take (index + 1) sortedRanks
   in numAbove upper + numAbove lower + 1

-- Function to get the number of consecutive ranks a card is part of in a hand of eight
{- We get the index of the card in the hand, and use numAboveBelow to get 
   the number of consecutive values. -}
numConsecutive :: Hand -> Card -> Int
numConsecutive hand card =
  let sortedRanks = orderedRanks hand
      cardRank = rank card
      maybeIndex = List.elemIndex cardRank sortedRanks
   in case maybeIndex of
        Nothing -> 0 -- this should not occur, because the card is in the list.
        (Just i) -> numAboveBelow i sortedRanks

-- Function to evaluate the overall strength of a suit
{- This function depends on the sum of the card values, the number of remaining suits in the deck,
   and the number of consecutive neighbours each card has. The purpose of this function is to 
   group the cards by suit when sorted by evaluation, and to also act as a tie-breaker when
   two groups of suits have the same number of cards in it. -}
suitStrength :: Hand -> [Card] -> Card -> Float
suitStrength hand remainingCards card =
  let 
    -- sum of card values
    -- filter for same suit, convert to enum and sum up.
    overallCardValue = fromIntegral $ sum (map (fromEnum . rank) $ 
                        filter (\c -> suit c == suit card) hand)
    -- number of remaining in the deck
    -- filter for suit in the deck, and take the length
    overallRemSuit = fromIntegral $ length (filter (\c -> suit c == suit card) remainingCards)
    -- consecutives
    -- sum up numConsecutive value for each card. 
    overallCons = fromIntegral $ sum (map (numConsecutive hand) hand)
   {-The weighting for each component is as follows:
      CARD VALUE = 1
      REMAINING SUIT = 1
      CONSECTUVIE COMPONENT = 1
      SUIT NUMBER = 2
    The purpose of the suit number is to break a tie in the case 
    all three components sum to the same value, so we get a grouping of suits no matter what -}
   in 4 * (overallCardValue + overallRemSuit + overallCons + 2 * fromIntegral (fromEnum (suit card)))

{- Below are piecewise functions to weight the number of occurrence of suit, rank 
   and consecutive component in the evaluation function. 
   
   The reason why piecewise functions were chosen, is because not all amounts are equal.
   E.g. three of same rank is a lot stronger than two of same rank. if we used a polynomial or 
   exponential to calculate it, this would be slow. so we precompute the values and store them here. -}

-- Piecewise function for suit evaluation calculation
suitFormula :: Int -> Int
suitFormula 0 = 0
suitFormula 2 = 11
suitFormula 3 = 50
suitFormula 4 = 287
suitFormula 5 = 500
suitFormula _ = 0

-- Piecewise function for straight evaluation calculation
consFormula :: Int -> Int
consFormula 0 = 0
consFormula 1 = 0
consFormula 2 = 0
consFormula 3 = 6
consFormula 4 = 134
consFormula 5 = 400
consFormula _ = 0

-- Piecewise function for rank evaluation calculation
rankFormula :: Int -> Int
rankFormula 0 = 0
rankFormula 1 = 0
rankFormula 2 = 4
rankFormula 3 = 500
rankFormula 4 = 700
rankFormula _ = 0

{- EVALUATION FUNCTION - the most important function in Ex. 8 
    The point of this function is to evaluate each card, so we can sort them
    and take the N worst ones to discard. N is determined by what we have in the hand.
    In this code, N is either 3, 4 or 5. we try to discard 5 if possible, but 3 or 4 if we have
    something very good already. 

    The components used are as follows:
      - number of occurrence of suit
      - number of occurrence of rank
      - number of consecutive neighbours
      - suit strength component (defined above)
      - card value
    
    - We weight the first three using the piecewise functions
    - Card value is given a weight of 1.5
    - suit strength is given a weight of 4, see above.

    - we calculate the evaluation by summing up these weighted components.
-}

-- Function to evaluate each card, the greater the value the better it is
evaluateCard :: Hand -> [Card] -> Card -> Float
evaluateCard hand remainingCards card =
  let -- initial variables we calculate
      numSuits = numOccurrences suit (suit card) hand
      numCons = numConsecutive hand card
      numRanks = numOccurrences rank (rank card) hand

      -- the actual things we use
      countSuitComponent = fromIntegral $ suitFormula numSuits
      countRankComponent = fromIntegral $ rankFormula numRanks
      suitStrengthComponent = suitStrength hand remainingCards card
      cardValue = 1.5 * fromIntegral (fromEnum $ rank card)
      consecutiveComponent = fromIntegral $ consFormula numCons
   in countSuitComponent
        + cardValue
        + consecutiveComponent
        + suitStrengthComponent
        + countRankComponent

-- Function to check if a move is a discard
-- Pattern match on the play or discard constructors.
isDiscard :: Move -> Bool
isDiscard move = case move of
  Move Play _ -> False
  Move Discard _ -> True

-- Function to check for the number of discards in the move history
numDiscards :: [Move] -> Int
numDiscards moveHistory = 3 - length (filter isDiscard moveHistory)

-- Function to check for the number of plays in the move history.
numPlays :: [Move] -> Int
numPlays moveHistory = 3 - length (filter (not . isDiscard) moveHistory)

-- Function to sort the hand of cards by the evaluation function
{- First we calculate the number of remaining cards in the deck
   And then we pass this to the evaluateCard function, and use List.sortBy to sort the list using
   the evaluation function -}
sortByEvaluation :: [Move] -> [Card] -> [Card]
sortByEvaluation moveHistory cards =
  let remainingCards = originalDeck List.\\ usedCards moveHistory
   in List.sortBy (Ord.comparing $ evaluateCard cards remainingCards) cards

-- Function to get the maximum partial-flush length in the hand
-- find the largest value of numoccurrences for each card
maxPartialFlush :: [Card] -> Int
maxPartialFlush cards = maximum [numOccurrences suit s cards | s <- allSuites]

-- Function to get the maximum partial straight length in the hand
-- find the largest value of numconsecutive for each card
maxPartialStraight :: [Card] -> Int
maxPartialStraight cards = maximum $ map (numConsecutive cards) cards

-- Function to determine how many cards to discard
{- This is an important function! 
    The purpose of this function is to make sure we do not discard good cards.
    The two main cases are if we have a partial flush of 4 or above, we DO NOT discard 5 cards.
    Same goes for partial straight of 4 or above, we DO NOT discard 5 cards
    The reason why we do >= 4 is because 5 may occur, and there is a case where if we have a flush, 
      we may want to use a discard.
-}
numToDiscard :: [Card] -> Int
numToDiscard cards
  | flush >= 4 = 8 - flush
  | straight >= 4 = 8 - straight
  | otherwise = 5
  where
    flush = maxPartialFlush cards
    straight = maxPartialStraight cards

-- Function to get used cards from the move history
-- We concatenate all lists of cards from the move history to get the used cards
usedCards :: [Move] -> [Card]
usedCards = concatMap getUsedCards
  where
    getUsedCards :: Move -> [Card]
    getUsedCards move = case move of
      Move Play cards -> cards
      Move Discard cards -> cards

-- Constant to store the original deck of cards
originalDeck :: [Card]
originalDeck = [Card r s | r <- allRanks, s <- allSuites]

-- Main function for Exercise 8
{- The main function for the AI! 
    The main strategy this AI uses is:
    - if we have 1 play left and we have discards, we will try to discard cards that are bad, to see if we
      can get anything better. we take care in NOT discarding the good cards we currently have. 
    - else if we have discards and we don't have a straight or above, we discard N cards, where
      N is determined by the numToDiscard function above.
    - otherwise, we use Exercise 5 to find the highest scoring hand we can play, 
      get the scoring cards and fill up the hand with bad cards so we can get rid of bad cards 
      whilst we are doing a Move Play.
-}
myAI :: [Move] -> [Card] -> Move
myAI moveHistory cards
  | plays == 1 && discards > 0 = 
    if best == FourOfAKind -- four of a kind is maximal, so we can just play it.
      then Move Play bestHand
      else Move Discard $ take (numToDiscard cards) $ sortByEvaluation moveHistory cards
  | discards > 0 && best < Straight =
      Move Discard $ take (numToDiscard cards) $ sortByEvaluation moveHistory cards
  | otherwise =
      let choice = whichCardsScore $ bestHand
          orderedCards = sortByEvaluation moveHistory (cards List.\\ choice)
       in Move Play (choice ++ take (5 - length choice) orderedCards)
  where
    discards = numDiscards moveHistory
    plays = numPlays moveHistory
    bestHand = highestScoringHand cards
    best = bestHandType bestHand

-- myAI = sensibleAI