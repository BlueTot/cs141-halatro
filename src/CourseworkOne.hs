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
numOccurrences :: (Eq a) => (Card -> a) -> a -> Hand -> Int
numOccurrences f target = length . filter (\c -> f c == target)

-- Constant to store all the ranks
allRanks :: [Rank]
allRanks = [(minBound :: Rank) ..]

-- Constant to store all the suits
allSuites :: [Suit]
allSuites = [(minBound :: Suit) ..]

-- Function to count the occurrences of each rank in a hand
countRanksBy :: (Int -> Bool) -> Hand -> [Int]
countRanksBy f hand = filter f [numOccurrences rank r hand | r <- allRanks]

{-Check if there are at least k instances of a given occurrence or higher
    in the list of occurrences of a hand -}
enoughRankCount :: Hand -> Int -> Int -> Bool
enoughRankCount hand count target = length (countRanksBy (>= count) hand) == target

-- Function to remove duplicates from a list
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = Set.toList . Set.fromList

-- Function to get ranks from a hand
ranks :: Hand -> [Rank]
ranks = map rank

-- Function to get suits from a hand
suits :: Hand -> [Suit]
suits = map suit

-- Function to get unique suites from a hand
uniqueSuites :: Hand -> [Suit]
uniqueSuites = removeDuplicates . suits

-- Function to get unique ranks from a hand
uniqueRanks :: Hand -> [Rank]
uniqueRanks = removeDuplicates . ranks

-- Function to check if a hand is ascending
isAscending :: Hand -> Bool
isAscending hand =
  let scores = List.sort $ map fromEnum $ ranks hand
   in and [b - a == 1 | (a, b) <- zip scores (tail scores)]

-- Function to check if a hand is straight
isStraight :: Hand -> Bool
isStraight hand =
  let handRanks = List.sort $ ranks hand
   in length hand == 5 && (handRanks == [Two, Three, Four, Five, Ace] || isAscending hand)

-- Function to check if a hand is flush
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
bestHandTypeFrom :: Hand -> [HandType] -> HandType
bestHandTypeFrom _ [] = None
bestHandTypeFrom hand (x : xs) = if contains hand x then x else bestHandTypeFrom hand xs

-- Main function for Exericse 2
bestHandType :: Hand -> HandType
bestHandType hand = bestHandTypeFrom hand $ reverse [(minBound :: HandType) ..]

--------------------------------------------------------------------------------
-- Part 3: score a played hand

-- Function to count number of times each rank occurs in the hand
counterRank :: Hand -> [(Rank, Int)]
counterRank hand = [(r, numOccurrences rank r hand) | r <- allRanks]

-- Function to get the ranks with a certain count in the hand
rankWithCount :: Hand -> Int -> [Rank]
rankWithCount hand count = map fst $ filter (\(_, c) -> c == count) $ counterRank hand

-- Function to get the ranks with at least a certain count in the hand
rankWithCountAtLeast :: Hand -> Int -> [Rank]
rankWithCountAtLeast hand count = map fst $ filter (\(_, c) -> c >= count) $ counterRank hand

-- Function to get the score of the highest rank in the hand
highestRankScore :: Hand -> Int
highestRankScore hand = maximum (map rankScore (ranks hand))

-- Main function for Exercise 3
whichCardsScore :: Hand -> [Card]
whichCardsScore hand =
  let pairs = rankWithCount hand 2
      triplets = rankWithCount hand 3
      quads = rankWithCountAtLeast hand 4
   in case bestHandType hand of
        None -> []
        HighCard -> [maximum hand]
        Pair -> filter (\(Card r _) -> r `elem` pairs) hand
        TwoPair -> filter (\(Card r _) -> r `elem` pairs) hand
        ThreeOfAKind -> filter (\(Card r _) -> r `elem` triplets) hand
        Straight -> hand
        Flush -> hand
        FullHouse -> hand
        FourOfAKind -> take 4 $ filter (\(Card r _) -> r `elem` quads) hand
        StraightFlush -> hand
        RoyalFlush -> hand

-- Main function for Exercise 4
scoreHand :: Hand -> Int
scoreHand hand =
  let scoreCards = whichCardsScore hand
      best = bestHandType hand
      base = fst (handTypeValues best)
      mult = snd (handTypeValues best)
      cardVals = sum (map (rankScore . rank) scoreCards)
   in (base + cardVals) * mult

--------------------------------------------------------------------------------
-- Part 4: find the highest scoring hand of 5 cards out of n>=5 cards

{- Original combinations method for Exercise 5, now only used to testing. -}

-- Function to check if an item is less than or equal to another item
isLessThanStarting :: (Ord a) => [a] -> a -> Bool
isLessThanStarting [] _ = True
isLessThanStarting (x : _) y = y <= x

-- Function to get all possible combinations of r items from n >= r items
combinations :: (Ord a) => [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations xs k =
  removeDuplicates
    [ x : y | x <- xs, y <- combinations (xs List.\\ [x]) (k - 1), isLessThanStarting y x
    ]

-- Original function for Ex. 5, no longer used anymore, only used for testing.
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
containsList :: (Eq a) => [a] -> [a] -> Bool
containsList sublist xs = and [x `elem` xs | x <- sublist]

-- Function that returns a royal flush of a given suit
royalFlushSuit :: Suit -> [Card]
royalFlushSuit s = [Card Ace s, Card King s, Card Queen s, Card Jack s, Card Ten s]

-- Function that returns the best royal flush hand and score if exist, and nothing otherwise
bestRoyalFlush :: [Card] -> Maybe (Hand, Int)
bestRoyalFlush hand =
  let poss =
        [ sample | s <- allSuites, let sample = royalFlushSuit s, containsList sample hand
        ]
   in case poss of
        [] -> Nothing
        xs ->
          let choice = head xs
           in Just (choice, scoreHand choice)

-- Function to get straights from a list of cards
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
straightFlushesFrom :: [Card] -> [Hand]
straightFlushesFrom cards =
  concat [allStraightsFrom $ filter (\c -> suit c == s) cards | s <- allSuites]

-- Function to get the maximum key and its score from a list of keys and a scoring function
maxKey :: (Ord a, Ord b) => [a] -> (a -> b) -> (a, b)
maxKey xs f = Tuple.swap $ maximum [(f x, x) | x <- xs]

-- Function that returns the best straight flush and score if exist, and nothing otherwise
bestStraightFlush :: [Card] -> Maybe (Hand, Int)
bestStraightFlush hand =
  case straightFlushesFrom (List.sort hand) of
    [] -> Nothing
    xs -> Just $ maxKey xs scoreHand

-- Function that gets the ranks with a count of at least a given amount
rankCountAtLeast :: [Card] -> Int -> [Rank]
rankCountAtLeast cards minCount =
  map fst $
    filter (\(_, c) -> c >= minCount) $
      counterRank cards

-- Function that gets the cards that have a rank that occur at least a given amount of times
cardsWithRankCountAtLeast :: [Card] -> Int -> [[Card]]
cardsWithRankCountAtLeast cards minCount =
  [ take minCount (filter (\c -> rank c == currRank) cards)
    | currRank <- rankCountAtLeast cards minCount
  ]

-- Function that gets the cards that have a rank that occur equal to a given amount of time
cardsWithRankCount :: [Card] -> Int -> [[Card]]
cardsWithRankCount cards count =
  [ take count (filter (\c -> rank c == currRank) cards)
    | currRank <- rankWithCount cards count
  ]

-- Function that returns the best N of a kind and score if exist, and nothing otherwise
bestNOfAKind :: [Card] -> Int -> Maybe (Hand, Int)
bestNOfAKind cards n =
  case cardsWithRankCountAtLeast cards n of
    [] -> Nothing
    xs ->
      let choice = fst $ maxKey xs (rank . head)
       in Just (choice, scoreHand choice)

-- Function that returns the best full house and score if exist, and nothing otherwise
bestFullHouse :: [Card] -> Maybe (Hand, Int)
bestFullHouse cards =
  let triples = cardsWithRankCountAtLeast cards 3
      pairs = cardsWithRankCountAtLeast cards 2
   in case triples of
        [] -> Nothing
        _ ->
          let tripleChoice = fst $ maxKey triples (rank . head)
              validPairs = filter (\xs -> rank (head xs) /= rank (head tripleChoice)) pairs
           in case validPairs of
                [] -> Nothing
                _ ->
                  let pairChoice = fst $ maxKey validPairs (rank . head)
                      choice = tripleChoice ++ pairChoice
                   in Just (choice, scoreHand choice)

-- Function to remove duplicate ranked cards from a hand of cards
uniqueRankHand :: [Card] -> [Card]
uniqueRankHand cards = [head poss | r <- allRanks, let poss = filter (\c -> rank c == r) cards, not $ null poss]

-- Function that returns the best straights and score if exist, and nothing otherwise
bestStraights :: [Card] -> Maybe (Hand, Int)
bestStraights cards =
  case allStraightsFrom $ List.sort $ uniqueRankHand cards of
    [] -> Nothing
    xs ->
      let choice = fst $ maxKey xs scoreHand
       in Just (choice, scoreHand choice)

-- Function that returns the best two pairs and score if exist, and nothing otherwise
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
bestHandAndScoreFrom :: [Card] -> (Hand, Int)
bestHandAndScoreFrom cards =
  let outputLength = min 5 (length cards)
      allCombinations = combinations cards outputLength
   in maxKey allCombinations scoreHand

-- Function that returns the best flush and score if exist, and nothing otherwise
bestFlushes :: [Card] -> Maybe (Hand, Int)
bestFlushes cards =
  let poss =
        [ bestHandAndScoreFrom xs | s <- allSuites, let xs = filter (\c -> suit c == s) cards, length xs >= 5
        ]
   in case poss of
        [] -> Nothing
        _ -> Just (Tuple.swap $ maximum $ map Tuple.swap poss)

-- Main Function for Exercise 5
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
   in scoringCards ++ take (5 - length scoringCards) (List.sort $ hand List.\\ scoringCards)

--------------------------------------------------------------------------------
-- Part 5: implement an AI for maximising score across 3 hands and 3 discards

-- Main function for Exercise 6
simpleAI :: [Move] -> [Card] -> Move
simpleAI _ cards = Move Play $ take 5 $ List.sortBy (Ord.comparing Ord.Down) cards

-- Main function for Exercise 7
sensibleAI :: [Move] -> [Card] -> Move
sensibleAI _ cards = Move Play $ highestScoringHand cards

-- Function to get list of ordered ranks from hand of eight
orderedRanks :: Hand -> [Rank]
orderedRanks hand = List.sort $ Set.toList $ Set.fromList $ ranks hand

-- Function to get number of consecutive elements above current position
numAbove :: [Rank] -> Int
numAbove [] = 0
numAbove (x : xs) = case xs of
  [] -> 0
  (y : _) -> case abs (fromEnum y - fromEnum x) of
    1 -> 1 + numAbove xs
    _ -> 0

-- Function to get number of consecutive elements above and below an index (inc. itself) in a list of ordered ranks
numAboveBelow :: Int -> [Rank] -> Int
numAboveBelow index sortedRanks =
  let upper = drop index sortedRanks
      lower = reverse $ take (index + 1) sortedRanks
   in numAbove upper + numAbove lower + 1

-- Function to get the number of consecutive ranks a card is part of in a hand of eight
numConsecutive :: Hand -> Card -> Int
numConsecutive hand card =
  let sortedRanks = orderedRanks hand
      cardRank = rank card
      maybeIndex = List.elemIndex cardRank sortedRanks
   in case maybeIndex of
        Nothing -> 0
        (Just i) -> numAboveBelow i sortedRanks

-- Function to evaluate the overall strength of a suit
suitStrength :: Hand -> [Card] -> Card -> Float
suitStrength hand remainingCards card =
  let overallCardValue = fromIntegral $ sum (map (fromEnum . rank) $ filter (\c -> suit c == suit card) hand)
      overallRemSuit = fromIntegral $ length (filter (\c -> suit c == suit card) remainingCards)
      overallCons = fromIntegral $ sum (map (numConsecutive hand) hand)
   in 4 * (overallCardValue + overallRemSuit + overallCons + 2 * fromIntegral (fromEnum (suit card)))

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
isDiscard :: Move -> Bool
isDiscard move = case move of
  Move Play _ -> False
  Move Discard _ -> True

-- Function to check for the number of discards in the move history
numDiscards :: [Move] -> Int
numDiscards moveHistory = 3 - length (filter isDiscard moveHistory)

numPlays :: [Move] -> Int
numPlays moveHistory = 3 - length (filter (not . isDiscard) moveHistory)

-- Function to sort the hand of cards by the evaluation function
sortByEvaluation :: [Move] -> [Card] -> [Card]
sortByEvaluation moveHistory cards =
  let remainingCards = originalDeck List.\\ usedCards moveHistory
   in List.sortBy (Ord.comparing $ evaluateCard cards remainingCards) cards

-- Function to get the maximum partial-flush length in the hand
maxPartialFlush :: [Card] -> Int
maxPartialFlush cards = maximum [numOccurrences suit s cards | s <- allSuites]

-- Function to get the maximum partial straight length in the hand
maxPartialStraight :: [Card] -> Int
maxPartialStraight cards = maximum $ map (numConsecutive cards) cards

-- Function to determine how many cards to discard
numToDiscard :: [Card] -> Int
numToDiscard cards
  | flush >= 4 = 8 - flush
  | straight >= 4 = 8 - straight
  | otherwise = 5
  where
    flush = maxPartialFlush cards
    straight = maxPartialStraight cards

-- Function to get used cards from the move history
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
myAI :: [Move] -> [Card] -> Move
myAI moveHistory cards
  | plays == 1 && discards > 0 = 
    if best == FourOfAKind
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