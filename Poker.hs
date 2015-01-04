import Data.List

-- Types
data Suit = C | S | D | H deriving (Eq, Ord)
data Value = Val Int | Jack | Queen | King | Ace deriving (Eq, Ord)
data Win = Pair | TwoPair | Three | Straight | Flush | 
			FullHouse | Four | StraightFlush deriving (Eq, Ord, Show)
type Card = (Value,Suit)
type Hand = [Card] 


instance Show Suit where
	show C = "Clubs"
	show S = "Spades"
	show H = "Hearts"
	show D = "Diamonds"

instance Show Value where
	show (Val x) = show x
	show Jack = "Jack"
	show Queen = "Queen"
	show King = "King"
	show Ace = "Ace"


-- Equality
valueEquals :: Card -> Card -> Bool
valueEquals (a, _) (b, _) = a == b

suitEquals :: Card -> Card -> Bool
suitEquals (_, a) (_, b) = a == b

multipleKind :: Int -> (Card -> Card -> Bool) -> Win -> Hand -> Maybe Win
multipleKind number eqFn possibleWin x = 
	     let 
             groups = groupBy eqFn x
             thePair = filter (\n -> length n == number) groups
		 in case thePair of
             [] -> Nothing
             _ -> Just possibleWin


pair :: Hand -> Maybe Win
pair = multipleKind 2 valueEquals Pair

three :: Hand -> Maybe Win
three = multipleKind 3 valueEquals Three

four :: Hand -> Maybe Win
four = multipleKind 4 valueEquals Four

flush :: Hand -> Maybe Win
flush = multipleKind 5 suitEquals Flush

straight :: Hand -> Maybe Win
straight = multipleKind 5 (>) Straight

fullHouse :: Hand -> Maybe Win 
fullHouse x = 
	do
		_ <- pair x
		_ <- three x
		Just FullHouse

scores :: [(Hand -> Maybe Win)]
scores = [pair, three, four, flush, straight, fullHouse]

main :: IO () 

main = 
	let
		hand = sort ([(Jack,H), ((Val 5),D), ((Val 5),C), ((Val 5),H), (Queen,C)])
	in do
		print $ valueEquals (Jack, H) (Jack, D)
		print $ foldl (\prev element -> case element (hand) of 
			Just x -> Just x
			Nothing -> prev) Nothing scores



