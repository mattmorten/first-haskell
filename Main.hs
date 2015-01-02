import Data.List

-- Types
data Suit = C | S | D | H deriving (Eq, Ord)
data Value = Val Int | Jack | Queen | King | Ace deriving (Eq, Ord)
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

-- Successor
isSuccessor :: Card -> Card -> Bool
isSuccessor (Ace,_) ((Val 2),_) = True
isSuccessor ((Val 10),_) (Jack,_) = True
isSuccessor (Jack,_) (Queen,_) = True
isSuccessor (Queen,_) (King,_) = True
isSuccessor (King,_) (Ace,_) = True
isSuccessor ((Val a), _) ((Val b),_) = a == (b - 1)
isSuccessor _ _ = False

multipleKind :: Int -> (Card -> Card -> Bool) -> Hand -> Maybe Hand
multipleKind number eqFn x = 
	     let 
             groups = groupBy eqFn x
             thePair = filter (\n -> length n == number) groups
		 in case thePair of
             [] -> Nothing
             y -> Just $ head y


pair :: Hand -> Maybe Hand
pair = multipleKind 2 valueEquals

three :: Hand -> Maybe Hand
three = multipleKind 3 valueEquals

four :: Hand -> Maybe Hand
four = multipleKind 4 valueEquals

flush :: Hand -> Maybe Hand
flush = multipleKind 5 suitEquals

straight :: Hand -> Maybe Hand
straight = multipleKind 5 isSuccessor

fullHouse :: Hand -> Maybe Hand
fullHouse x = 
	do
		a <- pair x
		b <- three x
		Just (a ++ b)

scores :: [(Hand -> Maybe Hand)]
scores = [pair, three, four, flush, straight, fullHouse]

main :: IO () 

main = 
	let
		hand = sort ([(Jack,H), ((Val 5),D), ((Val 5),C), ((Val 5),H), (Jack,C)])
	in do
		print $ valueEquals (Jack, H) (Jack, D)
		print $ isSuccessor (Jack, H) (King, D)
		print $ map (\x -> x (hand)) scores
		print $ multipleKind 2 isSuccessor hand



