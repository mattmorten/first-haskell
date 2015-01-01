import Data.List

data Suit = C | S | D | H deriving (Eq, Show, Ord)
data Value = Num Int | Jack | Queen | King | Ace deriving (Eq, Show, Ord)
type Card = (Value,Suit)
type Hand = [Card] 

-- Equality
valueEquals :: Card -> Card -> Bool
valueEquals (a, _) (b, _) = a == b

suitEquals :: Card -> Card -> Bool
suitEquals (_, a) (_, b) = a == b

-- Successor
isSuccessor :: Card -> Card -> Bool
isSuccessor (Ace,_) ((Num 2),_) = True
isSuccessor ((Num 10),_) (Jack,_) = True
isSuccessor (Jack,_) (Queen,_) = True
isSuccessor (Queen,_) (King,_) = True
isSuccessor (King,_) (Ace,_) = True
isSuccessor ((Num a), _) ((Num b),_) = a == (b - 1)
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

scores :: [(Hand -> Maybe Hand)]
scores = [pair, three, four, flush, straight]

main :: IO () 

main = 
	let
		hand = sort ([(Jack,H), ((Num 5),D), ((Num 6),H), ((Num 5),H), (Jack,C)])
	in do
		print $ valueEquals (Jack, H) (Jack, D)
		print $ isSuccessor (Jack, H) (King, D)
		print $ map (\x -> x (hand)) scores
		print $ multipleKind 2 isSuccessor hand



