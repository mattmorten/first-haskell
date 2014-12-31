import Data.List

data Suit = C | S | D | H deriving (Eq, Show, Ord)
data Value = Num Int | Ace | Jack | Queen | King deriving (Eq, Show, Ord)
type Card = (Value,Suit)
type Hand = [Card] 

-- Equality
valueEquals :: Card -> Card -> Bool
valueEquals (a, _) (b, _) = a == b

suitEquals :: Card -> Card -> Bool
suitEquals (_, a) (_, b) = a == b

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

scores :: [(Hand -> Maybe Hand)]
scores = [pair, three, four, flush]

main :: IO () 

main = 
	let
		hand = sort ([(Jack,H), ((Num 5),D), ((Num 7),H), ((Num 5),H), (Jack,C)])
	in do
		print $ valueEquals (Jack, H) (Jack, D)
		print $ map (\x -> x (hand)) scores



