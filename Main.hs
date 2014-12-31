import Data.List

data Suit = C | S | D | H deriving (Eq, Show, Ord)
data Value = Num Int | Ace | Jack | Queen | King deriving (Eq, Show, Ord)
type Card = (Value,Suit)
type Hand = [Card] 

groupByValue :: Hand -> Hand -> [[Card]] -> [[Card]]
groupByValue [] curr result = curr : result
groupByValue ((x1,y1) : []) curr result = ((x1,y1) : curr) : result
groupByValue ((x1,y1) : (x2,y2) : xs) curr result
	| x1 == x2 = groupByValue ((x2,y2) : xs) ((x1,y1) : curr) result
	| otherwise = groupByValue ((x2,y2) : xs) [] (((x1,y1) : curr) : result)

groupValues :: Hand -> [[Card]]
groupValues x = sort (groupByValue (sort x) [] []) 

multipleValue :: Int -> Hand -> Maybe Hand
multipleValue c x = 
	     let 
             groups = groupValues x
             thePair = filter (\n -> length n == c) groups
		 in case thePair of
             [] -> Nothing
             y -> Just $ head y


pair :: Hand -> Maybe Hand
pair = multipleValue 2

three :: Hand -> Maybe Hand
three = multipleValue 3

four :: Hand -> Maybe Hand
four = multipleValue 4

scores :: [(Hand -> Maybe Hand)]
scores = [pair, three, four]

main :: IO () 
main = print $ map (\n -> n [(Jack,H), ((Num 5),D), ((Num 7),C), ((Num 5),H)]) scores