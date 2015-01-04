import Data.List
import Data.List.Split
import Data.Maybe

-- types
data Resource = Sheep | Wheat | Ore | Brick | Wood deriving (Show, Enum)
data Tile = Grass | Plains | Mountains | Clay | Forest | Desert deriving (Show, Enum)

type Board = [[Tile]]


type Index = Int
type MaxIndex = Int

type BoardSquare = (Index,Index)
type Intersection = (Index,Index)
type Robber = BoardSquare

-- func

tile2Resource :: Tile -> Maybe Resource
tile2Resource Grass = Just Sheep
tile2Resource Plains = Just Wheat
tile2Resource Mountains = Just Ore
tile2Resource Clay = Just Brick
tile2Resource Forest = Just Wood
tile2Resource Desert = Nothing

tileQuantities :: Tile -> Int
tileQuantities Desert = 1 
tileQuantities Clay = 3
tileQuantities Mountains = 4 -- change
tileQuantities _ = 4

allTiles :: [Tile]
allTiles = concatMap (\tile -> replicate (tileQuantities tile) tile) [Grass ..]

placeTiles :: [Tile] -> Board
placeTiles tiles = chunk 5 tiles

createBoard :: Board
createBoard = placeTiles $ allTiles

ix2Tile :: MaxIndex -> Index -> [Index]
ix2Tile _ 0 = [0]
ix2Tile a b 
	| a == b = [a]
	| otherwise = [b - 1, b]

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

valueAt :: Index -> [a] -> Maybe a
valueAt x list 
	| x >= length list = Nothing
	| otherwise = Just $ list !! x


valueAt2 :: Board -> BoardSquare -> Maybe Tile
valueAt2 board (x,y) = do
	row <- valueAt y board
	valueAt x row

claimResource :: Board -> Robber -> BoardSquare -> Maybe Resource
claimResource board robber square 
	| robber == square = Nothing
	| otherwise = do 
		tile <- valueAt2 board square
		tile2Resource tile

getResourcesForIntersection :: Intersection -> Board -> Robber -> [Resource]
getResourcesForIntersection (x,y) board robber =
	let 
		-- [BoardSquare]
		boardSquares = cartProd (ix2Tile 5 x) (ix2Tile 4 y)
		-- [Resource]
		resources = catMaybes $ map (claimResource board robber) boardSquares
	in
		resources





main :: IO () 

main = 
	let
		board = createBoard
		resources = getResourcesForIntersection (4,3) board (3,2)
	in do
		print $ resources
		