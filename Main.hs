import Data.List
import Data.List.Split
import Data.Maybe

-- types
data Resource = Sheep | Wheat | Ore | Brick | Wood deriving (Show, Enum)
data Tile = Grass | Plains | Mountains | Clay | Forest | Desert deriving (Show, Enum)

data Piece = Settlement | City

type Board = [[Tile]]
type Coins = [[Coin]]
type Pieces = [[Piece]]

type Road = (Intersection, Intersection)

type Index = Int
type MaxIndex = Int

type BoardSquare = (Index,Index)
type Intersection = (Index,Index)
type Robber = BoardSquare

type Rolled = Int

type Coin = Int

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

coinQuantities :: Coin -> Int
coinQuantities 2 = 1
coinQuantities 12 = 1
coinQuantities 7 = 0
coinQuantities _ = 2

pieceValue :: Piece -> Int
pieceValue Settlement = 1
pieceValue City = 2

allTiles :: [Tile]
allTiles = concatMap (\tile -> replicate (tileQuantities tile) tile) [Grass ..]

allCoins :: [Coin]
allCoins = concatMap (\coin -> replicate (coinQuantities coin) coin) [2 .. 12]

placeTiles :: [Tile] -> Board
placeTiles tiles = chunk 5 tiles

placeCoins :: [Coin] -> Coins
placeCoins coins = chunk 5 coins

createBoard :: Board
createBoard = placeTiles $ allTiles

createCoins :: Coins
createCoins = placeCoins $ allCoins

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

tileAt :: Board -> BoardSquare -> Maybe Tile
tileAt board (x,y) = do
	row <- valueAt y board
	valueAt x row

coinAt :: Coins -> BoardSquare -> Maybe Coin
coinAt board (x,y) = do
	row <- valueAt y board
	valueAt x row

validIntersection :: Pieces -> Intersection -> Bool
validIntersection pieces (x,y) 
	| x < 0 || y < 0 = False
	| y >= (length pieces) = False
	| x >= (length (head pieces)) = False
	| otherwise = True

neighbourIntersections :: Pieces -> Intersection -> [Intersection]
neighbourIntersections pieces (x,y) =
	filter (validIntersection pieces) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

pieceAt :: Pieces -> Intersection -> Maybe Piece
pieceAt pieces (x,y) = do
	row <- valueAt y pieces
	valueAt x row

emptyIntersection :: Pieces -> Intersection -> Bool
emptyIntersection pieces intersection = case (pieceAt pieces intersection) of
	Nothing -> True
	_ -> False

pieceValid :: Pieces -> Intersection -> Bool
pieceValid pieces intersection = 
	isNothing (pieceAt pieces intersection) &&
	all (emptyIntersection pieces) (neighbourIntersections pieces intersection)


claimResources :: Board -> Coins -> Robber -> Rolled -> Piece -> BoardSquare -> [Resource]
claimResources board coins robber rolled piece square 
	| robber == square = []
	| otherwise = 
		let
			resource = do 
				tile <- tileAt board square
				coin <- coinAt coins square
				tile2Resource tile
		in
			case (resource, (coinAt coins square)) of
				((Just r), (Just c)) -> if (rolled == c) 
					then (replicate (pieceValue piece) r)
					else []
				_ -> []


getResourcesForIntersection :: Board -> Coins -> Robber -> Rolled -> Piece -> Intersection -> [Resource]
getResourcesForIntersection board coins robber rolled piece (x,y) =
	let 
		-- [BoardSquare]
		boardSquares = cartProd (ix2Tile 5 x) (ix2Tile 4 y)
		-- [Resource]
		resources =  concatMap (claimResources board coins robber rolled piece) boardSquares
	in
		resources





main :: IO () 

main = 
	let
		board = createBoard
		coins = createCoins
		resources = getResourcesForIntersection board coins (3,2) 10 City (4,3)
	in do
		print $ resources
		