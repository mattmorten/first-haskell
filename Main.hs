import Data.List
import Data.List.Split
import Data.Maybe

-- types
data Resource = Sheep | Wheat | Ore | Brick | Wood deriving (Show, Enum, Eq)
data Tile = Grass | Plains | Mountains | Clay | Forest | Desert deriving (Show, Enum)

data Piece = Settlement | City | DevelopmentCard | RoadPiece deriving (Show, Eq)
data Color = Red | White | Blue | Orange deriving (Show, Eq, Enum)

data CardType = Knight | Monopoly | Plenty | Victory deriving (Show, Eq)

data PlacedPiece = Placed Piece Color deriving (Show, Eq)

type Board = [[Tile]]
type Coins = [[Coin]]
type Pieces = [[Maybe PlacedPiece]]

type Hand = [Resource]

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
pieceValue _ = 0

cardValue :: CardType -> Int
cardValue Victory = 1
cardValue _ = 0

purchaseOptions :: [Piece]
purchaseOptions = [RoadPiece, DevelopmentCard, Settlement, City]

costOf :: Piece -> [Resource]
costOf Settlement = [Wheat, Brick, Sheep, Wood]
costOf City = [Ore, Ore, Ore, Wheat, Wheat]
costOf RoadPiece = [Wood, Brick]
costOf DevelopmentCard = [Wheat, Ore, Sheep] 


allTiles :: [Tile]
allTiles = concatMap (\tile -> replicate (tileQuantities tile) tile) [Grass ..]

allCoins :: [Coin]
allCoins = concatMap (\coin -> replicate (coinQuantities coin) coin) [2 .. 12]

placeTiles :: [Tile] -> Board
placeTiles tiles = chunksOf 5 tiles

placeCoins :: [Coin] -> Coins
placeCoins coins = chunksOf 5 coins

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

pieceAt :: Pieces -> Intersection -> Maybe PlacedPiece
pieceAt pieces (x,y) = do
	row <- valueAt y pieces
	piece <- valueAt x row
	piece

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


handContains :: Hand -> [Resource] -> Bool
handContains hand [] = True
handContains hand (x:xs) 
	| x `elem` hand = handContains (delete x hand) xs
	| otherwise = False 

handOptions :: Hand -> [Piece]
handOptions hand = filter (\piece -> handContains hand (costOf piece)) purchaseOptions

scorePlayer :: [Piece] -> [CardType] -> Int
scorePlayer pieces cards =
	(foldl (+) 0 $ map pieceValue pieces) + 
	(foldl (+) 0 $ map cardValue cards)

piecesPlayed :: Pieces -> Color -> [Piece]
piecesPlayed pieces color = concatMap (\row -> 
			let 
				rowJust = catMaybes row
				justColor = filter (\(Placed _ c) -> c == color) rowJust
			in
				map (\(Placed p _) -> p) justColor
			) pieces


main :: IO () 

main = 
	let
		board = createBoard
		coins = createCoins
		resources = getResourcesForIntersection board coins (3,2) 10 City (4,3)
		pieces = [[(Just  (Placed City Blue)), Nothing],
				  [Nothing, (Just (Placed Settlement Blue))],
				  [Nothing, (Just (Placed City Red))]]
	in do
		print $ piecesPlayed pieces Orange

		