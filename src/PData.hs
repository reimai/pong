module PData where

import System.Console.ANSI
import qualified System.Console.Terminal.Size as T
import System.Console.Terminal.Size (Window)
import Utils

class (Show a) => Renderable a where
	width :: a -> Int
	pos :: a -> Position
	render :: Window Int -> a -> String
	render wnd rndbl   	| (T.width wnd) < width rndbl = error "that's a wee window"
                 		| otherwise = replicate (x.coord.pos $ rndbl) ' ' ++ show rndbl

data World = World { human :: Paddle, comp :: Paddle, ball :: Ball} 
	
data Paddle = Paddle { 	id :: Int,
			ppos :: Position} 

pdWidth :: Int
pdWidth = 9

instance Show Paddle where
	show p = replicate (width p) '█'

instance Renderable Paddle where
	width _ = pdWidth 
	pos = ppos


data Position = Position {coord :: Coords, dir :: Direction } 
instance Show Position where 
	show p = show (coord p) ++ " " ++ show (dir p)

movePos :: Direction -> Int -> Position -> Position
movePos dir gap pos = Position (move dir gap (coord pos)) dir 	


data Coords = Coords { x :: Int, y :: Int } 
instance Show Coords where
	show (Coords x y) = show x ++ ":" ++ show y


data VDirection = VUp | VDown | VNone deriving (Show, Eq)
data HDirection = HRight | HLeft | HNone deriving (Show, Eq)

data Direction = Direction {h :: HDirection, v :: VDirection} deriving (Show, Eq)
dNone :: Direction
dNone = Direction HNone VNone

move :: Direction -> Int -> Coords -> Coords
move (Direction h v) gap crd = Coords (moveH h gap crd) (moveV v gap crd)

moveV :: VDirection -> Int -> Coords -> Int
moveV VUp gap crd = y crd - gap
moveV VDown gap crd = y crd + gap
moveV VNone _ crd = y crd

moveH :: HDirection -> Int -> Coords -> Int
moveH HRight gap crd = x crd + gap 
moveH HLeft gap crd = x crd - gap
moveH HNone _ crd = x crd 

isUp :: Direction -> Bool
isUp (Direction _ VUp) = True
isUp _ = False

isDown :: Direction -> Bool
isDown (Direction _ VDown) = True
isDown _ = False

isRight :: Direction -> Bool
isRight (Direction HRight _) = True
isRight _ = False

isLeft :: Direction -> Bool
isLeft (Direction HLeft _) = True
isLeft _ = False

collide :: Direction -> Direction -> Direction
collide (Direction h v) (Direction obstH obstV) = Direction newH newV
		where	newV = if' (v == obstV) (backV v) v
			newH = if' (h == obstH) (backH h) h

back :: Direction -> Direction
back (Direction h v) = Direction (backH h) (backV v)

backV :: VDirection -> VDirection
backV VUp = VDown
backV VDown = VUp
backV VNone = VNone

backH :: HDirection -> HDirection
backH HRight = HLeft
backH HLeft = HRight
backH HNone = HNone

data Ball = Ball { bpos :: Position } 
instance Show Ball where 
	show _ = "●"

instance Renderable Ball where
	width _ = 1
	pos = bpos
