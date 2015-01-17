module PData where

data World = World { human :: Paddle, comp :: Paddle, ball :: Ball} 
	
data Paddle = Paddle { 	id :: Int,
			pos :: Position} 
paddleWidth = 7

instance Show Paddle where
	show _ = replicate paddleWidth '█'

data Position = Position {coord :: Coords, dir :: Direction } deriving Show

data Coords = Coords { x :: Int, y :: Int } deriving Show

data Direction = Up | Down | Left | Right | None deriving (Show, Eq)

data Ball = Ball { bpos :: Position } 
instance Show Ball where 
	show _ = "●"
