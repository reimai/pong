module Main where

import System.Timeout
import System.Console.ANSI
import qualified System.Console.Terminal.Size as T
import System.Console.Terminal.Size (Window)
import Data.Maybe
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Control.Monad
import Control.Concurrent
import System.IO
import PData as P
import Utils
import Control.Concurrent
import System.CPUTime
import Control.Exception

main = do
	hSetBuffering stdin NoBuffering --get input immedietly
	hSetBuffering stdout NoBuffering 
	hSetEcho stdin False            --don't show the typed character
	jwindow <- T.size
	let window = fromJust jwindow
	let center = ((T.width window - pdWidth) `div` 2)
	let pd = initPaddle 1 center (T.height window - 2) --the lowest line of screen is taken by input
	let comp = initPaddle 2 center (0)
	let ball = Ball $ Position (Coords center 5) $ Direction HLeft VDown 
	gameLoop stdin window $ World pd comp ball

gameLoop :: Handle -> Window Int -> World -> IO()
gameLoop h wnd world = do
			renderWorld wnd world
			e <- threadDelay (floor(1/fps * 10^6))  
			ch <- readAll h ' '
			when (ch /= 'q') $ gameLoop h wnd $ moveWorld ch wnd world
				where fps = 20

--reads all input from Handle, returns only the last char
readAll :: Handle -> Char -> IO(Char)
readAll h ch = hReady h >>= \gotIt -> if' gotIt (hGetChar h >>= readAll h) $ return ch

moveWorld :: Char -> Window Int -> World -> World
moveWorld ch wnd (World pd comp ball) = World newHuman newComp newBall 
				where 	newHuman = movePd wnd pd $ parseDir ch 
					newComp = movePd wnd comp $ compTurn wnd comp ball
				  	newBall = moveBall pd comp wnd ball

renderWorld :: Window Int -> World -> IO ()
renderWorld wnd (World pd comp ball) = do
			clearScreen
			putStr $ render wnd comp-- ++ " " ++ show (pos comp)
			putStr $ replicate (getY ball) '\n'
			putStr $ render wnd ball-- ++ " " ++ show (pos ball)
			putStr $ replicate (getY pd - getY ball) '\n'
			putStrLn $ render wnd pd-- ++ " " ++ show (pos pd)

compTurn :: Window Int -> Paddle -> Ball -> Direction 
compTurn wnd comp ball 	| caught comp futureBall = dNone 
		   	| otherwise = if' (getX futureBall < getX comp) (Direction HLeft VNone) (Direction HRight VNone)  
				where futureBall = Ball $ Position (move (back $ getDir futureBall0) 1 (coord.pos $ futureBall0)) $ getDir futureBall0 				 
					where futureBall0 = predictBall wnd ball				 

--ball position on the upper (0) row if human player catch it
predictBall :: Window Int -> Ball -> Ball
predictBall wnd ball =  getFin.turnUp.getFin $ ball
			where 	turnUp ball = newBall (getX ball) (getY ball) $ collide (getDir ball) (Direction HNone VDown)
				getFin ball = flip $ move $ correct ball
					where 	correct ball 	| isUp $ getDir ball 	= newBall (getX ball) ((getY ball) - (highGlass wnd)) $ getDir ball
				 		 		| isDown $ getDir ball	= newBall (getX ball) ((getY ball) + (highGlass wnd)) $ getDir ball
								| otherwise = ball

						move ball 	| isLeft $ getDir ball 	= newBall ((getX ball) - (distY ball)) (moveY ball) $ getDir ball
							 	| isRight $ getDir ball	= newBall ((getX ball) + (distY ball)) (moveY ball) $ getDir ball
								| otherwise = ball
							where 	distY ball	| isUp $ getDir ball 	= getY ball
										| otherwise 		= T.height wnd - 1 - (getY ball)								
								moveY ball	| isUp $ getDir ball 	= 0
										| otherwise 		= T.height wnd - 1 
					
						flip ball	| getX ball <= 0 	   	= newBall (-(getX ball)) (getY ball) $ collide (getDir ball) (Direction HLeft VNone)
								| getX ball >= T.width wnd - 1 	= newBall (2 * (T.width wnd) - (getX ball)) (getY ball) $ collide (getDir ball) (Direction HRight VNone) 
								| otherwise = ball
								

newBall :: Int -> Int -> Direction -> Ball
newBall x y dir = Ball $ Position (Coords x y) dir 

--skip a long way through high glass - correct y coord by N window widths if it is < height
highGlass :: Window Int -> Int
highGlass wnd = T.height wnd `div` (T.width wnd) * (T.width wnd)   	

movePd :: Window Int -> Paddle -> Direction -> Paddle
movePd _ pd (Direction HNone VNone) = pd
movePd wnd pd dir = fitWindow wnd $ Paddle (P.id pd) $ movePos dir (getGap pd) (pos pd)

moveBall :: Paddle -> Paddle -> Window Int -> Ball -> Ball
moveBall pd comp wnd ball	| getY newBall == getY comp + 1 && not (caught comp newBall) = stop newBall
			        | getY newBall == getY pd - 1 && not (caught pd newBall) = stop newBall
			        | otherwise = newBall
			        	where newBall = Ball $ (movePos ((collision ball $ findCollision pd comp wnd ball)) ballSpeed (pos ball))

collision :: Ball -> Direction -> Direction
collision ball (Direction HNone VNone) = dir.pos $ ball
collision ball obst = collide (dir.pos $ ball) obst

findCollision :: Paddle -> Paddle -> Window Int -> Ball -> Direction
findCollision pd comp wnd ball 	| getX ball == 0 		= Direction HLeft VNone
				| getX ball == T.width wnd - 1 	= Direction HRight VNone
				| getY ball == getY comp + 1 	= Direction HNone VUp
				| getY ball == getY pd - 1	= Direction HNone VDown
				| otherwise 			= dNone
ballSpeed :: Int
ballSpeed = 1

caught :: Paddle -> Ball -> Bool
caught pd ball = x >= pdX - 1 && x <= pdX + pdWidth + 1
	where 	x = getX ball
		pdX = getX pd

stop :: Ball -> Ball
stop ball = Ball $ Position (coord.pos $ ball) dNone

--some fake inertia
getGap :: Paddle -> Int
getGap pd 	| (dir $ pos pd) == dNone = 2
		| otherwise = 4

fitWindow :: Window Int -> Paddle -> Paddle
fitWindow wnd pd = fitY wnd $ fitX wnd pd
	where
		-- 1 is not a bug - it's a feature - or the game could last forever
		fitX wnd pd 	| getX pd < 1 = Paddle (P.id pd) $ Position (Coords 1 $ getY pd) dNone
				| getX pd + width pd > T.width wnd = Paddle (P.id pd) $ Position (Coords (T.width wnd - width pd) $ getY pd) dNone
	   			| otherwise = pd

		fitY wnd pd 	| getY pd < 0 = Paddle (P.id pd) $ Position (Coords (getX pd) 0) dNone
				| getY pd  + 2 > T.height wnd = Paddle (P.id pd) $ Position (Coords (getX pd) (T.height wnd - 2)) dNone
				| otherwise = pd

--a quick way to get coords
getY :: Renderable a => a -> Int
getY r = y.coord.pos $ r
getX :: Renderable a => a -> Int
getX r = x.coord.pos $ r
getDir :: Renderable a => a -> Direction
getDir r = dir.pos $ r


parseDir :: Char -> Direction
parseDir 'D' = Direction HLeft VNone
--parseDir 'A' = up
parseDir 'C' = Direction HRight VNone
--parseDir 'B' = down
parseDir _ = dNone

initPaddle :: Int -> Int -> Int -> Paddle
initPaddle id x y = Paddle id $ Position (Coords x y) $ dNone


