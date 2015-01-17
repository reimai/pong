module Main where

import System.Timeout
import System.Console.ANSI
import System.Console.Terminal.Size
import Data.Maybe
import Control.Monad
import System.IO
import PData as P
import Utils

main = do 
	hSetBuffering stdin NoBuffering --get input immedietly 
	hSetEcho stdin False		--don't show the typed character
	jwindow <- size
	let window = fromJust jwindow
	let pd = initPaddle 1 ((width window - paddleWidth) `div` 2) (height window - 1)
	let comp = initPaddle 2 ((width window - paddleWidth) `div` 2) (0)
	let ball = Ball $ Position (Coords 0 0) None 
	gameLoop stdin window $ World pd comp ball 

gameLoop :: Handle -> Window Int -> World -> IO()
gameLoop h wnd world = do 
			renderWorld h wnd world 
			jch <- timeout (floor(1/fps * 10^6)) (hGetChar h)
			let ch = fromMaybe ' ' jch
			when (ch /= 'q') $ gameLoop h wnd $ moveWorld ch wnd world 
				where fps = 60

moveWorld :: Char -> Window Int -> World -> World		        	
moveWorld ch wnd (World pd comp ball) = World (newPd ch) (newPd $ compTurn ch) ball
				where newPd ch = movePd wnd pd $ parseDir ch

renderWorld :: Handle -> Window Int -> World -> IO ()
renderWorld h wnd (World pd comp ball) = do
			clearScreen
			putStrLn $ render wnd comp 
			--putStrLn $ replicate (y (bpos ball)) '\n'
			--putStrLn $ render wnd ball
			putStrLn $ replicate (height wnd - 5) '\n' --wtf is 5?
			putStrLn $ render wnd pd

compTurn ch = ch

movePd :: Window Int -> Paddle -> Direction -> Paddle
movePd _ pd None = pd
movePd wnd pd dir = fitWindow wnd $ Paddle (P.id pd) $ movePos (pos pd) dir $ getGap pd

getGap :: Paddle -> Int
getGap pd 	| (dir $ pos pd) == None = 2
		| otherwise = 4

fitWindow :: Window Int -> Paddle -> Paddle
fitWindow wnd pd = fitY wnd $ fitX wnd pd
	where
		fitX wnd pd 	| pdx pd < 0 = Paddle (P.id pd) $ Position (Coords 0 $ pdy pd) None
				| pdx pd + paddleWidth > width wnd = Paddle (P.id pd) $ Position (Coords (width wnd - paddleWidth) $ pdy pd) None
	   			| otherwise = pd
 
		fitY wnd pd 	| pdy pd < 0 = Paddle (P.id pd) $ Position (Coords (pdx pd) 0) None
				| pdy pd  + 1 > height wnd = Paddle (P.id pd) $ Position (Coords (pdx pd) (height wnd - 1)) None
				| otherwise = pd
--a quick way to get coords
pdy :: Paddle -> Int
pdy pd = y $ coord $ pos pd 
pdx :: Paddle -> Int
pdx pd = x $ coord $ pos pd 

movePos :: Position -> Direction -> Int -> Position 
movePos pos dir gap = Position (move (coord pos) dir gap) dir

move :: Coords -> Direction -> Int -> Coords
move crd P.Up gap = Coords (x crd) (y crd - gap)
move crd P.Down gap = Coords (x crd) (y crd + gap)
move crd P.Left gap = Coords (x crd - gap) (y crd)
move crd P.Right gap = Coords (x crd + gap) (y crd)
move crd P.None _ = crd

parseDir :: Char -> Direction
parseDir 'D' = P.Left 				
parseDir 'A' = P.Up 				
parseDir 'C' = P.Right 				
parseDir 'B' = P.Down
parseDir _ = P.None

initPaddle :: Int -> Int -> Int -> Paddle
initPaddle id x y = Paddle id $ Position (Coords x y) $ None 

render :: Window Int -> Paddle -> String
render wnd pd	| (width wnd) < paddleWidth = error "that's a wee window"
	  	| otherwise = replicate (pdx pd) ' ' ++ show pd


