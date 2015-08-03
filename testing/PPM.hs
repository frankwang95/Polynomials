module PPM where

import System.Environment
import System.IO
import qualified Data.Vector.Unboxed as V

data PPM = PPM {pixels :: V.Vector (Int, Int, Int) , height :: Int, width :: Int, cap :: Int}
	deriving (Show)

createBlank :: Int -> Int -> PPM
createBlank h w = PPM pix h w 255
	where pix = V.replicate (w * h) (0, 0, 0)

writeTup :: (Int, Int, Int) -> Handle -> IO ()
writeTup (r,g,b) h = hPutStrLn h $ (show r) ++ " " ++ (show g) ++ " " ++ (show b)

writeRow :: V.Vector (Int, Int, Int) -> Handle -> IO ()
writeRow v h = helper v h 0
	where helper v h n
			| n < V.length v = do
				writeTup (v V.! n) h
				helper v h $ n + 1
			| otherwise = return ()

writeImage :: PPM -> Handle -> IO ()
writeImage img h = do
	hPutStrLn h "P3"
	hPutStrLn h $ (show (height img)) ++ " " ++ (show (width img))
	hPutStrLn h $ show $ cap img
	writeRow (pixels img) h 