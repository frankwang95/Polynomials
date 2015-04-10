module Main where

import System.Environment
import System.IO
import Data.Char
import Control.Applicative as A
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Attoparsec.ByteString.Char8 as  P
import Text.ParserCombinators.ReadP as R

import PPM


----- PARSING -----
-------------------

data Complex = Complex Double Double
	deriving (Show)

instance Read Complex where
	readsPrec _ = readP_to_S parseAll

parseX :: ReadP String
parseX = string ("x->")

parseI :: ReadP Bool
parseI = fmap (const True) (string "*I") <++ pure False

parseNeg :: ReadP Bool
parseNeg = fmap (const True) (char '-') <++ fmap (const False) (char '+') <++ pure False

parseEnd :: ReadP String
parseEnd = fmap (const "0.0") eof

parseNumber :: ReadP Double
parseNumber = do
		a <- parseNeg
		b <- parseEnd <++ munch (\x -> (x == '.') || isDigit x)
		if a then return $ -read (b ++ "0")
		else return $ read $ b ++ "0"

parseComplex :: ReadP Complex
parseComplex = do
	a <- parseNumber
	b <- parseI
	if b then return (Complex 0 a)
	else return (Complex a 0)

parseAll ::ReadP Complex
parseAll = do
	parseX
	a <- parseComplex
	b <- parseComplex
	return $ complexSum a b

complexSum :: Complex -> Complex -> Complex
complexSum (Complex a b) ( Complex c d) = Complex (a + c) (b + d)


-- DATA FORMATTING --

split :: String -> [String]
split ss = helper ss []
	where
		helper [] t = [t]
		helper (s:ss) t
			| elem s ['\n', ','] = t : helper ss []
			| otherwise = helper ss (t ++ [s])

dataFilter :: String -> String
dataFilter [] = []
dataFilter (x:xs)
	| elem x ['{','}', ' '] = dataFilter xs
	| otherwise = x : dataFilter xs

dataFormat :: String -> [Complex]
dataFormat str = map read $ filter (/= "") $ map dataFilter $ split str


-- IMAGE PROCESSING --

left = (-4.0)
right = 4.0
top = 3.0
bottom = (-3.0)
vertPix = 400 -- Piexl ratios must adhere to coordinate ratios
horzPix = 300

convCoord :: (Int, Int) -> Int
convCoord (h, v) = v * horzPix + h

computePixel :: Double -> Double -> Double -> Int -> Int
computePixel leftDown rightUp cord nPix
	| cord < leftDown || cord > rightUp = (-1)
	| otherwise = floor $ (cord - leftDown) / pixLength
		where pixLength = (rightUp - leftDown) / fromIntegral nPix 

convComplex :: Complex -> (Int, Int)
convComplex (Complex r i) = (h, v)
	where
		h = computePixel bottom top r vertPix
		v = computePixel left right i horzPix

incrTup :: (Int, Int, Int) -> (Int, Int, Int)
incrTup (a, b, c) = (a + 1, b + 1, c + 1)

incr mv i = do
	item <- M.read mv i
	M.write mv i (incrTup item)

genVec :: [Complex] -> V.Vector (Int, Int, Int)
genVec xs = runST $ do
	mv <- M.replicate (vertPix * horzPix) (0, 0, 0)
	mapM_ (incr mv) $ map (convCoord . convComplex) xs
	V.freeze mv

genImage :: V.Vector (Int, Int, Int) -> PPM
genImage v = PPM v vertPix horzPix $ (\(x, y, z) -> x) $ V.maximum v


-- MAIN --

main = do
	-- Import / Parse
	rawData <- readFile "data"
	let formatedData = dataFormat rawData
	putStrLn $ show $ formatedData

	{-
	-- Create Image
	let ppm = genImage $ genVec $ formatedData

	-- Write Image
	h <- openFile "test.ppm" WriteMode
	writeImage ppm h
	hClose h -}