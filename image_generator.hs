module Main where

import System.Environment
import System.IO
import Data.Char
import Control.Applicative as A
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as  P
import qualified Text.ParserCombinators.ReadP as R

import PPM
import Data.Either

----- PARSING -----
-------------------
data Complex = Complex Double Double
	deriving (Show)

complexSum :: Complex -> Complex -> Complex
complexSum (Complex a b) ( Complex c d) = Complex (a + c) (b + d)

readComplex :: B.ByteString -> Either String Complex
readComplex = P.parseOnly parseComplex

-- Tokens
xT = B.pack "x->"
iT = B.pack "*I"

-- Parsers
parseX :: P.Parser B.ByteString
parseX = P.string xT

parseI :: P.Parser Bool
parseI = P.option False $ fmap (const True) $ P.string iT

parseNeg :: P.Parser Bool
parseNeg = P.option False $ fmap (const True) $ P.char '-'

parseAdd :: P.Parser Bool
parseAdd = P.option False $ fmap (const True) $ P.char '+'

parseSingle :: P.Parser Complex
parseSingle = P.option (Complex 0 0) $ do
	digits <- P.double
	i <- parseI
	if i
		then return $ Complex 0 digits
	else return $ Complex digits 0

parseComplex :: P.Parser Complex
parseComplex = do
	a <- parseSingle
	b <- parseSingle
	return $ complexSum a b


----- DATA FORMATTING -----
---------------------------
remSpace :: B.ByteString -> B.ByteString
remSpace = B.filter (\x -> x /= ' ' && x /= '\n')

split :: B.ByteString -> [B.ByteString]
split = B.splitWith (\x -> x == '{' || x == '}' || x == ',')

remTrash :: [B.ByteString] -> [B.ByteString]
remTrash = filter $ B.notElem 'x'

parseAll :: [B.ByteString] -> [Either String Complex]
parseAll = map readComplex

clean :: [Either String Complex] -> [Complex]
clean [] = []
clean ((Right x) : xs) = x : clean xs
clean ((Left x) : xs) = clean xs


----- IMAGE PROCESSING -----
----------------------------
left = (-2.0)
right = 2.0
top = 2.0
bottom = (-2.0)
<<<<<<< HEAD
vertPix = 800 -- Pixel ratios must adhere to coordinate ratios
horzPix = 800
=======
vertPix = 6000 -- Pixel ratios must adhere to coordinate ratios
horzPix = 6000
>>>>>>> parent of c9a74a7... Fixed indexing bug

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

incrTup :: (Int, Int, Int) -> Int -> (Int, Int, Int)
incrTup (a, b, c) i = (a + i, b + i, c + i)

incr mv i = do
	item <- M.read mv i
	if item == (10,10,10)
		then return ()
	else M.write mv i $ incrTup item 1

genVec :: [Complex] -> V.Vector (Int, Int, Int)
genVec xs = runST $ do
	mv <- M.replicate (vertPix * horzPix) (0, 0, 0)
	mapM_ (incr mv) $ map (convCoord . convComplex) xs
	V.freeze mv

genImage :: V.Vector (Int, Int, Int) -> PPM
genImage v = PPM v vertPix horzPix $ (\(x, y, z) -> x) $ V.maximum v


----- TEST CODE -----
---------------------
count :: (a -> Bool) -> [a] -> Int
count _ [] = 0
count f (x:xs)
	| f x = 1 + count f xs
	| otherwise = count f xs

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True	

center = [Complex 0 0]

----- MAIN -----
----------------
main = do
	-- Import / Parser
	rawData <- B.readFile "data"

	let formatedData = p ++ p -- Doubled bc/ new data contains half the points
		where p = clean $ parseAll $ remTrash $ split $ remSpace rawData

	-- Create Image
	let ppm = genImage $ genVec $ formatedData

	-- Write Image
	h <- openFile "test.ppm" WriteMode
	writeImage ppm h
	hClose h