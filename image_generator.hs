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
	parseX
	a <- parseSingle
	b <- parseSingle
	return $ complexSum a b

testVar = B.pack "x->-1."


----- DATA FORMATTING -----
---------------------------
split :: B.ByteString -> [B.ByteString]
split = B.splitWith (\x -> x == ',' || x == '\n')

dataFilter :: B.ByteString -> B.ByteString
dataFilter = B.filter (\x -> not (elem x ['{', '}', ' ']))

parseAll :: [B.ByteString] -> [Either String Complex]
parseAll = map readComplex


----- IMAGE PROCESSING -----
----------------------------
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


----- MAIN -----
----------------
main = do
	-- Import / Parser
	rawData <- B.readFile "data"
	let formatedData = parseAll $ split $ dataFilter rawData
	-- Create Image
	--let ppm = genImage $ genVec $ formatedData

	-- Write Image
	h <- openFile "test" WriteMode
	hPutStr h $ show formatedData
	hClose h