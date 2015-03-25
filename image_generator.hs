module Main where

import System.Environment
import System.IO
import Data.Char
import Data.List
import Control.Applicative as A
import Text.ParserCombinators.ReadP as R


-- PARSING --

data Complex = Complex Float Float
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

parseNumber :: ReadP Float
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


testStr = "{{x -> -1.}}\n{{x -> 1.}}\n{{x -> 1.}}\n{{x -> -1.}}\n{{x -> 0. - 1.*I}, {x -> 0. + 1.*I}}\n{{x -> -1.}, {x -> 1.}}\n{{x -> -1.}, {x -> 1.}}\n{{x -> 0. - 1.*I}, {x -> 0. + 1.*I}}\n{{x -> -0.5000000000000001 - 0.8660254037844386*I}, \n {x -> -0.4999999999999998 + 0.8660254037844387*I}}\n{{x -> -0.6180339887498949}, {x -> 1.618033988749895}}\n{{x -> 0.5000000000000001 + 0.8660254037844386*I}, \n {x -> 0.4999999999999998 - 0.8660254037844387*I}}\n{{x -> -1.618033988749895}, {x -> 0.6180339887498949}}\n{{x -> -1.618033988749895}, {x -> 0.6180339887498949}}\n{{x -> 0.5000000000000001 + 0.8660254037844386*I}, \n {x -> 0.4999999999999998 - 0.8660254037844387*I}}"
testList = dataFormat testStr
testRead :: [Complex]
testRead = testList

-- IMAGE PROCESSING --

data Image = Image {pixels :: [[Int]], height :: Int, width :: Int}


-- MAIN --

main = do
	rawData <- readFile "data"
	handle <- openFile "test" WriteMode
	let formatedData = dataFormat rawData
	hPutStrLn handle $ show formatedData
	hClose handle