import System.Environment
import System.IO
import Data.Char
import Control.Applicative as A
import Text.ParserCombinators.ReadP as R


-- PARSING --

data Complex = Complex Float Float
	deriving (Show)

data Test = Test String String
	deriving (Show)

parseX :: ReadP String
parseX = string ("x->")

parseFloatR :: ReadP String
parseFloatR = munch $ \x -> (x == '-') || isDigit x || (x == '.')
parseFloatC :: ReadP String
parseFloatC = munch $ \x -> (x == '-') || isDigit x || (x == '.') || (x == ' ')

parseAll = do
	parseX
	a <- parseFloatR
	b <- parseFloatC
	return $ Test a b

readC = readP_to_S parseAll


-- DATA FORMATTING --

split :: String -> Char -> [String]
split ss c = helper ss c []
	where
		helper [] _ t = [t]
		helper (s:ss) c t
			| s == c = t : helper ss c []
			| otherwise = helper ss c (t ++ [s])

dataFilter :: String -> String
dataFilter [] = []
dataFilter (x:xs)
	| elem x ['{','}', ' '] = dataFilter xs
	| otherwise = x : dataFilter xs

dataFormat :: String -> [String]
dataFormat str = map dataFilter $ split str '\n'

rawData = readFile "data"


-- MAIN --

-- TESTING VARIABLES --

teststr1 = "{}\n{}\n{{x -> -1.}}\n{{x -> 1.}}\n{{x -> 1.}}\n{{x -> -1.}}\n{{x -> 0. - 1.*I}, {x -> 0. + 1.*I}}\n{{x -> -1.}, {x -> 1.}}\n{{x -> -1.}, {x -> 1.}}\n{{x -> 0. - 1.*I}, {x -> 0. + 1.*I}}\n{{x -> -0.5000000000000001 - 0.8660254037844386*I}, \n {x -> -0.4999999999999998 + 0.8660254037844387*I}}\n{{x -> -0.6180339887498949}, {x -> 1.618033988749895}}\n{{x -> 0.5000000000000001 + 0.8660254037844386*I}, \n {x -> 0.4999999999999998 - 0.8660254037844387*I}}\n{{x -> -1.618033988749895}, {x -> 0.6180339887498949}}\n{{x -> -1.618033988749895}, {x -> 0.6180339887498949}}"
parsetest = ["","","x->-1.","x->1.","x->1.","x->-1.","x->0.-1.*I,x->0.+1.*I","x->-1.,x->1.","x->-1.,x->1.","x->0.-1.*I,x->0.+1.*I","x->-0.5000000000000001-0.8660254037844386*I,","x->-0.4999999999999998+0.8660254037844387*I","x->-0.6180339887498949,x->1.618033988749895","x->0.5000000000000001+0.8660254037844386*I,","x->0.4999999999999998-0.8660254037844387*I","x->-1.618033988749895,x->0.6180339887498949","x->-1.618033988749895,x->0.6180339887498949"]