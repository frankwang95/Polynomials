module Main where

import PPM
import System.IO
import Control.Applicative as A
import Control.Monad.ST
import Control.Monad.Par
import Control.DeepSeq
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lex.Fractional
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP

instance NFData Complex where
	rnf (Complex a b) = rnf a `seq` rnf b

apply :: a -> (a -> b) -> b
apply a = \f -> f a

extr :: LP.Result a -> a
extr (LP.Done _ r) = r
extr (LP.Fail _ _ y) = error y


---------- PARSING ----------

iT = B.pack "*I"

data Complex = Complex Double Double
	deriving (Show)

cAdd:: Complex -> [Complex] -> Complex
cAdd (Complex a b) [(Complex c d)] = Complex (a + c) (b + d)
cAdd c _ = c
cConv :: Complex -> Complex
cConv (Complex a b) = Complex b a

parseTerm :: P.Parser Complex
parseTerm = parseDouble <*> parseConv
	where 
		parseDouble = liftA (\x -> apply (Complex x 0)) $
			liftA2 (\x y -> toDecimal (B.append x y)) (P.take 1) $ P.takeWhile $ \x -> P.isDigit x || x == '.'
		parseConv = P.option id $ liftA (const cConv) $ P.string $ B.pack "*I"
		toDecimal s = case (readSigned readDecimal) s of
                       Just (d, _) -> d

test :: P.Parser String
test = liftA2 (\x y -> (x:y)) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'

parseComplex :: P.Parser Complex
parseComplex = liftA2 cAdd parseTerm $ many $ many (P.char '+') A.*> parseTerm



---------- IMAGE PROCESSING ----------

bounds = 2.0 :: Double
imgSize = 8000 :: Int

intensity = 5000.0 :: Double
falloff = 0.1 :: Double
(r, g, b) = (212.0, 118.0, 137.0) :: (Double, Double, Double)

computeIndex :: Double -> Int
computeIndex x = (+) 1 $ floor $ (x + bounds) / (2 * bounds / fromIntegral imgSize)

convCoord ::(Int, Int) -> Int
convCoord (h, v) = v * imgSize + h

computeCIndex :: Complex -> Int
computeCIndex (Complex r i) = convCoord (computeIndex r, computeIndex i)

genVec :: [Complex] -> V.Vector Int
genVec xs = runST $ do
	mv <- M.replicate (imgSize ^ 2) 0
	mapM_ (M.modify mv (+1)) $ map computeCIndex xs -- unsafeModify
	V.freeze mv

colorFunction :: Double -> (Int, Int, Int)
colorFunction d = (floor (y * r), floor(y * g), floor(y * b))
	where y = (intensity * d) ** falloff

colorFunction1 n = (n, n, n)

genImage :: V.Vector Int -> PPM
genImage v = PPM (V.map (\x -> colorFunction((fromIntegral x) / (fromIntegral mx))) v) imgSize imgSize 255
	where mx = V.maximum v

pipeLineFunction :: [LB.ByteString] -> V.Vector Int
pipeLineFunction xs = genVec $ map (extr.LP.parse parseComplex) xs


---------- MAIN ----------

main = do
	rawDataA <- liftA LB.words (LB.readFile "/mnt/hgfs/Outputs/Arch/xaa")
	rawDataB <- liftA LB.words (LB.readFile "/mnt/hgfs/Outputs/Arch/xab")

	let formatedData = runPar $ do
		a <- spawnP $ pipeLineFunction rawDataA
		b <- spawnP $ pipeLineFunction rawDataB
		vec1 <- get a
		vec2 <- get b
		return $ V.zipWith (+) vec1 vec2

	h <- openFile "test.ppm" WriteMode
	writeImage (genImage formatedData) h
	hClose h