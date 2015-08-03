module Main where

import PPM
import UPPM
import System.IO
import Control.Applicative as A
import Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Lex.Fractional
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP
import Control.Monad.Par

import Control.DeepSeq

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
imgSize = 1000 :: Int

intensity = 300.0 :: Double
falloff = 0.8 :: Double
(r, g, b) = (255.0, 76.5, 25.5) :: (Double, Double, Double)

computeIndex :: Double -> Int
computeIndex x = (+) 1 $ floor $ (x + bounds) / (2 * bounds / fromIntegral imgSize)

convCoord ::(Int, Int) -> Int
convCoord (h, v) = v * imgSize + h

computeCIndex :: Complex -> Int
computeCIndex (Complex r i) = convCoord (computeIndex r, computeIndex i)

genVec :: [Complex] -> V.Vector Int
genVec xs = runST $ do
	mv <- M.replicate (imgSize ^ 2) 0
	mapM_ (incr mv) $ map computeCIndex xs -- unsafeModify
	V.freeze mv
		where incr mv i = do
				pre <- M.unsafeRead mv i
				M.unsafeWrite mv i $ pre + 1


genVec' :: [Complex] -> UV.Vector Int
genVec' xs = runST $ do
	mv <- UM.replicate (imgSize ^ 2) 0
	mapM_ (UM.modify mv (+ 1)) $ map computeCIndex xs
	UV.freeze mv

colorFunction :: Double -> (Int, Int, Int)
colorFunction d = (floor (y * r), floor(y * g), floor(y * b))
	where y = (intensity * d) ** falloff

genImage :: V.Vector Int -> PPM
genImage v = PPM (V.map (\x -> colorFunction((fromIntegral x) / (fromIntegral mx))) v) imgSize imgSize 255
			where mx = V.maximum v

genImage' :: UV.Vector Int -> PPM'
genImage' v = PPM' (UV.map (\x -> colorFunction((fromIntegral x) / (fromIntegral mx))) v) imgSize imgSize 255
			where mx = UV.maximum v

genImage'' :: UV.Vector Int -> PPM'
genImage'' v = PPM' out imgSize imgSize 255
	where
		mx = UV.maximum v
		adjColF freq = colorFunction $ (fromIntegral freq) / (fromIntegral mx)
		out = UV.generate (imgSize ^ 2) $ adjColF.(UV.!) v

------

parseMap :: [LB.ByteString] -> Int -> [Complex]
parseMap x n = runPar $ do
	let l1 = take n x
	let l2 = take n $ drop n x
	let l3 = take n $ drop (2*n) x
	let l4 = drop (3*n) x
	a <- spawnP $ map (extr.LP.parse parseComplex) $ l1
	b <- spawnP $ map (extr.LP.parse parseComplex) $ l2
	c <- spawnP $ map (extr.LP.parse parseComplex) $ l3
	d <- spawnP $ map (extr.LP.parse parseComplex) $ l4
	w <- get a
	x <- get b
	y <- get c
	z <- get d
	return $ c ++ d

---------- MAIN ----------

main = do
	rawData <- liftA LB.words (LB.readFile "/mnt/hgfs/outputs/out.txt")
	--let formatedData = map (extr.LP.parse parseComplex) rawData
	let formatedData = parseMap rawData 10000000
	formatedData `deepseq` return ()

	h <- openFile "test.ppm" WriteMode
	writeImage'(genImage' (genVec' formatedData)) h
	hClose h