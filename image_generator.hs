import PPM
import System.IO
import Control.Applicative as A
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.ByteString.Lazy as LP


apply :: a -> (a -> b) -> b
apply a = \f -> f a


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
			liftA2 (\x y -> read ((x:y) ++ "0")) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'
		parseConv = P.option id $ liftA (const cConv) $ P.string $ B.pack "*I"

test :: P.Parser String
test = liftA2 (\x y -> (x:y)) P.anyChar $ many $ P.satisfy $ \x -> P.isDigit x || x == '.'

parseComplex :: P.Parser Complex
parseComplex = liftA2 cAdd parseTerm $ many $ many (P.char '+') A.*> parseTerm


---------- IMAGE PROCESSING ----------

bounds = 2.0 :: Double
imgSize = 400 :: Int

intensity = 300 :: Int
falloff = 0.8 :: Double
(r, g, b) = (1.0, 0.3, 0.1)

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
		where
			incr mv i = do
				pre <- M.unsafeRead mv i
				M.unsafeWrite mv i $ pre + 1

colorFunction :: Int -> (Int, Int, Int)
colorFunction n = (floor (y * r), floor(y * g), floor(y * b))
	where y = ((fromIntegral (intensity * n)) ** falloff)

colorFunction1 n = (n, n, n)

genImage :: V.Vector Int -> PPM
genImage v = PPM (V.map colorFunction1 v) imgSize imgSize $ V.maximum v


extr :: LP.Result a -> a
extr (LP.Done _ r) = r
extr (LP.Fail _ _ y) = error y

---------- MAIN ----------

main = do
	rawData <- liftA LB.words (LB.readFile "/mnt/hgfs/outputs/out_1-14.txt")
	let formatedData = map (extr.LP.parse parseComplex) rawData

	h <- openFile "test.ppm" WriteMode
	writeImage (genImage (genVec formatedData)) h
	hClose h