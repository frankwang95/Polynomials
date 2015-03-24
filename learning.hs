data Parser s = Parser {runParser :: String -> [(s, String)]}

instance Functor Parser where
	fmap f p = Parser $ \s -> [(f t, u) | (t,u) <- runParser p s]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
	[] -> []
	a:as -> [(a, as) | p a]

string :: String -> Parser String
string str = Parser $ \s -> [(t, u) | let (t, u) = splitAt (length str) s, str == t]

token :: String -> a -> Parser a
token s t = fmap (const t) $ string s

parseTrue = token "True" True
parseFalse = token "False" False

