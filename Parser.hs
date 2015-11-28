module Parser where

import Data.Char
import Control.Monad

newtype Parser a = P (String -> [(a, String)])
instance Monad Parser where
--  return     :: a -> Parser a
  return v   =  P (\inp -> [(v, inp)])

--  (>>=)      :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f    =  P (\inp -> case parse p inp of
    [] -> []
    [(v, remains)] -> parse (f v) remains)

instance MonadPlus Parser where
  mzero       = P (\_ -> [])
  p `mplus` q = P (\inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = \inp -> p inp

failure :: Parser a
failure = mzero

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

item :: Parser Char
item = P(\inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then
              return x
            else failure

char :: Char -> Parser Char
char ch = sat (ch ==)

digit :: Parser Char
digit = sat isDigit

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

int :: Parser Int
int = do
  digits <- many1 digit
  let res = foldl (\acc -> \d -> acc * 10 + digitToInt d) 0 digits
  return res  

space :: Parser ()
space = do many (sat (' ' ==))
           return ()

-- space seperated parsers
ssp :: Parser a -> Parser [a]
ssp p = many (do space
                 x <- p
                 return x)                 
