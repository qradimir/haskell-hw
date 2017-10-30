module Task.Parser where

import            Control.Monad ((>=>))
import            Control.Applicative hiding (many)

data ParseError = EOF | Rejected
  deriving (Show, Eq)

combineErrors :: ParseError -> ParseError -> ParseError
combineErrors Rejected Rejected = Rejected
combineErrors _        _        = EOF


newtype Monstupar s a = Monstupar { runParser :: [s] -> Either ParseError ([s], a)}

type Parser = Monstupar Char

parser :: (String -> Either ParseError (String, a)) -> Parser a
parser = Monstupar

pass :: a -> Monstupar s a
pass a = Monstupar $ \xs -> Right (xs, a)

ok :: Monstupar s ()
ok = pass ()

rejectWith :: ParseError -> Monstupar s a
rejectWith = Monstupar . const . Left

reject :: Monstupar s a
reject = rejectWith Rejected

eof :: Monstupar s ()
eof = Monstupar f
  where
    f [] = Right ([], ())
    f _  = Left Rejected

like :: (s -> Bool) -> Monstupar s s
like p = Monstupar f
  where
    f []                 = Left EOF
    f (x:xs) | p x       = Right (xs, x)
             | otherwise = Left Rejected

same :: Eq s => s -> Monstupar s s
same x = like $ (==) x

instance Functor (Monstupar s) where
  fmap f (Monstupar p) = Monstupar (p >=> (pure . fmap f))


instance Applicative (Monstupar s) where
  pure = pass

  (Monstupar pf) <*> (Monstupar px) = Monstupar (pf >=> help)
    where
      help (s, f) =  px s >>= (pure . fmap f)

instance Alternative (Monstupar s) where
  empty = reject

  (Monstupar x) <|> (Monstupar y) = Monstupar $ \s -> case x s of
    Left e1 -> case y s of
      Left e2 -> Left $ combineErrors e1 e2
      Right r -> Right r
    Right r -> Right r

instance Monad (Monstupar s) where
  return = pass

  (Monstupar p) >>= f = Monstupar $ \s -> case p s of
    Right (news, a) -> runParser (f a) news
    Left e          -> Left e


sequential :: [Monstupar s a] -> Monstupar s [a]
sequential = foldr (\p ps -> p >>= \x -> (x:) <$> ps) (pass [])

match :: Eq s => [s] -> Monstupar s [s]
match = sequential . map same

many :: Monstupar s a -> Monstupar s [a]
many p = (p >>= \r -> (r:) <$> many p) <|> pass []

many1 :: Monstupar s a -> Monstupar s [a]
many1 p = p >>= \r -> (r:) <$> many p
