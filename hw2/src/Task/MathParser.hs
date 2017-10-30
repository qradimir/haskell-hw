module Task.MathParser where

import           Control.Applicative ((<|>))
import           Data.Char
import           Data.List           (intercalate)
import qualified Data.Map.Lazy       as M
import           Data.Maybe
import           Task.Parser

abParser :: Parser (Char, Char)
abParser = (,) <$> same 'a' <*> same 'b'

abParser_ :: Parser ()
abParser_ = abParser *> ok

newtype Name = Name String
  deriving (Eq,  Ord)

instance Show Name where
  show (Name s) = s

data Term = Const Int | Ref Name
  deriving (Eq)


intParser :: Parser Int
intParser = read <$> many1 (like isDigit)

nameParser :: Parser Name
nameParser = Name <$> ((:) <$> like isLetter <*> many (like isLetter <|> like isDigit))

space :: Parser Char
space = same ' ' <|> same '\t'

termParser :: Parser Term
termParser = Const <$> intParser <|> Ref <$> nameParser


data SExpr = Atom Term | Comb [SExpr]
  deriving (Show, Eq)

sExprParser :: Parser SExpr
sExprParser = many space *> (atomParser <|> combParser) <* many space where
  atomParser = Atom <$> termParser
  combParser = Comb <$> (same '(' *> many sExprParser <* same ')')

data Expr = Expr Name [Term]
  deriving (Eq)

instance Show Term where
  show (Const i) = show i
  show (Ref n)   = show n

instance Show Expr where
  show (Expr n ts) = "let " ++ show n ++ " = " ++ intercalate " + " (map show ts)

type Dict = M.Map Name [Term]

fold :: [Expr] -> [Expr]
fold = fold' M.empty
 where
   fold' :: Dict -> [Expr] -> [Expr]
   fold' _   []     = []
   fold' dict (e:es) = let (newE, newDict) = foldExpr dict e in newE : fold' newDict es

   foldExpr :: Dict -> Expr -> (Expr, Dict)
   foldExpr dict (Expr n ts) = let
                                 inlinedTs = concatMap (inlineTerm dict) ts
                                 (is, ns)  = decompose inlinedTs
                                 foldedTs  = Const is : map Ref ns
                               in
                                (Expr n foldedTs, M.insert n foldedTs dict)

   decompose :: [Term] -> (Int,  [Name])
   decompose = foldr (\t (is, ns) -> case t of
       Const i -> (i+is, ns)
       Ref n   -> (is, n:ns)
     ) (0,[])

   inlineTerm :: Dict -> Term -> [Term]
   inlineTerm _    t@(Const _) = [t]
   inlineTerm dict t@(Ref n)   = fromMaybe [t] (M.lookup n dict)

exprParser :: Parser Expr
exprParser = many space >>
             match "let" >>
             many1 space >>
             nameParser >>= \name ->
             many space >>
             same '=' >>
             many space >>
             termParser >>= \fstTerm ->
             many (many space >>
                   same '+' >>
                   many space >>
                   termParser) >>= \restTerms ->
             pass (Expr name (fstTerm:restTerms))

programParser :: Parser [Expr]
programParser = exprParser >>= \expr -> many (many space >> same '\n' >> exprParser) >>= \exprs -> pass (expr:exprs)

instance Read Expr where
  readsPrec _ s = case runParser exprParser s of
    Right (rest, expr) -> [(expr, rest)]
    Left _             -> []

programReader :: IO ()
programReader = interact (\s -> case runParser programParser s of
  Right (_, exprs) -> unlines . map show . fold $ exprs
  Left e           -> "Parse error: " ++ show e)
