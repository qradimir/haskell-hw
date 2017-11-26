{-# LANGUAGE OverloadedStrings #-}

module Parser
     ( parseProgram
     , parseStatement
     , parse
     ) where

import           Structure

import           Text.Megaparsec
import           Text.Megaparsec.Expr
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Void
import           Data.Char (isSpace, isAlphaNum, isAlpha)
import           Data.Functor
import qualified Data.Text.Lazy as T

import           Control.Applicative
import           Control.Monad

type Parser = Parsec Void T.Text
type PError = ParseError Char Void

sc :: Parser ()
sc = L.space (takeWhile1P Nothing isSpace' $> ()) empty empty
  where
    isSpace' c = isSpace c && (c /= '\n')

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser ()
symbol = void <$> L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lit :: Parser Int
lit = fromInteger <$> lexeme L.decimal

rword :: T.Text -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [T.Text]
rws = ["let", "in", "mut", "for", "if", "then", "break"]

identifier :: Parser Ref
identifier = T.unpack <$> (lexeme . try) (do
    head' <- satisfy isAlpha
    rest' <- takeWhileP Nothing isAlphaNum
    let id' = T.cons head' rest'
    guard $ id' `notElem` rws
    return id')

ops :: [[Operator Parser Expr]]
ops = [ [ InfixL (symbol "*" $> Mul)
        , InfixL (symbol "/" $> Div) ]
      , [ InfixL (symbol "+" $> Add)
        , InfixL (symbol "-" $> Sub) ]
      ]

term :: Parser Expr
term = parens expr <|> Var <$> identifier <|> Lit <$> lit

expr :: Parser Expr
expr = makeExprParser term ops <|> letExpr

letExpr :: Parser Expr
letExpr = do
    rword "let"
    v <- identifier
    symbol "="
    d <- expr
    rword "in"
    e <- expr
    return $ Let v d e

stmt :: Parser Statement
stmt = newVarStmt <|> updVarStmt <|> printStmt <|> readStmt <|> forStmt <|> ifStmt <|> breakStmt

assigment :: Parser (Ref, Expr)
assigment = do
  v <- identifier
  symbol "="
  e <- expr
  return (v, e)

newVarStmt :: Parser Statement
newVarStmt = do
  rword "mut"
  (v, e) <- assigment
  return $ NewVarStatement v e

updVarStmt :: Parser Statement
updVarStmt = do
  (v, e) <- assigment
  return $ UpdVarStatement v e

printStmt :: Parser Statement
printStmt = do
  symbol "<"
  e <- expr
  return $ PrintStatement e

readStmt :: Parser Statement
readStmt = do
  symbol ">"
  v <- identifier
  return $ ReadStatement v

forStmt :: Parser Statement
forStmt = L.indentBlock scn $ do
  rword "for"
  v <- identifier
  rword "in"
  st <- expr
  symbol ".."
  end <- expr
  return (L.IndentMany Nothing (return . ForStatement v st end) stmt)

ifStmt :: Parser Statement
ifStmt = L.indentBlock scn $ do
  rword "if"
  e <- expr
  rword "then"
  return (L.IndentMany Nothing (return . IfStatement e) stmt)

breakStmt :: Parser Statement
breakStmt = rword "break" $> BreakStatement

program :: Parser [Statement]
program = sepEndBy (L.nonIndented scn stmt) scn <* eof

-- parseExpression :: T.Text -> Either PError Expr
-- parseExpression = runParser expr ""

parseStatement :: T.Text -> Either PError Statement
parseStatement = runParser stmt ""

parseProgram :: T.Text -> Either (ParseError Char Void) [Statement]
parseProgram = runParser program ""
