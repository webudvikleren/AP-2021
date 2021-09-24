-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import BoaAST
import Data.Char (isSpace, isDigit)
import Text.ParserCombinators.Parsec.Char (digit)
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s = case readP_to_S (do whitespace; r <- expr; eof; return r) s of
                [] -> Left "Cannot parse."
                [(a,_)] -> Right a
                _ -> Left "Ambiguous grammar"

program :: Parser Program
program = undefined

statements :: Parser [Stmt]
statements = undefined

statement :: Parser Stmt
statement = undefined

exp :: Parser Exp
exp = undefined

oper :: Parser Oper
oper = undefined

forc :: Parser CCFor
for = undefined

ifc :: Parser CCIf
for = undefined

clausez :: Parser CClause
clausez = undefined

expz :: Parser [Exp]
expz = undefined

exps :: Parser [Exp]
exps = undefined

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

numConst :: Parser Exp
numConst = do
        Num <$> pNum
       <|>
       do
        symbol "-"
        n <- pNumNoWhiteSpace
        return (Const (IntVal -n))

ident :: Parser Exp
ident = lexeme $ do
  cs <- many1 (satisfy identChecker);
  return $ read ds

identChecker :: Char -> Bool
identChecker c = c elem ['_', '1', '2', '3', '4', '5', '6', '7']

stringConst :: Parser Exp
stringConst = lexeme $ do ds <- many1 (satisfy stringChecker); return $ read ds

stringChecker :: Char -> Bool
stringChecker c = not elem c ['\'', '\\', '\n']

pNum :: Parser Int
pNum = lexeme pNumNoWhiteSpace

pNumNoWhiteSpace :: Parser Int
pNumNoWhiteSpace = do
  n <- satisfy (\number -> (number /=  0) && isDigit)
  n1 <- many1 (satisfy isDigit)
  return $ read (n ++ n1)
  <|>
  do
    0
