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

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do whitespace; r <- numConst; eof; return r) s of
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

oper :: Parser Exp
oper = undefined

forc :: Parser Exp
forc = undefined

ifc :: Parser Exp
ifc = undefined

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

--- Handling of numConst
--- TODO: "- 5" still parses, perhaps because of read?
numConst :: Parser Exp
numConst = do
        n <- pNum
        return (Const (IntVal (n)))
       <|>
       do
        symbol "-"
        n <- pNumNoWhiteSpace
        return (Const (IntVal (-n)))

pNum :: Parser Int
pNum = lexeme pNumNoWhiteSpace

pNumNoWhiteSpace :: Parser Int
pNumNoWhiteSpace = do
  n <- satisfy (\number -> (number /=  '0') && isDigit number)
  n1 <- many1 (satisfy isDigit)
  return $ read (n : n1)
  <|>
  do
  n <- satisfy isDigit
  return $ read [n]

-- Handling of identifiers

ident :: Parser Exp
ident = lexeme $ do
  cs <- many1 (satisfy identChecker);
  return $ read cs

identChecker :: Char -> Bool
identChecker c = c `elem` ['_', '1', '2', '3', '4', '5', '6', '7']

-- Handling of stringConst

stringConst :: Parser Exp
stringConst = lexeme $ do ds <- many1 (satisfy stringChecker); return $ read ds

stringChecker :: Char -> Bool
stringChecker c = c `notElem` ['\'', '\\', '\n']



