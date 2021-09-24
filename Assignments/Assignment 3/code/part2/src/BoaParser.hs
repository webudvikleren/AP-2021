-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import BoaAST
import Data.Char (isSpace, isDigit, isAlpha, isNumber, isAlphaNum)
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

expp :: Parser Exp
expp = undefined

oper :: Parser Exp
oper = undefined

forc :: Parser CClause
forc = do
      symbol "for"
      i <- ident
      symbol "in"
      CCFor i <$> expp

ifc :: Parser CClause
ifc = do
      symbol "if"
      CCIf <$> expp

clausez :: [CClause ] -> Parser [CClause]
clausez cs = do
        for <- forc
        clausez (for:cs)
       <|>
       do
        _if <- ifc
        clausez (_if:cs)
       <|>
        return cs

expz :: [Exp] -> Parser [Exp]
expz es = do
        exps
       <|>
        return es

exps :: Parser [Exp]
exps = do
        n <- expp
        return [n]
       <|>
       do
        n <- expp
        symbol ","
        n1 <- exps
        return (n:n1)

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

symbolNoWhiteSpace :: String -> Parser ()
symbolNoWhiteSpace s = do string s; return ()

--- Handling of numConst
numConst :: Parser Exp
numConst = do
        Const . IntVal <$> pNum
       <|>
       do
        symbolNoWhiteSpace "-"
        n <- pNumNoWhiteSpace
        return (Const (IntVal (-n)))

pNum :: Parser Int
pNum = lexeme pNumNoWhiteSpace

pNumNoWhiteSpace :: Parser Int
pNumNoWhiteSpace = do
  n <- satisfy (\number -> number /= '0' && isDigit number)
  n1 <- many1 (satisfy isDigit)
  return $ read (n : n1)
  <|>
  do
  n <- satisfy isDigit
  return $ read [n]

-- Handling of identifiers

reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

ident :: Parser String
ident = lexeme $ do
  c <- satisfy isAlpha;
  cs <- many (satisfy isAlphaNum);
  let word = c:cs;
  if word `notElem` reserved then return word
  else return pfail "variable can't be a reserved word"

-- Handling of stringConst

stringConst :: Parser Exp
stringConst = lexeme $ do ds <- many1 (satisfy stringChecker); return $ read ds

stringChecker :: Char -> Bool
stringChecker c = c `notElem` ['\'', '\\', '\n']



