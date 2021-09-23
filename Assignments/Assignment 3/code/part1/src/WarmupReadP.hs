module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
-- E ::= T E' | "-" T E'
-- E' ::= "+" T E' | "-" T E' | eps
-- T ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char (isSpace, isDigit)
import Text.Parsec.Token (GenTokenParser(whiteSpace))

  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString s = case readP_to_S (do whitespace; r <- expr; eof; return r) s of
                [] -> Left "Cannot parse."
                [(a,_)] -> Right a
                _ -> Left "Ambiguous grammar"

expr :: Parser Exp
expr = do
        t <- term
        expr' t
       <|>
       do
        symbol "-"
        t <- term
        expr' (Negate t)

expr' :: Exp -> Parser Exp
expr' e1 = do
         symbol "+"
         t <- term
         expr' (Add e1 t)
        <|>
        do
         symbol "-"
         t <- term
         expr' (Add e1 (Negate t))
        <|>
         return e1

term :: Parser Exp
term = do
        Num <$> pNum
       <|>
       do
        symbol "("
        e <- expr
        symbol ")"
        return e

-- Helper functions (from slides)
whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

pNum :: Parser Int
pNum = lexeme $ do ds <- many1 (satisfy isDigit); return $ read ds

