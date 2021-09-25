-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import BoaAST
import Data.Char (isSpace, isDigit, isAlpha, isNumber, isAlphaNum, isPrint)
import Text.ParserCombinators.Parsec.Char (digit)
import Text.Read.Lex (Lexeme(Ident))
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Exp
parseString s =
   case readP_to_S (do whitespace; r <- stringConst; eof; return r) s of
                [] -> Left "Cannot parse."
                [(a,_)] -> Right a
                _ -> Left "ambiguous grammar."

program :: Parser Program
program = stmts

stmts :: Parser [Stmt]
stmts = do
        n <- stmt
        return [n]
       <|>
       do
        n <- stmt
        symbol ";"
        n1 <- stmts
        return (n:n1)

stmt :: Parser Stmt
stmt = do
       name <- ident
       symbol "="
       SDef name <$> expp
       <|>
       do
       SExp <$> expp

-- Disambiguated grammar

-- First level of precedence with 'not'
expp :: Parser Exp
expp = do
       symbol "not"
       symbol "("
       e <- expp'
       symbol ")"
       return (Not e)
       <|>
       do
       expp'

-- Second level of precdence with relational operators. Notice that they are
-- non-associative.
expp' :: Parser Exp
expp' = do {e1 <- e ; symbol "=="; Oper Eq e1 <$> e}
        <|>
        do {e1 <- e ; symbol "<"; Oper Less e1 <$> e}
        <|>
        do {e1 <- e ; symbol "<="; Not . Oper Greater e1 <$> e;}
        <|>
        do {e1 <- e ; symbol ">"; Oper Greater e1 <$> e}
        <|>
        do {e1 <- e ; symbol ">="; Not . Oper Less e1 <$> e}
        <|>
        do {e1 <- e ; symbol "in"; Oper In e1 <$> e}
        <|>
        do {e1 <- e ; symbol "not"; symbol "in"; Not . Oper In e1 <$> e}
        <|>
        e

-- Third level of precedence where plus and minus is handled. These operators
-- are left-associative. Grammar is split into e and e' to avoid left-recursion.
-- TODO: e and e' are unfinished
e :: Parser Exp
e = do
      _t <- t
      e' _t
    <|>
    do
      symbol "-"
      _t <- t
      e' _t

e' :: Exp -> Parser Exp
e' e1 = do
          symbol "+";
          _t <- t;
          e' (Oper Plus _t e1)
        <|>
        do
          symbol "+";
          _t <- t;
          e' (Oper Plus _t e1)
        <|>
          return e1

t :: Parser Exp
t = do
      _f <- f
      t' _f
    <|>
    do
      symbol "-"
      _f <- f
      t' _f

t' :: Exp -> Parser Exp
t' t1 = do
          symbol "*";
          _f <- f;
          t' (Oper Times  _f t1)
        <|>
        do
          symbol "//";
          _f <- f;
          t' (Oper Div  _f t1)
        <|>
        do
          symbol "%";
          _f <- f;
          t' (Oper Mod  _f t1)
        <|>
          return t1

f :: Parser Exp
f = numConst
    <|>
    stringConst
    <|>
    do
      symbol "None"
      return (Const NoneVal)
    <|>
    do
      symbol "False"
      return (Const FalseVal)
    <|>
    do
      symbol "True"
      return (Const NoneVal)
    <|>
    do
      Var <$> ident
    <|>
    --TODO: ident "(" exprz ")"
    do
      symbol "["
      es <- expz []
      symbol "]"
      return (List es)
    <|>
    do
      symbol "["
      e1 <- expp
      f <- forc
      cs <- clausez []
      symbol "]"
      return (Compr e1 cs)
    <|>
    do
      symbol "("
      e1 <- expp
      symbol ")"
      return e1


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

clausez :: [CClause] -> Parser [CClause]
clausez cs = do
        for <- forc
        clausez (cs ++ [for])
       <|>
       do
        _if <- ifc
        clausez (cs ++ [_if])
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

alphaNumOr_ :: Char -> Bool
alphaNumOr_ c = c == '_' || isAlphaNum c

ident :: Parser String
ident = lexeme $ do
  c <- satisfy isAlpha <|> char '_'
  cs <- many (satisfy alphaNumOr_)
  let word = c:cs
  if word `notElem` reserved then return word
  else return pfail "variable cannot be a reserved word"

-- Handling of stringConst

-- checks if the strings contains illegal characters and that the characters
-- are printable.
-- TODO: there is a lot of functionality missing here.
stringChecker :: Char -> Bool
stringChecker c = (c == '\n') || isPrint c

stringConst :: Parser Exp
stringConst = do
   symbolNoWhiteSpace "'"
   ds <- many1 (satisfy stringChecker);
   symbolNoWhiteSpace "'"
   return (Const (StringVal ds))

test = "'fo\\o\
         \b\na\'r'"

-- Utility functions

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- used throughout the functionality where we do not want 'lexeme' to 
-- interact with whitespace, e.g. within strings.
symbolNoWhiteSpace :: String -> Parser ()
symbolNoWhiteSpace s = do string s; return ()





