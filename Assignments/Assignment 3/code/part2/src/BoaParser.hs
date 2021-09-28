-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import BoaAST
import Data.Char (isSpace, isDigit, isAlpha, isNumber,
                  isAlphaNum, isPrint, isPunctuation, isSymbol, isMark)
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString s =
   case readP_to_S (do whitespace; comments; r <- program; eof; return r) s of
                [] -> Left "Cannot parse."
                [(a,_)] -> Right a
                _ -> Left "Ambiguous grammar"

program :: Parser Program
program = stmts

stmts :: Parser [Stmt]
stmts = do
        n <- stmt
        symbol ";"
        n1 <- stmts
        return (n:n1)
       <|>
        do
        n <- stmt
        return [n]

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
-- needs to work with "not 5", now only works with "not (5)"
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
        do {e1 <- e ; symbol "!="; Not . Oper Eq e1 <$> e}
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
e :: Parser Exp
e = do
      _t <- t
      e' _t

e' :: Exp -> Parser Exp
e' e1 = do
          symbol "+";
          _t <- t;
          e' (Oper Plus e1 _t)
        <|>
        do
          symbol "-";
          _t <- t;
          e' (Oper Minus e1 _t)
        <|>
          return e1

-- Fourth level of precedence, here mul, div and mod is handled.
t :: Parser Exp
t = do
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
      return (Const TrueVal)
    <|>
    do
      Var <$> ident
    <|>
    do
      i <- ident
      symbol "("
      e <- expz []
      symbol ")"
      return (Call i e)
    <|>
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
      cs <- clausez [f]
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
      symbolNoWhiteSpace "for"
      i <- ident
      symbolNoWhiteSpace "in"
      CCFor i <$> expp

ifc :: Parser CClause
ifc = do
      symbolNoWhiteSpace  "if"
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
exps =  do
        n <- expp
        symbol ","
        n1 <- exps
        return (n:n1)
        <|>
        do
        n <- expp
        return [n]


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
-- are printable. illegal chars are ' and \, unless escaped, i.e. \i and \\
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
-- Added function for checking if something is a comment and then ignoring it.
comment :: Char -> Bool
comment c = isAlphaNum c || isPunctuation c || isSymbol c || isMark c || isSpace c

comments :: Parser ()
comments = do
           symbolNoWhiteSpace "#"
           many (satisfy comment)
           return ()
           <|>  
           return ()

whitespace :: Parser ()
whitespace = do many (satisfy isSpace); return ()

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; whitespace; comments; return a

symbol :: String -> Parser ()
symbol s = lexeme $ do string s; return ()

-- used throughout the functionality where we do not want 'lexeme' to 
-- interact with whitespace, e.g. within strings.
symbolNoWhiteSpace :: String -> Parser ()
symbolNoWhiteSpace s = do string s; return ()





