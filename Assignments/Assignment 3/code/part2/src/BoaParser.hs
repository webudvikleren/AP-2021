-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need

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

numConst :: Parser Int
numConst = lexeme $ do ds <- many1 (satisfy isDigit); return $ read ds


