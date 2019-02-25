module While.Parser (
    parseWhile
  ) where

import Control.Applicative ((<$>))

import Parser

import While.Syntax

-- | Parse a While program.
parseWhile :: String -> Maybe Stm
parseWhile = parse p
  where
    p :: Parser Stm
    p = do r <- stmsP
           eof
           return r

-- Parse an arithmetic expression
aexpP :: Parser Aexp
aexpP =
    term `chainl1` addop
  where
    term :: Parser Aexp
    term = factor `chainl1` mulop

    factor :: Parser Aexp
    factor =  Const <$> integer
          <|> Var <$> identifier
          <|> parens aexpP

    mulop = symbol "*" >> return Mul

    addop  =  do { _ <- symbol "+"
                 ; return Add
                 }
          <|> do { _ <- symbol "-"
                 ; return Sub
                 }

-- Parse a boolean expression
bexpP :: Parser Bexp
bexpP  =  do     { _ <- symbol "true"
                 ; return BTrue
                 }
          <|> do { _ <- symbol "false"
                 ; return BFalse
                 }
          <|> do { _ <- symbol "-"
                 ; b <- bexpP
                 ; return (Not b)
                 }
          <|> do { a1 <- aexpP
                 ; op <- eqop
                 ; a2 <- aexpP
                 ; return (op a1 a2)
                 }
          <|> do { b1 <- bexpP
                 ; op <- boolop
                 ; b2 <- bexpP
                 ; return (op b1 b2)
                 }
  where
    eqop  =  (symbol "=" >> return Eq)
         <|> (symbol "<=" >> return Le)

    boolop = symbol "&&" >> return And

-- Parse a statement
stmP :: Parser Stm
stmP =  do { keyword "skip"
           ; return Skip
           }
    <|> do { keyword "if"
           ; b <- bexpP
           ; keyword "then"
           ; s1 <- stmP
           ; keyword "else"
           ; s2 <- stmP
           ; return (If b s1 s2)
           }
    <|> do { keyword "while"
           ; b <- bexpP
           ; s <- stmP
           ; return (While b s)
           }
    <|> do { x <- identifier
           ; keyword ":="
           ; a <- aexpP
           ; return (Assign x a)
           }
    <|> parens stmsP

-- Parse a sequence of statements
stmsP :: Parser Stm
stmsP =
  stmP `chainl1` seqStmP
  where
    seqStmP = do { keyword ";"
                 ; return Seq
                 }
