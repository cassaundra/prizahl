{-# LANGUAGE GADTs #-}

module Language.Prizahl.Parser where

import           Control.Monad                    (void)
import           Data.List.NonEmpty               (fromList)
import           Data.Maybe                       (fromMaybe)
import           Data.Void                        (Void)
import           Language.Prizahl.Prog
import qualified Math.NumberTheory.Primes         as P
import qualified Math.NumberTheory.Primes.Testing as PT
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

type Parser = Parsec Void String

parseExpr = runParser expr ""

declaration :: Parser Declaration
declaration = sexp $ do
  symbol "define"
  choice
    [ VariableDefinition <$> identifier <*> expr,
      FunctionDefinition <$> identifier <*> formals <*> body
    ]

formals :: Parser Formals
formals =
  lexeme $
    choice
      [ SingleFormal <$> identifier,
        MultipleFormals <$> sexp (many identifier)
      ]

replLine :: Parser ReplLine
replLine =
  choice
    [ ReplDeclr <$> try declaration,
      ReplExpr <$> expr
    ]

body :: Parser Body
body = do
  decls <- many (lexeme $ try declaration)
  Body decls <$> expr

identifier :: Parser Identifier
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

expr :: Parser Expr
expr = lexeme $ try (Value <$> value) <|> variable <|> try ifStatement <|> application

variable :: Parser Expr
variable = Variable <$> identifier

ifStatement :: Parser Expr
ifStatement =
  sexp $ do
    symbol "if"
    test <- expr
    a <- expr
    If test a <$> expr

application :: Parser Expr
application =
  sexp $ do
    first <- expr
    rest <- many expr
    return $ Application first rest

value :: Parser Value
value = lexeme $ (Prime <$> prime) <|> factorization <|> boolean <|> try quoteSymbol <|> list <|> lambda

prime :: Parser (P.Prime Integer)
prime = do
  p <- lexeme L.decimal
  if PT.isPrime p
    then return $ P.nextPrime p
    else fail "Expected a prime"

factorization :: Parser Value
factorization =
  surround '[' ']' $
    Factorization . fromList <$> some factor

factor :: Parser Factor
factor = lexeme $ do
  base <- prime
  expt <- optional (char '^' *> L.decimal)
  return $ Factor (base, fromMaybe 1 expt)

boolean :: Parser Value
boolean = do
  char '#'
  choice
    [ Boolean True <$ char 't',
      Boolean False <$ char 'f'
    ]

quoteSymbol :: Parser Value
quoteSymbol = do
  char '\''
  s <- some (alphaNumChar <|> char '-' <|> char '_')
  return $ Symbol s

list :: Parser Value
list = do
  char '\''
  List <$> sexp (sepBy1 value whitespace)

lambda :: Parser Value
lambda =
  sexp $ do
    symbol "lambda"
    formals <- formals
    Lambda formals <$> body

whitespace :: Parser ()
whitespace = () <$ many space1

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

surround :: Char -> Char -> Parser a -> Parser a
surround l r a = lexeme (char l) *> a <* char r

sexp :: Parser a -> Parser a
sexp = surround '(' ')'
