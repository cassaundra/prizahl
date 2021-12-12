{-# LANGUAGE GADTs #-}

module Language.Prizahl.Parser where

import           Control.Monad                    (guard, void)
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
declaration = label "definition" $ sexp $ do
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

body :: Parser Body
body = do
  decls <- many (lexeme $ try declaration)
  Body decls <$> expr

identifier :: Parser Identifier
identifier =
  label "identifier" $
  lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

expr :: Parser Expr
expr = lexeme $ try (Value <$> value) <|> variable <|> try ifStatement <|> application

variable :: Parser Expr
variable = Variable <$> identifier <?> "variable"

ifStatement :: Parser Expr
ifStatement = label "if statement" $
  sexp $ do
    symbol "if"
    test <- expr
    a <- expr
    b <- expr
    return $ If test a b

application :: Parser Expr
application = label "application" $
  sexp $ do
    first <- expr
    rest <- many expr
    return $ Application first rest

value :: Parser Value
value = label "value" $ lexeme $ (Prime <$> prime) <|> factorization <|> boolean <|> try quoteSymbol <|> list <|> lambda

prime :: Parser (P.Prime Integer)
prime = label "prime" $ do
  p <- lexeme L.decimal
  guard (PT.isPrime p) <?> "prime number"
  return (P.nextPrime p)

factorization :: Parser Value
factorization =
  label "factorization" $
  surround '[' ']' $ Factorization . fromList <$> some factor

factor :: Parser Factor
factor = label "factor" $ lexeme $ do
  base <- prime
  expt <- fromMaybe 1 <$> optional (char '^' *> L.decimal)
  guard (expt > 0) <?> "positive integer"
  return $ Factor (base, expt)

boolean :: Parser Value
boolean = label "boolean" $ do
  char '#'
  choice
    [ Boolean True <$ char 't',
      Boolean False <$ char 'f'
    ]

quoteSymbol :: Parser Value
quoteSymbol = label "symbol" $ do
  char '\''
  s <- some (alphaNumChar <|> char '-' <|> char '_')
  return $ Symbol s

list :: Parser Value
list = label "list" $ do
  char '\''
  List <$> sexp (many value)

lambda :: Parser Value
lambda =
  label "lambda" $ sexp $ do
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

replLine :: Parser (Maybe ReplLine)
replLine = optional $ choice [ReplDeclr <$> try declaration, ReplExpr <$> expr]

file :: Parser Body
file = body <* eof
