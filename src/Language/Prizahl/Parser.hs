{-# LANGUAGE GADTs            #-}

module Language.Prizahl.Parser where

import           Control.Monad              (void)
import           Data.List.NonEmpty         (fromList)
import           Data.Void                  (Void)
import           Language.Prizahl.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseExpr = runParser expr ""

declaration :: Parser Declaration
declaration = sexp $ do
  symbol "define"
  choice [ VariableDefinition <$> lexeme identifier <*> expr,
           FunctionDefinition <$> lexeme identifier <*> formals <*> body
         ]

formals :: Parser Formals
formals = lexeme $ choice
  [ SingleFormal <$> identifier,
    MultipleFormals <$> sexp (many identifier)
  ]

body :: Parser Body
body = do
  decls <- many (lexeme $ try declaration)
  expr <- expr
  return $ Body decls expr

identifier :: Parser Identifier
identifier = (:) <$> letterChar <*> many (alphaNumChar <|> char '-')

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
    b <- expr
    return $ If test a b

application :: Parser Expr
application = sexp $ do
  first <- expr
  rest <- many expr
  return $ Application first rest

value :: Parser Value
value = lexeme $ boolean <|> try quoteSymbol <|> list <|> lambda

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
  s <- some (alphaNumChar <|> char '-') -- TODO more characters
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
    body <- body
    return $ Lambda formals body

whitespace :: Parser ()
whitespace = () <$ many space1

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

surround :: Char -> Char -> Parser a -> Parser a
surround l r a = (lexeme $ char l) *> a <* char r

sexp :: Parser a -> Parser a
sexp = surround '(' ')'
