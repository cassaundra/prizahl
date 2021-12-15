{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Prizahl.Parser
  ( body
  , replLine
  , parseFile
  , parseReplLine
  ) where

import           Control.Monad                    (guard, void)
import           Control.Monad.Reader
import           Control.Monad.State              (runState)
import           Control.Monad.Trans.Reader       (ReaderT)
import           Data.List.NonEmpty               (fromList)
import           Data.Maybe                       (fromMaybe)
import           Data.Void                        (Void)
import qualified Math.NumberTheory.Primes         as P
import qualified Math.NumberTheory.Primes.Testing as PT
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

import           Language.Prizahl.AST

data ParserState =
  ParserState
    { quoted :: Bool
    }

defaultState = ParserState {quoted = False}

type Parser = ReaderT ParserState (Parsec Void String)

parseFile = runParser (runReaderT file defaultState)
parseReplLine = runParser (runReaderT replLine defaultState) "repl"

file :: Parser Body
file = body <* eof

replLine :: Parser (Either Declaration Expr)
replLine = choice [Left <$> try declaration, Right <$> expr]

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
expr = lexeme $ try (EValue <$> value) <|> variable <|> try begin <|> application

variable :: Parser Expr
variable = EVariable <$> identifier <?> "variable"

application :: Parser Expr
application = label "application" $
  sexp $ do
    first <- expr
    rest <- many expr
    return $ EApplication first rest

begin :: Parser Expr
begin = label "begin block" $
  sexp $ do
    symbol "begin"
    EBegin <$> body

-- TODO let

value :: Parser Value
value = label "value" $ lexeme $ (VPrime <$> prime) <|> factorization <|> boolean <|> try quoteSymbol <|> list <|> lambda

prime :: Parser (P.Prime Integer)
prime =
  label "prime" $ do
    p <- lexeme L.decimal
    unless (PT.isPrime p) $ failHere "not prime!"
    return (P.nextPrime p)

factorization :: Parser Value
factorization =
  label "factorization" $
  surround '[' ']' $ VFactorization . fromList <$> some factor

factor :: Parser Factor
factor =
  label "factor" $
  lexeme $ do
    base <- prime
    expt <- fromMaybe 1 <$> optional (char '^' *> L.decimal)
    unless (expt > 0) $ failHere "not positive!"
    return $ Factor (base, expt)

boolean :: Parser Value
boolean = label "boolean" $ do
  char '#'
  choice
    [ VBoolean True <$ char 't',
      VBoolean False <$ char 'f'
    ]

quoteSymbol :: Parser Value
quoteSymbol = label "symbol" $ do
  ParserState {quoted} <- ask
  unless quoted $ void (char '\'')
  s <- some (alphaNumChar <|> char '-' <|> char '_' <|> char '\'')
  return $ VSymbol s

list :: Parser Value
list =
  label "list" $ do
    char '\''
    local (const ParserState {quoted=True}) $
      VList <$> sexp (many value)

lambda :: Parser Value
lambda =
  label "lambda" $ sexp $ do
    symbol "lambda"
    formals <- formals
    VLambda formals <$> body

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

failHere :: (MonadParsec e s m, MonadFail m) => String -> m b
failHere msg = do
  offset <- getOffset
  setOffset (offset - 1)
  fail msg
