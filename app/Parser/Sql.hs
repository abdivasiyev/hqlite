{-# LANGUAGE OverloadedStrings #-}

module Parser.Sql where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Control.Applicative hiding (some, many)

type Parser = Parsec Void Text

data NumBase = Hexadecimal
             | Decimal
             | Octal
             | Binary deriving (Show, Eq)

data Literal = LNum { int :: Integer,
                      frac :: Maybe Integer,
                      exponent :: Maybe Integer,
                      base :: NumBase } 
                 | LStr Text
                 -- | Blob Text
                 | LNull
                 | LBool Bool
                 | LCurrentTime
                 | LCurrentDate
                 | LCurrentTimestamp
            deriving (Show, Eq)

comment :: Parser ()
comment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 comment blockComment

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t' <|> char '\n')) comment blockComment

-- lexeme parses a parser and consumes trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parseLNull parses a NULL literal with case insensitivity.
parseLNull :: Parser Literal
parseLNull = LNull <$ lexeme (string' "null")

-- parseLBool parses a boolean literal with case insensitivity.
parseLBool :: Parser Literal
parseLBool = LBool <$> try parseBool
    where parseBool = (True  <$ lexeme (lookAhead $ string' "true"))
                  <|> (False <$ lexeme (lookAhead $ string' "false"))

-- parseLCurrentTime, parseLCurrentDate, and parseLCurrentTimestamp
parseLCurrentTime :: Parser Literal
parseLCurrentTime = LCurrentTime <$ lexeme (string' "current_time")

parseLCurrentDate :: Parser Literal
parseLCurrentDate = LCurrentDate <$ lexeme (string' "current_date")

parseLCurrentTimestamp :: Parser Literal
parseLCurrentTimestamp = LCurrentTimestamp <$ lexeme (string' "current_timestamp")

parseLStr :: Parser Literal
parseLStr = LStr <$> (char '\'' *> lexeme (T.pack <$> manyTill L.charLiteral (char '\'')))

parseNum :: NumBase -> Parser Integer
parseNum base = case base of
    Binary      -> L.binary
    Octal       -> L.octal
    Hexadecimal -> L.hexadecimal
    _           -> L.decimal

parseSignedNum :: NumBase -> Parser Integer
parseSignedNum base = do
   sign <- optional (char '-' <|> char '+')
   num  <- lexeme (parseNum base)

   return $ case sign of
      Just '-' -> negate num
      _        -> num

parseBase :: Parser NumBase
parseBase = choice 
  [  lexeme (char' 'b') *> pure Binary
   , lexeme (char' 'o') *> pure Octal
   , lexeme (char' 'x') *> pure Hexadecimal
  ] <|> pure Decimal


parseLNum :: Parser Literal
parseLNum = do
   base <- parseBase

   LNum
      <$> parseSignedNum base
      <*> (optional (char '.' *> parseNum base) <|> return Nothing)
      <*> (optional (char' 'e' *> parseSignedNum base) <|> return Nothing)
      <*> pure base

parseLiteral :: Parser Literal
parseLiteral = choice
    [ parseLNum
    , parseLStr
    , parseLNull
    , parseLBool
    , parseLCurrentTime
    , parseLCurrentDate
    , parseLCurrentTimestamp
    ]

data Expr =
    ELiteral Literal
  | EColumn Text
 deriving (Show, Eq)

parseIdentifier :: Parser Text
parseIdentifier = lexeme (T.pack <$> some (alphaNumChar <|> char '_' <|> char '.' <|> char '*'))

parseExpr :: Parser Expr
parseExpr = choice
   [ ELiteral <$> parseLiteral
   , EColumn  <$> parseIdentifier
   ]

data SelectStmt = SelectStmt {
    selectExpr :: [Expr],
    selectFrom :: Text
    } deriving (Show, Eq)

parseSelectStmt :: Parser SelectStmt
parseSelectStmt = do
   void $ lexeme (string' "select")
   exprs <- sepBy parseExpr (lexeme $ char ',')

   void $ lexeme (string' "from")

   fromTable <- parseIdentifier

   void $ char ';'

   SelectStmt <$> pure exprs <*> pure fromTable

data Stmt = Select SelectStmt
    deriving (Show, Eq)

