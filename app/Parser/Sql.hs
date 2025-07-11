{-# LANGUAGE OverloadedStrings #-}

module Parser.Sql where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L


data Literal = LNum { int :: Integer,
                      frac :: [Int],
                      exponent :: Integer,
                      base :: Int } 
                 | LStr Text
                 -- | Blob Text
                 | LNull
                 | LBool Bool
                 | LCurrentTime
                 | LCurrentDate
                 | LCurrentTimestamp
            deriving (Show, Eq)

data BindParam = BindParam Text

data Column = Column {column :: Text, alias :: Text}

data Table = Table Text Column

data Schema = Schema Text Table

data UnaryOp = UnaryOp Text Expr

data BinaryOp = BinaryOp Text Expr Expr

data Expr = ELiteral   Literal
          | EBindParam BindParam
          | EColumn    Column
          | ETable     Table
          | ESchema    Schema
          | EUnary     UnaryOp
          | EBinaryOp  BinaryOp
    deriving (Show, Eq)
