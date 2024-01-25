{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad.State.Strict
-- (1)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Bifunctor (Bifunctor(bimap))

type Pair = (Maybe String, Maybe String)

type Parser = ParsecT Void Text (State Pair)

sc :: Parser ()
sc =
  L.space
    space1 -- (2)
    (L.skipLineComment "//") -- (3)
    (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

cfst v (x, y)  = (v, y)

csnd v (x, y)  = (x, v)

ident :: (Pair -> Maybe String) -> (Maybe String -> Pair -> Pair) -> Parser String
ident getter setter = observing (lexeme (some alphaNumChar)) >>= f
    where
        f (Right v) = modify (setter (Just v)) >> pure v
        f (Left e) = gets getter >>= maybe (parseError e) pure

state = ident fst cfst

symbol = ident snd csnd

