{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.AST
       ( Regex(..)
       , parseRegex
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)

import           Data.Attoparsec.Text hiding (take, takeWhile)
import           Data.Text (Text)
import qualified Data.Text as T

type CharClass = Char

data Regex = Empty
           | Literal !Text
           | AnyChar
           | AnyCharNoNL
           | Class !CharClass
           | StartLine
           | EndLine
           | StartText
           | EndText
           | WordBoundary
           | NotWordBoundary
           | Group Regex
           | ZeroOrOne Regex
           | OneOrMore Regex
           | Repeat !Regex !Integer !(Maybe Integer)
           | Concat ![Regex]
           | Alternate ![Regex]
           deriving (Eq, Show)

instance Monoid Regex where
  mempty = Empty

  mappend r Empty = r
  mappend Empty r = r
  mappend r1 r2 = Concat [r1, r2]

emptyP :: Parser Regex
emptyP = pure Empty

literalCharP :: Parser Char
literalCharP = do
  c <- satisfy (inClass "a-zA-Z0-9")
  next <- peekChar
  guard (next /= Just '?' && next /= Just '+')
  pure c

literalP :: Parser Regex
literalP = Literal . T.pack <$> many1 literalCharP

anyCharP :: Parser Regex
anyCharP = char '.' >> pure AnyChar

singleCharP :: Parser Regex
singleCharP = Literal . T.singleton <$> satisfy (inClass "a-zA-Z0-9")

zeroOrOneP :: Parser Regex
zeroOrOneP = do
  sub <- singleCharP <|> anyCharP <|> groupP
  char '?'
  pure (ZeroOrOne sub)

oneOrMoreP :: Parser Regex
oneOrMoreP = do
  sub <- singleCharP <|> anyCharP <|> groupP
  char '+'
  pure (OneOrMore sub)

repeatP :: Parser Regex
repeatP = do
  sub <- singleCharP <|> anyCharP <|> groupP
  char '{'
  first <- decimal
  second <- option (Just first) (char ',' *> option Nothing (Just <$> decimal))
  char '}'
  pure (Repeat sub first second)

alternateP :: Parser Regex
alternateP = do
  first <- concatP <|> emptyP
  char '|'
  rest <- sepBy1 (concatP <|> emptyP) (char '|')
  pure (Alternate (first : rest))

groupP :: Parser Regex
groupP = do
  char '('
  sub <- regexP
  char ')'
  pure (Group sub)

concatP :: Parser Regex
concatP = do
  parts <- many1' (choice [ zeroOrOneP
                          , oneOrMoreP
                          , repeatP
                          , anyCharP
                          , groupP
                          , literalP])
  case parts of
    [part] -> pure part
    _ -> pure (Concat parts)

regexP :: Parser Regex
regexP = alternateP <|> concatP <|> emptyP

parseRegex :: Text -> Either String Regex
parseRegex = parseOnly regexP
