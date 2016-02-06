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
type GroupName = Text

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
           | Group Regex (Maybe GroupName)
           | ZeroOrOne Regex
           | ZeroOrMore Regex
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

anchorP :: Parser Regex
anchorP = (char '^' >> pure StartLine) <|> (char '$' >> pure EndLine)

singleCharP :: Parser Regex
singleCharP = Literal . T.singleton <$> satisfy (inClass "a-zA-Z0-9")

zeroOrOneP :: Parser Regex
zeroOrOneP = do
  sub <- singleCharP <|> anyCharP <|> groupP
  char '?'
  pure (ZeroOrOne sub)

zeroOrMoreP :: Parser Regex
zeroOrMoreP = do
  sub <- singleCharP <|> anyCharP <|> groupP
  char '*'
  pure (ZeroOrMore sub)

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

groupNameP :: Parser GroupName
groupNameP = do
  string "?P<"
  name <- takeTill ('>' ==)
  guard (T.length name > 0)
  char '>'
  pure name

groupP :: Parser Regex
groupP = do
  char '('
  name <- option Nothing (Just <$> groupNameP)
  sub <- regexP
  char ')'
  pure (Group sub name)

concatP :: Parser Regex
concatP = do
  parts <- many1' (choice [ anchorP
                          , zeroOrOneP
                          , zeroOrMoreP
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
parseRegex = parseOnly (regexP <* endOfInput)
