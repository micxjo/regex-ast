{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module         : Text.Regex.AST
Copyright      : (c) 2016 Micxjo Funkcio
License        : BSD3
Maintainer     : micxjo@fastmail.com
Stability      : Experimental
-}
module Text.Regex.AST
       ( Regex(..)
       , CharClass
       , parseRegex
       , toText
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.List (intersperse)
import           Data.Monoid ((<>))
import           GHC.Generics (Generic)

import           Control.DeepSeq
import           Data.Attoparsec.Text hiding (take, takeWhile)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy
import           Data.Text.Lazy.Builder (Builder, singleton, fromText,
                                         toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int

import           Text.Regex.AST.CharClass (CharClass)
import qualified Text.Regex.AST.CharClass as CharClass

type GroupName = Text

-- | A regular expression abstract syntax tree.
data Regex
  -- | Never match
  = Empty
  -- | Match a sequence of one or more literal characters.
  | Literal !Text
  -- |Match any character.
  | AnyChar
  -- | Match any character, excluding newline.
  | AnyCharNoNL
  -- | Match against a character class.
  | Class !CharClass
  -- | Match the start of a line or beginning of input.
  | StartLine
  -- | Match the end of a line or end of input.
  | EndLine
  -- | Match the start of input.
  | StartText
  -- | Match the end of input.
  | EndText
  -- | Match at a word boundary.
  | WordBoundary
  -- | Match a non-boundary position.
  | NotWordBoundary
  -- | Match the sub-expression as a group, with an optional name.
  | Group Regex (Maybe GroupName)
  -- | Repeat the sub-expression with a lower bound and optional upper bound.
  | Repeat !Regex !Integer !(Maybe Integer)
  -- | Match the sub-expressions one after another.
  | Concat ![Regex]
  -- | Match at least one of the sub-expressions.
  | Alternate ![Regex]
  deriving (Eq, Show, Generic)

instance NFData Regex

emptyP :: Parser Regex
emptyP = pure Empty

notRepeater :: Maybe Char -> Bool
notRepeater Nothing = True
notRepeater (Just c) = not (inClass "?*+{" c)

notSpecial :: Char -> Bool
notSpecial = not . inClass "\\(){}[].*+?^$|"

literalCharP :: Parser Builder
literalCharP = do
  c <- satisfy notSpecial
  next <- peekChar
  guard (notRepeater next)
  pure (singleton c)

isEscapeChar :: Char -> Bool
isEscapeChar = inClass "\\aftnrv.*+?^$()[]{}|"

escapeSeqP :: Parser Builder
escapeSeqP = do
  char '\\'
  c <- satisfy isEscapeChar
  next <- peekChar
  guard (notRepeater next)
  pure (singleton '\\' <> singleton c)

escapeLiteralP :: Parser Regex
escapeLiteralP = do
  char '\\'
  c <- satisfy isEscapeChar
  pure (Literal (T.pack ['\\', c]))

toStrictText :: Builder -> Text
toStrictText = T.Lazy.toStrict . toLazyText

literalP :: Parser Regex
literalP =
  Literal . toStrictText . mconcat <$> many1 (escapeSeqP <|> literalCharP)

anyCharP :: Parser Regex
anyCharP = char '.' >> pure AnyChar

anchorP :: Parser Regex
anchorP = (char '^' >> pure StartLine) <|> (char '$' >> pure EndLine)

singleCharP :: Parser Regex
singleCharP = Literal . T.singleton <$> satisfy notSpecial

repeatUnit :: Parser Regex
repeatUnit =
  choice [ charClassP
         , escapeLiteralP
         , singleCharP
         , anyCharP
         , groupP
         ]

zeroOrOneP :: Parser Regex
zeroOrOneP = do
  sub <- repeatUnit
  char '?'
  pure (Repeat sub 0 (Just 1))

zeroOrMoreP :: Parser Regex
zeroOrMoreP = do
  sub <- repeatUnit
  char '*'
  pure (Repeat sub 0 Nothing)

oneOrMoreP :: Parser Regex
oneOrMoreP = do
  sub <- repeatUnit
  char '+'
  pure (Repeat sub 1 Nothing)

repeatP :: Parser Regex
repeatP = do
  sub <- repeatUnit
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

charClassP :: Parser Regex
charClassP = Class <$> CharClass.charClassP

concatP :: Parser Regex
concatP = do
  parts <- many1' (choice [ anchorP
                          , zeroOrOneP
                          , zeroOrMoreP
                          , oneOrMoreP
                          , repeatP
                          , charClassP
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

builder :: Regex -> Builder
builder Empty = mempty
builder (Literal t) = fromText t
builder AnyChar = singleton '.'
builder AnyCharNoNL = singleton '.'
builder (Class cc) = CharClass.builder cc
builder StartLine = singleton '^'
builder EndLine = singleton '$'
builder StartText = fromText "\\A"
builder EndText = fromText "\\z"
builder WordBoundary = fromText "\\b"
builder NotWordBoundary = fromText "\\B"
builder (Group sub Nothing) =
  singleton '(' <> builder sub <> singleton ')'
builder (Group sub (Just n)) =
  fromText "(?P<" <> fromText n <> singleton '>' <> builder sub <> singleton ')'
builder (Repeat sub 0 (Just 1)) = builder sub <> singleton '?'
builder (Repeat sub 0 Nothing) = builder sub <> singleton '*'
builder (Repeat sub 1 Nothing) = builder sub <> singleton '+'
builder (Repeat sub lo Nothing) =
  builder sub <> singleton '{' <> Builder.Int.decimal lo <> fromText ",}"
builder (Repeat sub lo (Just hi))
  | lo == hi = builder sub <> singleton '{' <> Builder.Int.decimal lo
    <> singleton '}'
  | otherwise = builder sub <> singleton '{' <> Builder.Int.decimal lo
    <> singleton ',' <> Builder.Int.decimal hi <> singleton '}'
builder (Concat subs) = mconcat (map builder subs)
builder (Alternate subs) =
  mconcat (intersperse (singleton '|') (map builder subs))

toText :: Regex -> Text
toText = toStrictText .  builder
