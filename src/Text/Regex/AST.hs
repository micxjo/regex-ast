{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.AST
       ( Regex(..)
       , parseRegex
       , toText
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.List (intersperse)
import           Data.Monoid ((<>))

import           Data.Attoparsec.Text hiding (take, takeWhile)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T.Lazy
import           Data.Text.Lazy.Builder (Builder, singleton, fromText,
                                         toLazyText)
import qualified Data.Text.Lazy.Builder.Int as Builder.Int

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

perlClassP :: Parser Regex
perlClassP = Class <$> (char '\\' *> satisfy (inClass "dDsSwWhHvV"))

singleCharP :: Parser Regex
singleCharP = Literal . T.singleton <$> satisfy notSpecial

repeatUnit :: Parser Regex
repeatUnit =
  perlClassP <|> escapeLiteralP <|> singleCharP <|> anyCharP <|> groupP

zeroOrOneP :: Parser Regex
zeroOrOneP = do
  sub <- repeatUnit
  char '?'
  pure (ZeroOrOne sub)

zeroOrMoreP :: Parser Regex
zeroOrMoreP = do
  sub <- repeatUnit
  char '*'
  pure (ZeroOrMore sub)

oneOrMoreP :: Parser Regex
oneOrMoreP = do
  sub <- repeatUnit
  char '+'
  pure (OneOrMore sub)

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

concatP :: Parser Regex
concatP = do
  parts <- many1' (choice [ anchorP
                          , zeroOrOneP
                          , zeroOrMoreP
                          , oneOrMoreP
                          , repeatP
                          , anyCharP
                          , perlClassP
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
builder (Class c) = singleton '\\' <> singleton c
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
builder (ZeroOrOne sub) = builder sub <> singleton '?'
builder (ZeroOrMore sub) = builder sub <> singleton '*'
builder (OneOrMore sub) = builder sub <> singleton '+'
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
