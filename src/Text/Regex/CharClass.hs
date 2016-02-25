{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module Text.Regex.CharClass
       ( CharClass
       , perl_d
       , perl_w
       , perl_s
       , perl_h
       , perl_v
       , charClassP
       , builder
       ) where

import           Control.Applicative ((<|>))

import           Data.Attoparsec.Text
import qualified Data.Range as Range
import           Data.RangeSet
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

type CharClass = RangeSet Char

perl_d :: CharClass
perl_d = rangeSet '0' '9'

perl_w :: CharClass
perl_w = unions [ rangeSet 'a' 'z'
                , rangeSet 'A' 'Z'
                , rangeSet '0' '9'
                , singleton '_'
                ]

perl_s :: CharClass
perl_s = unions [ singleton '\t'
                , singleton '\n'
                , singleton '\f'
                , singleton '\r'
                , singleton ' '
                ]

perl_h :: CharClass
perl_h = unions [ singleton '\t'
                , singleton ' '
                ]

perl_v :: CharClass
perl_v = unions [ singleton '\n'
                , singleton '\r'
                ]

perlClassP :: Parser CharClass
perlClassP =
  char '\\' *> ((char 'd' *> pure perl_d) <|>
                (char 'w' *> pure perl_w) <|>
                (char 's' *> pure perl_s) <|>
                (char 'h' *> pure perl_h) <|>
                (char 'v' *> pure perl_v))

charRangeP :: Parser CharClass
charRangeP = do
  lo <- satisfy (not . inClass "[]-")
  char '-'
  hi <- satisfy (not . inClass "[]-")
  pure (rangeSet lo hi)

singleCharP :: Parser CharClass
singleCharP = singleton <$> satisfy (not . inClass "[]-")

bracketClassP :: Parser CharClass
bracketClassP = do
  char '['
  classes <- many1' (perlClassP <|> charRangeP <|> singleCharP)
  char ']'
  pure (unions classes)

charClassP :: Parser CharClass
charClassP = perlClassP <|> bracketClassP

builder :: CharClass -> Builder
builder cc
  | cc == perl_d = B.fromText "\\d"
  | cc == perl_w = B.fromText "\\w"
  | cc == perl_s = B.fromText "\\s"
  | cc == perl_h = B.fromText "\\h"
  | cc == perl_v = B.fromText "\\v"
  | otherwise = mconcat [ B.singleton '['
                        , mconcat (map rangeBuilder (ranges cc))
                        , B.singleton ']'
                        ]
  where rangeBuilder r
          | Range.isSingleton r = B.singleton (Range.rangeMin r)
          | otherwise = mconcat [ B.singleton (Range.rangeMin r)
                                , B.singleton '-'
                                , B.singleton (Range.rangeMax r)
                                ]
