{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-|
Module          : Text.Regex.AST.CharClass
Copyright       : (c) 2016 Micxjo Funkcio
License         : BSD3
Maintainer      : micxjo@fastmail.com
Stability       : Experimental
-}
module Text.Regex.AST.CharClass
       ( CharClass
       , perl_d
       , perl_D
       , perl_w
       , perl_W
       , perl_s
       , perl_S
       , perl_h
       , perl_H
       , perl_v
       , perl_V
       , charClassP
       , builder
       ) where

import           Control.Applicative ((<|>))
import           Data.List (foldl')

import           Data.Attoparsec.Text
import           Data.RangeSet.List
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

-- | A regex character-class, represented as a set of inclusive character ranges.
type CharClass = RSet Char

unions :: (Enum a, Ord a) => [RSet a] -> RSet a
unions = foldl' union empty

perl_d :: CharClass
perl_d = singletonRange ('0', '9')

perl_D :: CharClass
perl_D = complement perl_d

perl_w :: CharClass
perl_w = unions [ singletonRange ('a', 'z')
                , singletonRange ('A', 'Z')
                , singletonRange ('0', '9')
                , singleton '_'
                ]

perl_W :: CharClass
perl_W = complement perl_w

perl_s :: CharClass
perl_s = unions [ singleton '\t'
                , singleton '\n'
                , singleton '\f'
                , singleton '\r'
                , singleton ' '
                ]

perl_S :: CharClass
perl_S = complement perl_s

perl_h :: CharClass
perl_h = unions [ singleton '\t'
                , singleton ' '
                ]

perl_H :: CharClass
perl_H = complement perl_h

perl_v :: CharClass
perl_v = unions [ singleton '\n'
                , singleton '\r'
                ]

perl_V :: CharClass
perl_V = complement perl_v

perlClassP :: Parser CharClass
perlClassP =
  char '\\' *> ((char 'd' *> pure perl_d) <|>
                (char 'D' *> pure perl_D) <|>
                (char 'w' *> pure perl_w) <|>
                (char 'W' *> pure perl_W) <|>
                (char 's' *> pure perl_s) <|>
                (char 'S' *> pure perl_S) <|>
                (char 'h' *> pure perl_h) <|>
                (char 'H' *> pure perl_H) <|>
                (char 'v' *> pure perl_v) <|>
                (char 'V' *> pure perl_V))

charRangeP :: Parser CharClass
charRangeP = do
  lo <- satisfy (not . inClass "[]-")
  char '-'
  hi <- satisfy (not . inClass "[]-")
  pure (singletonRange (lo, hi))

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
  | cc == perl_D = B.fromText "\\D"
  | cc == perl_w = B.fromText "\\w"
  | cc == perl_W = B.fromText "\\W"
  | cc == perl_s = B.fromText "\\s"
  | cc == perl_S = B.fromText "\\S"
  | cc == perl_h = B.fromText "\\h"
  | cc == perl_H = B.fromText "\\H"
  | cc == perl_v = B.fromText "\\v"
  | cc == perl_V = B.fromText "\\V"
  | otherwise = mconcat [ B.singleton '['
                        , mconcat (map rangeBuilder (toRangeList cc))
                        , B.singleton ']'
                        ]
  where rangeBuilder (rMin, rMax)
          | rMin == rMax = B.singleton rMin
          | otherwise = mconcat [ B.singleton rMin
                                , B.singleton '-'
                                , B.singleton rMax
                                ]
