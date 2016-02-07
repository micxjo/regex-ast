{-# LANGUAGE OverloadedStrings #-}
import Data.Either (isLeft)

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)

import Text.Regex.AST

describe :: TestName -> [TestTree] -> TestTree
describe = testGroup

it :: TestName -> Assertion -> TestTree
it = testCase

shouldParseTo :: Text -> Regex -> Assertion
shouldParseTo text regex = parseRegex text @?= Right regex

shouldNotParse :: Text -> Assertion
shouldNotParse text = isLeft (parseRegex text) @?= True

testParses :: [(Text, Regex)] -> Assertion
testParses = mapM_ (uncurry shouldParseTo)

parseTests :: TestTree
parseTests = describe "parseRegex"
  [ it "parses an empty regex" $
      "" `shouldParseTo` Empty
  , it "parses basic literals" $ testParses
    [ ("a", Literal "a")
    , ("ab", Literal "ab")
    , ("1FooBar23", Literal "1FooBar23")
    ]
  , it "parses any-char" $ testParses
    [ (".", AnyChar)
    , ("..", Concat [AnyChar, AnyChar])
    ]
  , it "parses literal and empty alternations" $ testParses
    [ ("a|b", Alternate [Literal "a", Literal "b"])
    , ("a|b|c", Alternate [Literal "a", Literal "b", Literal "c"])
    , ("foo|bar|123", Alternate [Literal "foo", Literal "bar", Literal "123"])
    , ("foo|", Alternate [Literal "foo", Empty])
    , ("|foo", Alternate [Empty, Literal "foo"])
    , ("|", Alternate [Empty, Empty])
    , ("||", Alternate [Empty, Empty, Empty])
    ]
  , it "parses basic groups" $ testParses
    [ ("()", Group Empty Nothing)
    , ("(f)", Group (Literal "f") Nothing)
    , ("(foo)", Group (Literal "foo") Nothing)
    , ("(foo|bar)", Group (Alternate [Literal "foo", Literal "bar"]) Nothing)
    , ("(foo|bar|123)",
       Group (Alternate
              [ Literal "foo"
              , Literal "bar"
              , Literal "123"])
       Nothing)
    , ("(foo|ba?r?|baz)",
       Group (Alternate
              [ Literal "foo"
              , Concat [ Literal "b"
                       , ZeroOrOne (Literal "a")
                       , ZeroOrOne (Literal "r")]
              , Literal "baz"])
       Nothing)
    ]
  , it "parses concatenations of groups" $ testParses
    [ ("()()", Concat [Group Empty Nothing, Group Empty Nothing])
    , ("(abc)()", Concat [Group (Literal "abc") Nothing, Group Empty Nothing])
    , ("()(abc)", Concat [Group Empty Nothing, Group (Literal "abc") Nothing])
    , ("(foo)(bar)", Concat [ Group (Literal "foo") Nothing
                            , Group (Literal "bar") Nothing])
    ]
  , it "parses nested groups" $ testParses
    [ ("(())", Group (Group Empty Nothing) Nothing)
    , ("((foo))", Group (Group (Literal "foo") Nothing) Nothing)
    , ("(foo(bar)baz)",
       Group (Concat
              [ Literal "foo"
              , Group (Literal "bar") Nothing
              , Literal "baz"])
       Nothing)
    , ("(foo(bar)?baz)",
       Group (Concat
              [ Literal "foo"
              , ZeroOrOne (Group (Literal "bar") Nothing)
              , Literal "baz"])
       Nothing)
    ]
  , it "parses named groups" $ testParses
    [ ("(?P<foo>)", Group Empty (Just "foo"))
    , ("(?P<foo>bar)", Group (Literal "bar") (Just "foo"))
    , ("(?P<foo>bar)(?P<baz>quux)",
       Concat [ Group (Literal "bar") (Just "foo")
              , Group (Literal "quux") (Just "baz")])
    , ("(?P<foo>())", Group (Group Empty Nothing) (Just "foo"))
    ]
  , it "parses zero-or-one" $ testParses
    [ ("a?", ZeroOrOne (Literal "a"))
    , ("a?b?", Concat [ZeroOrOne (Literal "a"), ZeroOrOne (Literal "b")])
    , ("(abc)?", ZeroOrOne (Group (Literal "abc") Nothing))
    , ("(a|b)?",
       ZeroOrOne (Group (Alternate [Literal "a", Literal "b"]) Nothing))
    , (".?", ZeroOrOne AnyChar)
    ]
  , it "parses zero-or-more" $ testParses
    [ ("a*", ZeroOrMore (Literal "a"))
    , ("a*b*", Concat [ZeroOrMore (Literal "a"), ZeroOrMore (Literal "b")])
    , ("(abc)*", ZeroOrMore (Group (Literal "abc") Nothing))
    , ("(a|b)*",
       ZeroOrMore (Group (Alternate [Literal "a", Literal "b"]) Nothing))
    , (".*", ZeroOrMore AnyChar)
    ]
  , it "parses one-or-more" $ testParses
    [ ("a+", OneOrMore (Literal "a"))
    , ("a+b+", Concat [OneOrMore (Literal "a"), OneOrMore (Literal "b")])
    , ("(abc)+", OneOrMore (Group (Literal "abc") Nothing))
    , ("(a|b)+",
       OneOrMore (Group (Alternate [Literal "a", Literal "b"]) Nothing))
    , (".+", OneOrMore AnyChar)
    ]
  , it "parses repeats" $ testParses
    [ ("a{4,10}", Repeat (Literal "a") 4 (Just 10))
    , ("a{4,}", Repeat (Literal "a") 4 Nothing)
    , ("a{4}", Repeat (Literal "a") 4 (Just 4))
    , ("a{4,3}b{0,10}", Concat [ Repeat (Literal "a") 4 (Just 3)
                               , Repeat (Literal "b") 0 (Just 10)])
    , ("(abc){0,}", Repeat (Group (Literal "abc") Nothing) 0 Nothing)
    , ("(a|b){2,3}",
       Repeat (Group (Alternate [Literal "a", Literal "b"]) Nothing) 2 (Just 3))
    , (".{3,3}", Repeat AnyChar 3 (Just 3))
    ]
  , it "parses line boundaries" $ testParses
    [ ("^", StartLine)
    , ("$", EndLine)
    , ("^$", Concat [StartLine, EndLine])
    , ("^foo$", Concat [StartLine, Literal "foo", EndLine])
    , ("^(foo$)|(bar)$", Alternate
       [ Concat [ StartLine
                , Group (Concat [ Literal "foo"
                                , EndLine])
                  Nothing
                ]
       , Concat [ Group (Literal "bar") Nothing
                , EndLine]])
    ]
  , it "parses perl-style character classes" $ testParses
    [ ("\\d", Class 'd')
    , ("\\D", Class 'D')
    , ("\\d\\s", Concat [Class 'd', Class 's'])
    , ("\\d+", OneOrMore (Class 'd'))
    , ("(\\d*)", Group (ZeroOrMore (Class 'd')) Nothing)
    ]
  , it "fails to parse bad regexes" $ mapM_ shouldNotParse
    [ "?"
    , "??"
    , "?+"
    , "+"
    , "++"
    , "+?"
    , "*"
    , "**"
    , "*?"
    , "("
    , "(("
    , "(()"
    , "(foo"
    , "(?P<name>"
    , "(?P<>foo)"   -- empty group name not allowed
    , ")"
    , "))"
    , "a)"
    , "a|b)"
    , "a{1,2,3}"
    , "a{3.14}"
    , "a{foo}"
    , "\\"
    , "\\\\"
    , "\\x"
    ]
  ]

toTextTests :: TestTree
toTextTests = testGroup "toText"
  [ testCase "round trips basic patterns" $
    mapM_ (\t -> toText <$> parseRegex t @?= Right t)
    [ ""
    , "foo"
    , "."
    , "\\d"
    , "^"
    , "$"
    , "^$"
    , "^foo$"
    , "(foo)"
    , "foo|bar|123"
    , "(foo|bar|123)(baz)"
    , "abc?"
    , "(blah)*"
    , "(())+"
    , "^|$"
    , "(?P<a>(?P<bc>)){1,3}"
    , ".{0}"
    , "\\d{5,}"
    ]
  ]

tests :: TestTree
tests = testGroup "regex-ast tests" [parseTests, toTextTests]

main :: IO ()
main = defaultMain tests
