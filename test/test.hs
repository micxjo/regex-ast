{-# LANGUAGE OverloadedStrings #-}
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
    [ ("()", Group Empty)
    , ("(f)", Group (Literal "f"))
    , ("(foo)", Group (Literal "foo"))
    , ("(foo|bar)", Group (Alternate [Literal "foo", Literal "bar"]))
    , ("(foo|bar|123)", Group (Alternate
                               [ Literal "foo"
                               , Literal "bar"
                               , Literal "123"]))
    , ("(foo|ba?r?|baz)",
       Group (Alternate
              [ Literal "foo"
              , Concat [ Literal "b"
                       , ZeroOrOne (Literal "a")
                       , ZeroOrOne (Literal "r")]
              , Literal "baz"]))
    ]
  , it "parses concatenations of groups" $ testParses
    [ ("()()", Concat [Group Empty, Group Empty])
    , ("(abc)()", Concat [Group (Literal "abc"), Group Empty])
    , ("()(abc)", Concat [Group Empty, Group (Literal "abc")])
    , ("(foo)(bar)", Concat [Group (Literal "foo"), Group (Literal "bar")])
    ]
  , it "parses nested groups" $ testParses
    [ ("(())", Group (Group Empty))
    , ("((foo))", Group (Group (Literal "foo")))
    , ("(foo(bar)baz)", Group (Concat
                               [ Literal "foo"
                               , Group (Literal "bar")
                               , Literal "baz"]))
    , ("(foo(bar)?baz)", Group (Concat
                                [ Literal "foo"
                                , ZeroOrOne (Group (Literal "bar"))
                                , Literal "baz"]))
    ]
  , it "parses zero-or-one" $ testParses
    [ ("a?", ZeroOrOne (Literal "a"))
    , ("a?b?", Concat [ZeroOrOne (Literal "a"), ZeroOrOne (Literal "b")])
    , ("(abc)?", ZeroOrOne (Group (Literal "abc")))
    , ("(a|b)?", ZeroOrOne (Group (Alternate [Literal "a", Literal "b"])))
    , (".?", ZeroOrOne AnyChar)
    ]
  , it "parses one-or-more" $ testParses
    [ ("a+", OneOrMore (Literal "a"))
    , ("a+b+", Concat [OneOrMore (Literal "a"), OneOrMore (Literal "b")])
    , ("(abc)+", OneOrMore (Group (Literal "abc")))
    , ("(a|b)+", OneOrMore (Group (Alternate [Literal "a", Literal "b"])))
    , (".+", OneOrMore AnyChar)
    ]
  ]

tests :: TestTree
tests = testGroup "regex-ast tests" [parseTests]

main :: IO ()
main = defaultMain tests
