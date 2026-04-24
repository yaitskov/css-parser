-- cabal test   --test-option=--quickcheck-tests=10 --test-option=--quickcheck-max-size=22 --test-option=--hide-successes
module Main where

import CssParser
import CssParser.Test.Arbitrary.File ()
import CssParser.Test.Arbitrary.Media ()
import CssParser.Utils (encodeString, readCssString, encodeIdentifier, readIdentifier)
import Data.Text (pack)
import Data.Text.Lazy qualified as TL
import Prelude
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@=?) )
import Test.Tasty.QuickCheck -- ( testProperty, withMaxSuccess, withMaxSize, label, (===) )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "CssParser"
  [ testGroup "Encode-decode"
    [ testProperty "Encode-decode identity 1" (encodeDecode '"')
    , testProperty "Encode-decode identity 2" (encodeDecode '\'')
    , testProperty "Encode-decode identifier" encodeDecodeId
    ]
  , testGroup "Examples"
    [ testGroup "Empty body"
      (fmap (\x -> cpt x (x <> " {}")) validSelectors)
    , testGroup "Nested1"
      (fmap (\x -> cpt x (x <> " {\n" <> x <> "{\t}\r}"))
        validSelectors)
    , testGroup "Layer"
      [ testGroup "Stmt"
        (fmap (\x -> cpt x (x <> ";")) layerStmt)
      ]
    , testGroup "Properties"
      (fmap (\x -> cpt x ("p { " <> x <> " }")) properties)
    ]
  , testGroup "Arbitrary "
    [ testProperty "Alex"
      (withMaxSuccess 410 encodeDecodeAlex)
    , testProperty "Encode-decode CSS identity"
      -- cabal test with by default runs about a minute
      -- (withMaxSize 60 (withMaxSuccess 61
        encodeDecodeCss -- ) )
    ]
  ]
  where
    cpt m x = testCase m (True @=? checkParse (x <> " {}"))

encodeDecode :: Char -> String -> Bool
encodeDecode c b = readCssString (encodeString c b) == b

encodeDecodeId :: String -> Bool
encodeDecodeId b = readIdentifier (TL.unpack (encodeIdentifier (pack b))) == b

encodeDecodeAlex :: CssFile -> Property
encodeDecodeAlex cf =
  label cfCss $
    case alex cfCss of
      Left e -> error $ "Alex failed: " <> e <> "After:\n" <> cfCss
      Right v -> v === v
  where
   cfCss = TL.unpack $ toCssText cf


encodeDecodeCss :: CssFile -> Bool
encodeDecodeCss sg =
  case parseCssP sgTxt of
    Failed e ->
      error $ "Failed to parse:\n" <> sgTxt <>
      "\nTokens:\n" <>  show (fmap getToken <$> alex sgTxt) <>
      "\nError: " <> e
    Ok parsedSg
      | sg == parsedSg -> True
      | otherwise ->
        error $ "Parsed value differs:\n" <> show parsedSg <>
          "\nCSS Input:\n" <> sgTxt <>
          "\nCSS Print:\n" <> TL.unpack (toCssText parsedSg)
  where
    sgTxt = TL.unpack $ toCssText sg

checkParse :: String -> Bool
checkParse x = y == y
  where y = parseCss x

examples :: [String]
examples = media <> validSelectors <> layer <> page

properties :: [String]
properties =
  [ "margin: 20px;"
  , "--my-prop: 20px;"
  , "padding: 12em 20mm;"
  , "padding: 12em 25pt 2px 20mm;"
  , "margin-top: auto;"
  , "display: none;"
  , "border: 1px solid green;"
  , "src: url(\"https://example.org/SWOP2006_Coated5v2.icc\");"
  , "src: format(\"opentype\");"
  , "src: format(\"opentype\") tech(color);"
  ]

page :: [String]
page =
  [ "@page"
  , "@page one"
  , "@page xxx:first"
  , "@page :blank eee"
  , "@top-left"
  ]

layerStmt :: [String]
layerStmt =
  [ "@layer ao-euth"
  , "@layer l1"
  , "@layer l1, and, oo "
  ]

layer :: [String]
layer =
  [ "@layer"
  , "@layer l1"
  ]

media :: [String]
media =
  [ "@media screen and (width >= 900px)"
  , "@media not all and (hover: hover)"
  , "@media screen, print"
  , "@media all "
  , "@media  "
  , "@media (max-width: 320px)"
  , "@media (-webkit-transform-3d) "
  , "@media (400px < width < 1000px) or (a)"
  ]

-- Based on the w3c testkit: https://test.csswg.org/harness/suite/selectors-3_dev/
validSelectors :: [String]
validSelectors =
  [ "body > p"
  , "div ol>li p"
  , "*.pastoral"
  , ".pastoral"
  , "h1.pastoral"
  , "p.pastoral.marine"
  , "h1#chapter1"
  , "#chapter1"
  , "*#z98y"
  , "math + p"
  , "/* ---- */ math +/* ]] */p/* (((- */ "
  , "<!--  ---- --> math +<!-- ]] -->p<!-- (((- --> "
  , "h1.opener + h2"
  , "h1 ~ pre"
  , "*"
  , "div :first-child"
  , "div *:first-child"
  , "body > h2:nth-of-type(n+2):nth-last-of-type(n+2)"
  , "body > h2:not(:first-of-type):not(:last-of-type)"
  , "h1, h2, h3"
  , "h1"
  , "foo|h1"
  , "foo|*"
  , "|h1"
  , "*|h1"
  , "*[hreflang|=en]"
  , "[hreflang|=en]"
  , "*.warning"
  , ".warning"
  , "*#myid"
  , "#myid"
  , "ns|*"
  , "*|*"
  , "|*"
  , "*"
  , "[att]"
  , "[att=val]"
  , "[att~=val]"
  , "[att|=val]"
  , "h1[title]"
  , "span[class=\"example\"]"
  , "span[hello=\"Cleveland\"][goodbye=\"Columbus\"]"
  , "a[rel~=\"copyright\"]"
  , "a[href=\"http://www.w3.org/\"]"
  , "a[hreflang=fr]"
  , "a[hreflang|=\"en\"]"
  , "DIALOGUE[character=romeo]"
  , "DIALOGUE[character=juliet]"
  , "[att^=val]"
  , "[att$=val]"
  , "[att*=val]"
  , "object[type^=\"image/\"]"
  , "a[href$=\".html\"]"
  , "p[title*=\"hello\"]"
  , "[foo|att=val]"
  , "[*|att]"
  , "[|att]"
  , "[att]"
  , "EXAMPLE[radix=decimal]"
  , "EXAMPLE[radix=octal]"
  , "EXAMPLE"
  , "*.pastoral"
  , ".pastoral"
  , "H1.pastoral"
  , "p.pastoral.marine"
  , "h1#chapter1"
  , "#chapter1"
  , "*#z98y"
  , "a.external:visited"
  , "a:link"
  , "a:visited"
  , "a:hover"
  , "a:active"
  , "a:focus"
  , "a:focus:hover"
  , "p.note:target"
  , "*:target"
  , "*:target::before"
  , "html:lang(fr-be)"
  , "html:lang(de)"
  , ":lang(fr-be) > q"
  , ":lang(de) > q"
  , "[lang|=fr]"
  , ":lang(fr)"
  , "[lang|=fr]"
  , "tr:nth-child(2n+1)"
  , "tr:nth-child(odd)"
  , "tr:nth-child(2n+0)"
  , "tr:nth-child(even)"
  , "p:nth-child(4n+1)"
  , "p:nth-child(4n+2)"
  , "p:nth-child(4n+3)"
  , "p:nth-child(4n+4)"
  , ":nth-child(10n-1)"
  , ":nth-child(10n+9)"
  , "foo:nth-child(0n+5)"
  , "foo:nth-child(5)"
  , "p"
  , "p::first-letter"
  , "span"
  , "h1 em"
  , "div * p"
  , "div p *[href]"
  , "body > p"
  , "div ol>li p"
  , "match + p"
  , "h1.opener + h2"
  , "h1 ~ pre"
  , "*"
  , "LI"
  , "UL LI"
  , "UL OL+LI"
  , "H1 + *[REL=up]"
  , "UL OL LI.red"
  , "LI.red.level"
  , "#x34y"
  , "#s12:not(FOO)"
  , "*"
  , "E"
  , "E[foo]"
  , "E[foo=\"bar\"]"
  , "E[foo~=\"bar\"]"
  , "E[foo^=\"bar\"]"
  , "E[foo$=\"bar\"]"
  , "E[foo*=\"bar\"]"
  , "E[foo|=\"en\"]"
  , "E:root"
  , "E:nth-child(n)"
  , "E:nth-last-child(n)"
  , "E:nth-of-type(n)"
  , "E:nth-last-of-type(n)"
  , "E:first-child"
  , "E:last-child"
  , "E:first-of-type"
  , "E:last-of-type"
  , "E:only-child"
  , "E:only-of-type"
  , "E:empty"
  , "E:link"
  , "E:visited"
  , "E:active"
  , "E:hover"
  , "E:focus"
  , "E:target"
  , "E:lang(fr)"
  , "E:enabled"
  , "E:disabled"
  , "E:checked"
  , "E::first-line"
  , "E::first-letter"
  , "E::before"
  , "E::after"
  , "E.warning"
  , "E#myid"
  , "E:not(s)"
  , "E F"
  , "E > F"
  , "E + F"
  , "E ~ F"
  , ".class"
  , ".class1.class2"
  , ".class1 .class2"
  , "#id"
  , "*"
  , "element"
  , "element.class"
  , "element,element"
  , "element element"
  , "element>element"
  , "element+element"
  , "element1~element2"
  , "[attribute]"
  , "[attribute=value]"
  , "[attribute~=value]"
  , "[attribute|=value]"
  , "[attribute^=value]"
  , "[attribute$=value]"
  , "[attribute*=value]"
  , ":active"
  , "::after"
  , "::before"
  , ":checked"
  , ":default"
  , ":disabled"
  , ":empty"
  , ":enabled"
  , ":first-child"
  , "::first-letter"
  , "::first-line"
  , ":first-of-type"
  , ":focus"
  , ":fullscreen"
  , ":hover"
  , ":in-range"
  , ":indeterminate"
  , ":invalid"
  , ":lang(language)"
  , ":last-child"
  , ":last-of-type"
  , ":link"
  , "::marker"
  , ":not(selector)"
  , ":nth-child(n)"
  , ":nth-last-child(n)"
  , ":nth-last-of-type(n)"
  , ":nth-of-type(n)"
  , ":only-of-type"
  , ":only-child"
  , ":optional"
  , ":out-of-range"
  , "::placeholder"
  , ":read-only"
  , ":read-write"
  , ":required"
  , ":root"
  , "::selection"
  , ":target"
  , ":valid"
  , ":visited"
  ]
