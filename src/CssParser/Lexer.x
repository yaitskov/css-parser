--vim:ft=haskell
{
module CssParser.Lexer where

import Control.Monad ((<=<))
import CssParser.At.Page
import CssParser.Fun
import CssParser.Rule
import CssParser.Rule.Pseudo
import CssParser.Rule.Value (Ratio(..), readRatio)
import CssParser.Utils(readCssString, readIdentifier, readDecimalE, dropEnd)
import Data.Decimal(Decimal)
import Data.Text (pack)
import Prelude
import Text.Read (readEither)
}

%wrapper "monadUserState"

$nonascii = [^\0-\xff]
$w        = [\ \t\r\n\f]
$tl       = [\~]
$pm       = [\-\+]

@nl       = \r|\n|\r\n|\f
@unicode  = \\[0-9a-fA-F]{1,6}(\r\n|[ \n\r\t\f])?
@escape   = @unicode | \\[^\n\r\f0-9a-fA-F]
@wo = $w*
@nonaesc = $nonascii | @escape
@nmstart = [_a-zA-Z] | @nonaesc
@nmchar  = [_\-a-zA-Z0-9] | @nonaesc
@ident   = [\-]? @nmstart @nmchar*
@name    = @nmchar+
@int     = [0-9]+
@uint    = [0-9]+
@hexdig  = [0-9a-fA-F]+
@float   = [0-9]*[\.][0-9]+
@string1 = \'([^\n\r\f\\\'] | \\@nl | @nonaesc )*\'   -- strings with single quote
@string2 = \"([^\n\r\f\\\"] | \\@nl | @nonaesc )*\"   -- strings with double quotes
@string  = @string1 | @string2

@a       = a|A|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?
@b       = b|B|\\0{0,4}(42|62)(\r\n|[ \t\r\n\f])?
@c       = c|C|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?
@d       = d|D|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?
@e       = e|E|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?
@f       = f|F|\\0{0,4}(46|66)(\r\n|[ \t\r\n\f])?
@g       = g|G|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?
@h       = h|H|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?
@i       = i|I|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?
@j       = j|J|\\0{0,4}(4a|6a)(\r\n|[ \t\r\n\f])?
@k       = k|K|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?
@l       = l|L|\\0{0,4}(4c|6c)(\r\n|[ \t\r\n\f])?
@m       = m|M|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?
@n       = n|N|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n
@o       = o|O|\\0{0,4}(4f|6f)(\r\n|[ \t\r\n\f])?|\\o
@p       = p|P|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?
@q       = q|Q|\\0{0,4}(51|71)(\r\n|[ \t\r\n\f])?
@r       = r|R|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?
@s       = s|S|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?
@t       = t|T|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t
@u       = u|U|\\0{0,4}(55|75)(\r\n|[ \t\r\n\f])?
@v       = v|V|\\0{0,4}(56|76)(\r\n|[ \t\r\n\f])?|\\v
@w       = w|W|\\0{0,4}(57|77)(\r\n|[ \t\r\n\f])?
@x       = x|X|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?
@y       = y|Y|\\0{0,4}(59|79)(\r\n|[ \t\r\n\f])?
@z       = z|Z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?
@hyphen  = [\-]|\\0{0,4}2d
@var     = [\-][\-]

@firsth  = @f@i@r@s@t@hyphen
@nthh    = @n@t@h@hyphen
@onlyh   = @o@n@l@y@hyphen
@child   = @c@h@i@l@d
@oftype  = @o@f@hyphen@t@y@p@e
@lasth   = @l@a@s@t@hyphen

@unicode = @u@n@i@c@o@d@e
@range   = @r@a@n@g@e
@src     = @s@r@c
@font    = @f@o@n@t
@face    = @f@a@c@e
@keyframes = @k@e@y@f@r@a@m@e@s
@charset = @c@h@a@r@s@e@t
@import  = @i@m@p@o@r@t
@style   = @s@t@y@l@e
@counter = @c@o@u@n@t@e@r
@namespace = @n@a@m@e@s@p@a@c@e
@layer   = @l@a@y@e@r
@media   = @m@e@d@i@a
@property = @p@r@o@p@e@r@t@y
@page    = @p@a@g@e
@top     = @t@o@p
@bottom  = @b@o@t@t@o@m
@right   = @r@i@g@h@t
@left    = @l@e@f@t
@center  = @c@e@n@t@e@r
@corner  = @c@o@r@n@e@r
@middle  = @m@i@d@d@l@e

@first   = @f@i@r@s@t
@blank   = @b@l@a@n@k
@color   = @c@o@l@o@r
@profile = @p@r@o@f@i@l@e

@not     = @n@o@t
@and     = @a@n@d
@or     =  @o@r
@only   = @o@n@l@y

@url     = @u@r@l

@cmo     = \/\*
@cmc     = \*\/
@psc     = [:]
@pse     = [:][:]
@psb     = [:][:]?
@lang    = [A-Za-z\-]+

@deg     = @d@e@g
@rad     = @r@a@d
@grad    = @g@r@a@d
@turn    = @t@u@r@n

@mm      = @m@m
@px      = @p@x
@cm      = @c@m
@em      = @e@m
@vh      = @v@h
@vw      = @v@w
@dpi     = @d@p@i
@percent = \%

tokens :-
 <0> {
  @wo "=" @wo                             { constoken TEqual }
  @wo "~=" @wo                            { constoken TIncludes }
  @wo "|=" @wo                            { constoken TDashMatch }
  @wo "^=" @wo                            { constoken TPrefixMatch }
  @wo "$=" @wo                            { constoken TSuffixMatch }
  @wo "*=" @wo                            { constoken TSubstringMatch }
  @wo ","  @wo                            { constoken Comma }
  @psc @wo                                { constoken Colon }
  (@wo ";" @wo)+                          { constoken Semicolon }

  @unicode "-" @range                     { constoken UnicodeRangeT }
  @src                                    { constoken SrcPropT }
  "@" @font "-" @face                     { constoken FontFaceT }
  @wo "@" @page $w @wo                    { constoken PageT }

  @wo "@" @top "-" @left "-" @corner      { constoken (PageMarginT TopLeftCorner) }
  @wo "@" @bottom "-" @right "-" @corner  { constoken (PageMarginT BottomRightCorner) }
  @wo "@" @top "-" @right "-" @corner     { constoken (PageMarginT TopRightCorner) }
  @wo "@" @bottom "-" @left "-" @corner   { constoken (PageMarginT BottomLeftCorner) }

  @wo "@" @top "-" @left                  { constoken (PageMarginT TopLeft) }
  @wo "@" @top "-" @center                { constoken (PageMarginT TopCenter) }
  @wo "@" @top "-" @right                 { constoken (PageMarginT TopRight) }
  @wo "@" @bottom "-" @left               { constoken (PageMarginT BottomLeft) }
  @wo "@" @bottom "-" @center             { constoken (PageMarginT BottomCenter) }
  @wo "@" @bottom "-" @right              { constoken (PageMarginT BottomRight) }

  @wo "@" @left "-" @top                  { constoken (PageMarginT LeftTop) }
  @wo "@" @left "-" @middle               { constoken (PageMarginT LeftMiddle) }
  @wo "@" @left "-" @bottom               { constoken (PageMarginT LeftBottom) }
  @wo "@" @right "-" @top                 { constoken (PageMarginT RightTop) }
  @wo "@" @right "-" @middle              { constoken (PageMarginT RightMiddle) }
  @wo "@" @right "-" @bottom              { constoken (PageMarginT RightBottom) }

  "@" @color "-" @profile                 { constoken ColorProfileT }
  @wo "@" @property $w @wo                { constoken PropertyT }
  @wo "@" @counter "-" @style $w @wo      { constoken CounterStyleT }
  @wo "@" @charset $w @wo                 { constoken CharsetT }
  @wo "@" @namespace $w @wo               { constoken NamespaceT }
  @wo "@" @import $w @wo                  { constoken ImportT }
  @wo "@" @keyframes $w @wo               { constoken KeyframesT }
  @wo "@" @layer $w @wo                   { constoken LayerT }
  @wo "@" @media $w @wo                   { constoken MediaT }
  @only @wo                               { constoken OnlyT }
  @not @wo                                { constoken NotT }
  @or @wo                                 { constoken OrT }
  @and @wo                                { constoken AndT }
  @wo @url @wo "("                        { constoken UrlT }
  "."                                     { constoken Dot }
  "*"                                     { constoken Asterisk }
  "&"                                     { constoken Ampersand }
  "|"                                     { constoken Pipe }
  @ident                                  { tokenize (Ident . readIdentifier) }
  @string                                 { tokenize (String . readCssString) }
  "U+" ("?" | "1"? @hexdig){1,5} ("-" ("?" | "1"? @hexdig){1,5})?
                                          { tokenize (UnicodeRangeVal . drop 2) }
  @var @name                              { tokenize (Var . readIdentifier . drop 2) }
  "#" @name                               { tokenize (THash . readIdentifier . drop 1) }
  @float                                  { tokenizeE Decimal readDecimalE }

  @wo @uint @deg                          { tokenize (Deg . read . dropEnd 3) }
  @wo @uint @rad                          { tokenize (Rad . read . dropEnd 3) }
  @wo @uint @grad                         { tokenize (Grad. read . dropEnd 4) }
  @wo @uint @turn                         { tokenize (Turn . read . dropEnd 4) }

  @wo @uint @px                           { tokenize (Pixels . read . dropEnd 2) }
  @wo @uint @mm                           { tokenize (Mm . read . dropEnd 2) }
  @wo @uint @cm                           { tokenize (Cm . read . dropEnd 2) }
  @wo @uint @em                           { tokenize (Em . read . dropEnd 2) }
  @wo @uint @vh                           { tokenize (Vh . read . dropEnd 2) }
  @wo @uint @vw                           { tokenize (Vw . read . dropEnd 2) }
  @wo @uint @dpi                          { tokenize (Dpi . read . dropEnd 3) }
  @wo @uint "/" @uint                     { tokenize2 ((pure . RatioT) <=< readRatio) }
  @wo @uint @percent                      { tokenize (Percents . read . dropEnd 1) }
  @int                                    { tokenize (Integer . read) }
  @wo "+"                                 { constoken Plus }
  @wo ">" @wo                             { constoken Greater }
  @wo ">=" @wo                            { constoken GreaterEqual }
  @wo "<" @wo                             { constoken Less }
  @wo "<=" @wo                            { constoken LessEqual }
  @wo $tl @wo                             { constoken Tilde }
  "[" @wo                                 { constoken BOpen }
  @wo "]"                                 { constoken BClose }
  @wo "{" @wo                             { constoken COpen }
  @wo "}" @wo                             { constoken CClose }
  @psb @a@f@t@e@r                         { constoken (PseudoElementT After) }
  @psb @b@e@f@o@r@e                       { constoken (PseudoElementT Before) }
  @psb @firsth@l@e@t@t@e@r                { constoken (PseudoElementT FirstLetter) }
  @psb @firsth@l@i@n@e                    { constoken (PseudoElementT FirstLine) }
  @pse @m@a@r@k@e@r                       { constoken (PseudoElementT Marker) }
  @pse @p@l@a@c@e@h@o@l@d@e@r             { constoken (PseudoElementT Placeholder) }
  @pse @s@e@l@e@c@t@i@o@n                 { constoken (PseudoElementT Selection) }
  @psc @a@c@t@i@v@e                       { constoken (AtomicPseudoClassT Active) }
  @psc @c@h@e@c@k@e@d                     { constoken (AtomicPseudoClassT Checked) }
  @psc @d@e@f@a@u@l@t                     { constoken (AtomicPseudoClassT Default) }
  @psc @d@i@s@a@b@l@e@d                   { constoken (AtomicPseudoClassT Disabled) }
  @psc @e@m@p@t@y                         { constoken (AtomicPseudoClassT Empty) }
  @psc @e@n@a@b@l@e@d                     { constoken (AtomicPseudoClassT Enabled) }
  @psc @firsth@child                      { constoken (AtomicPseudoClassT FirstChild) }
  @psc @firsth@oftype                     { constoken (AtomicPseudoClassT FirstOfType) }
  @psc @f@o@c@u@s                         { constoken (AtomicPseudoClassT Focus) }
  @psc @f@u@l@l@s@c@r@e@e@n               { constoken (AtomicPseudoClassT Fullscreen) }
  @psc @h@o@v@e@r                         { constoken (AtomicPseudoClassT Hover) }
  @psc @i@n@d@e@t@e@r@m@i@n@a@t@e         { constoken (AtomicPseudoClassT Indeterminate) }
  @psc @i@n@hyphen@r@a@n@g@e              { constoken (AtomicPseudoClassT InRange) }
  @psc @i@n@v@a@l@i@d                     { constoken (AtomicPseudoClassT Invalid) }
  @psc @lasth@child                       { constoken (AtomicPseudoClassT LastChild) }
  @psc @lasth@oftype                      { constoken (AtomicPseudoClassT LastOfType) }
  @psc @l@i@n@k                           { constoken (AtomicPseudoClassT Link) }
  @psc @l@a@n@g "("                       { constAndBegin TLang lang_state }
  @psc @nthh@child "("                    { constAndBegin (PseudoFunction NthFChild) nth_state }
  @psc @nthh@lasth@child "("              { constAndBegin (PseudoFunction NthFLastChild) nth_state }
  @psc @nthh@lasth@oftype "("             { constAndBegin (PseudoFunction NthFLastOfType) nth_state }
  @psc @nthh@oftype "("                   { constAndBegin (PseudoFunction NthFOfType) nth_state }
  @psc @n@o@t "(" @wo                     { constoken TNot }
  @wo ")"                                 { constoken TClose }
  @wo "(" @wo                             { constoken TOpen }
  @psc @onlyh@oftype                      { constoken (AtomicPseudoClassT OnlyOfType) }
  @psc @onlyh@child                       { constoken (AtomicPseudoClassT OnlyChild) }
  @psc @o@p@t@i@o@n@a@l                   { constoken (AtomicPseudoClassT Optional) }
  @psc @o@u@t@hyphen@o@f@hyphen@r@a@n@g@e { constoken (AtomicPseudoClassT OutOfRange) }
  @psc @r@e@a@d@hyphen@o@n@l@y            { constoken (AtomicPseudoClassT ReadOnly) }
  @psc @r@e@a@d@hyphen@w@r@i@t@e          { constoken (AtomicPseudoClassT ReadWrite) }
  @psc @r@e@q@u@i@r@e@d                   { constoken (AtomicPseudoClassT Required) }
  @psc @r@o@o@t                           { constoken (AtomicPseudoClassT Root) }
  @psc @t@a@r@g@e@t                       { constoken (AtomicPseudoClassT Target) }
  @psc @v@a@l@i@d                         { constoken (AtomicPseudoClassT Valid) }
  @psc @v@i@s@i@t@e@d                     { constoken (AtomicPseudoClassT Visited) }
  @psc @left                              { constoken (PseudoPageT LeftPp) }
  @psc @right                             { constoken (PseudoPageT RightPp) }
  @psc @blank                             { constoken (PseudoPageT BlankPp) }
  @psc @first                             { constoken (PseudoPageT FirstPp) }
  $w @wo                                  { constoken Space }
  @cmo                                    { begin comment }
  "<!--"                                  { begin htmlComment }
 }
 <comment> {
  .                                       ;
  @cmc                                    { begin state_initial }
 }
 <htmlComment> {
  .                                       ;
  "-->"                                   { begin state_initial }
 }
 <nth_state> {
  $w @wo                                  { constoken Space }
  @e@v@e@n                                { constoken (TNth Even) }
  @o@d@d                                  { constoken (TNth Odd) }
  @n                                      { constoken TN }
  "+"                                     { constoken (TPM TpmIdF) }
  "-"                                     { constoken (TPM TpmNegF) }
  @int                                    { tokenize (TInt . read) }
  ")"                                     { constAndBegin TClose state_initial }
 }
 <lang_state> {
  @lang                                   { tokenize String }
  $w @wo                                  { skip }
  ")"                                     { constAndBegin TClose state_initial }
 }

{

data TokenLoc = TokenLoc Token String (Maybe AlexPosn) deriving (Show, Eq)

getToken :: TokenLoc -> Token
getToken (TokenLoc t _ _) = t

type AlexUserState = ()

data Token
    = TIncludes
    | TEqual
    | TDashMatch
    | TPrefixMatch
    | TSuffixMatch
    | TSubstringMatch
    | Ident String
    | String String
    | UnicodeRangeVal String
    | UnicodeRangeT
    | Var String
    | THash String
    | Decimal Decimal
    | Integer Integer

    | Deg Integer
    | Rad Integer
    | Grad Integer
    | Turn Integer

    | Pixels Integer
    | Mm Integer
    | Cm Integer
    | Em Integer
    | Vh Integer
    | Vw Integer
    | Dpi Integer

    | RatioT Ratio
    | Percents Integer
    | Comma
    | Ampersand
    | Colon
    | Semicolon
    | Pipe
    | Plus
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Tilde
    | Dot

    | PageT
    | PageMarginT PageMargin

    | SrcPropT
    | FontFaceT
    | NamespaceT
    | ColorProfileT
    | PropertyT
    | CounterStyleT
    | CharsetT
    | ImportT
    | KeyframesT
    | LayerT
    | MediaT
    | OnlyT
    | NotT
    | AndT
    | OrT
    | UrlT
    | Asterisk
    | Space
    | BOpen
    | BClose
    | COpen
    | CClose
    | AtomicPseudoClassT AtomicPseudoClass
    | PseudoPageT PseudoPage
    | PseudoFunction NthF
    | PseudoElementT PseudoElement
    | TN
    | TNth Nth
    | TPM TpmF
    | TInt Int
    | TOpen
    | TClose
    | TNot
    | TLang
    deriving (Show, Eq)



tokenizeE :: (a -> Token) -> (String -> Either String a) -> AlexInput -> Int -> Alex TokenLoc
tokenizeE f fe (p, _, _, str) len =
  case fe str' of
    Left err ->
      alexError ("FAILED " <> err <> "; len: " <> show len <> "; str = [" <> str <> "]")
    Right v ->
      pure (TokenLoc (f v) str' (Just p))
  where str' = take len str

tokenize :: (String -> Token) -> AlexInput -> Int -> Alex TokenLoc
tokenize f (p, _, _, str) len = pure (TokenLoc (f str') str' (Just p))
  where str' = take len str

tokenize2 :: (String -> Either String Token) -> AlexInput -> Int -> Alex TokenLoc
tokenize2 f (p, _, _, str) len =
  case f str' of
    Left err ->
      alexError ("FAILED " <> err <> "; len: " <> show len <> "; str = [" <> str <> "]")
    Right v ->
      pure (TokenLoc v str' (Just p))
  where
    str' = take len str

constoken :: Token -> AlexInput -> Int -> Alex TokenLoc
constoken = tokenize . const

constAndBegin :: Token -> Int -> AlexInput -> Int -> Alex TokenLoc
constAndBegin = andBegin . constoken

state_initial :: Int
state_initial = 0

alexInitUserState :: AlexUserState
alexInitUserState = ()

alexEOF :: Alex TokenLoc
alexEOF = pure (TokenLoc undefined "" Nothing)

alexScanTokens :: String -> Either String [TokenLoc]
alexScanTokens str = runAlex str loop
  where loop :: Alex [TokenLoc]
        loop = alexMonadScan >>= p
        p (TokenLoc _ _ Nothing) = pure []
        p toc = (toc:) <$> loop

}
