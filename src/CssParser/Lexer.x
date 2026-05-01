--vim:ft=haskell
{
module CssParser.Lexer where

import Control.Monad ((<=<))
import CssParser.At.Page
import CssParser.Fun
import CssParser.Rule
import CssParser.Rule.Pseudo hiding (Left, Right, ViewTransition)
import CssParser.Rule.Pseudo qualified as P
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
@dec     = [0-9]
@int     = @dec+
@uint    = @dec+
@hexdig  = [0-9a-fA-F]
@updig  = [0-9a-fA-F\?]
@hexdigs = @hexdig+
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
@starting = @s@t@a@r@t@i@n@g
@selector = @s@e@l@e@c@t@o@r
@supports = @s@u@p@p@o@r@t@s
@scope   = @s@c@o@p@e
@view    = @v@i@e@w
@active  = @a@c@t@i@v@e
@transition = @t@r@a@n@s@i@t@i@o@n
@position = @p@o@s@i@t@i@o@n
@try     = @t@r@y
@container = @c@o@n@t@a@i@n@e@r
@firsth  = @f@i@r@s@t@hyphen
@nthh    = @n@t@h@hyphen
@onlyh   = @o@n@l@y@hyphen
@child   = @c@h@i@l@d
@oftype  = @o@f@hyphen@t@y@p@e
@lasth   = @l@a@s@t@hyphen
@palette = @p@a@l@e@t@t@e
@values  = @v@a@l@u@e@s
@feature = @f@e@a@t@u@r@e
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

@to      = @t@o
@is      = @i@s
@has     = @h@a@s
@not     = @n@o@t
@where   = @w@h@e@r@e
@and     = @a@n@d
@or      = @o@r
@only    = @o@n@l@y

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
@rem     = @r@em
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
  (@wo ";" @wo)+                          { constoken Semicolon }

  @unicode "-" @range                     { constoken UnicodeRangeT }
  @src                                    { constoken SrcPropT }
  "@"                                     { constoken AtT }
  "@" @font "-" @face                     { constoken FontFaceT }
  "@" @position "-" @try                  { constoken PositionTryT }
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

  "@" @supports                           { constoken SupportsT }
  "@" @scope                              { constoken ScopeT }
  "@" @view "-" @transition               { constoken ViewTransitionT }
  "@" @starting "-" @style                { constoken StartingStyleT }
  "@" @container                          { constoken ContainerT }
  "@" @font "-" @palette "-" @values      { constoken FontPaletteValuesT }
  "@" @font "-" @feature "-" @values      { constoken FontFeatureValuesT }
  "@" @color "-" @profile                 { constoken ColorProfileT }
  @wo "@" @property $w @wo                { constoken PropertyT }
  @wo "@" @counter "-" @style $w @wo      { constoken CounterStyleT }
  @wo "@" @charset $w @wo                 { constoken CharsetT }
  @wo "@" @namespace $w @wo               { constoken NamespaceT }

  @wo "@" @import $w @wo                  { constoken ImportT }
  @wo "@" @keyframes $w @wo               { constoken KeyframesT }
  @wo "@" @layer $w @wo                   { constoken LayerT }
  @wo "@" @media $w @wo                   { constoken MediaT }
  @to                                     { constoken ToT }
  @only @wo                               { constoken OnlyT }
  @not @wo                                { constoken NotT }
  @or @wo                                 { constoken OrT }
  @and @wo                                { constoken AndT }
  @selector "("                           { constoken SelectorFunT }
  @wo @url @wo "("                        { constoken UrlT }
  "."                                     { constoken Dot }
  "*"                                     { constoken Asterisk }
  "&"                                     { constoken Ampersand }
  "|"                                     { constoken Pipe }
  "/"                                     { constoken DivT }
  @ident                                  { tokenize (Ident . readIdentifier) }
  @string                                 { tokenize (String . readCssString) }
  "U+" ("?" | "1")? ("?" | "0")? @updig{1,4} ("-" ("?" | "1")? ("?" | "0")? @updig{1,4})?
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
  @wo @uint @rem                          { tokenize (Rem . read . dropEnd 3) }
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
  @pse @a@f@t@e@r                                      { constoken (PseudoElementT After) }
  @pse @b@a@c@k@d@r@o@p                                { constoken (PseudoElementT Backdrop) }
  @pse @b@e@f@o@r@e                                    { constoken (PseudoElementT Before) }
  @pse @c@h@e@c@k@m@a@r@k                              { constoken (PseudoElementT Checkmark) }
  @pse @c@o@l@u@m@n                                    { constoken (PseudoElementT Column) }
  @pse @c@u@e                                          { constoken (PseudoElementT Cue) }
  @pse @d@e@t@a@i@l@s "-" @c@o@n@t@e@n@t               { constoken (PseudoElementT DetailsContent) }
  @pse @f@i@l@e "-" @s@e@l@e@c@t@o@r "-" @b@u@t@t@o@n  { constoken (PseudoElementT FileSelectorButton) }
  @pse @f@i@r@s@t "-" @l@e@t@t@e@r                     { constoken (PseudoElementT FirstLetter) }
  @pse @f@i@r@s@t "-" @l@i@n@e                         { constoken (PseudoElementT FirstLine) }
  @pse @g@r@a@m@m@a@r "-" @e@r@r@o@r                   { constoken (PseudoElementT GrammarError) }
  @pse @m@a@r@k@e@r                                    { constoken (PseudoElementT Marker) }
  @pse @p@i@c@k@e@r "-" @i@c@o@n                       { constoken (PseudoElementT PickerIcon) }
  @pse @p@l@a@c@e@h@o@l@d@e@r                          { constoken (PseudoElementT Placeholder) }
  @pse @s@c@r@o@l@l "-" @m@a@r@k@e@r                   { constoken (PseudoElementT ScrollMarker) }
  @pse @s@c@r@o@l@l "-" @m@a@r@k@e@r "-" @g@r@o@u@p    { constoken (PseudoElementT ScrollMarkerGroup) }
  @pse @s@e@a@r@c@h "-" @t@e@x@t                       { constoken (PseudoElementT SearchText) }
  @pse @s@e@l@e@c@t@i@o@n                              { constoken (PseudoElementT Selection) }
  @pse @s@p@e@l@l@i@n@g "-" @e@r@r@o@r                 { constoken (PseudoElementT SpellingError) }
  @pse @t@a@r@g@e@t "-" @t@e@x@t                       { constoken (PseudoElementT TargetText) }
  @pse @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n               { constoken (PseudoElementT P.ViewTransition) }

  @psc @active                                         { constoken (AtomicPseudoClassT Active) }
  @psc @active "-" @view "-" @t@r@a@n@s@i@t@i@o@n      { constoken (AtomicPseudoClassT ActiveViewTransition) }
  @psc @a@n@y "-" @l@i@s@t                             { constoken (AtomicPseudoClassT AnyList) }
  @psc @a@u@t@o@f@i@l@l                                { constoken (AtomicPseudoClassT Autofill) }
  @psc @b@l@a@n@k                                      { constoken (AtomicPseudoClassT Blank) }
  @psc @b@u@f@f@e@r@i@n@g                              { constoken (AtomicPseudoClassT Buffering) }
  @psc @c@h@e@c@k@e@d                                  { constoken (AtomicPseudoClassT Checked) }
  @psc @c@u@r@r@e@n@t                                  { constoken (AtomicPseudoClassT Current) }
  @psc @d@e@f@a@u@l@t                                  { constoken (AtomicPseudoClassT Default) }
  @psc @d@e@f@i@n@e@d                                  { constoken (AtomicPseudoClassT Defined) }
  @psc @d@i@s@a@b@l@e@d                                { constoken (AtomicPseudoClassT Disabled) }
  @psc @e@m@p@t@y                                      { constoken (AtomicPseudoClassT Empty) }
  @psc @e@n@a@b@l@e@d                                  { constoken (AtomicPseudoClassT Enabled) }
  @psc @f@i@r@s@t                                      { constoken (AtomicPseudoClassT First) }
  @psc @f@i@r@s@t "-" @c@h@i@l@d                       { constoken (AtomicPseudoClassT FirstChild) }
  @psc @f@i@r@s@t "-" @o@f "-" @t@y@p@e                { constoken (AtomicPseudoClassT FirstOfType) }
  @psc @f@o@c@u@s                                      { constoken (AtomicPseudoClassT Focus) }
  @psc @f@o@c@u@s "-" @v@i@s@i@b@l@e                   { constoken (AtomicPseudoClassT FocusVisible) }
  @psc @f@o@c@u@s "-" @w@i@t@h@i@n                     { constoken (AtomicPseudoClassT FocusWithin) }
  @psc @f@u@l@l@s@c@r@e@e@n                            { constoken (AtomicPseudoClassT Fullscreen) }
  @psc @f@u@t@u@r@e                                    { constoken (AtomicPseudoClassT Future) }
  @psc @h@a@s "-" @s@l@o@t@t@e@d                       { constoken (AtomicPseudoClassT HasSlotted) }
  @psc @h@e@a@d@i@n@g                                  { constoken (AtomicPseudoClassT Heading) }
  @psc @h@o@s@t                                        { constoken (AtomicPseudoClassT Host) }
  @psc @h@o@v@e@r                                      { constoken (AtomicPseudoClassT Hover) }
  @psc @i@n@d@e@t@e@r@m@i@n@a@t@e                      { constoken (AtomicPseudoClassT Indeterminate) }
  @psc @i@n "-" @r@a@n@g@e                             { constoken (AtomicPseudoClassT InRange) }
  @psc @i@n@t@e@r@e@s@t "-" @s@o@u@r@c@e               { constoken (AtomicPseudoClassT InterestSource) }
  @psc @i@n@t@e@r@e@s@t "-" @t@a@r@g@e@t               { constoken (AtomicPseudoClassT InterestTarget) }
  @psc @i@n@v@a@l@i@d                                  { constoken (AtomicPseudoClassT Invalid) }
  @psc @l@a@s@t "-" @c@h@i@l@d                         { constoken (AtomicPseudoClassT LastChild) }
  @psc @l@a@s@t "-" @o@f "-" @t@y@p@e                  { constoken (AtomicPseudoClassT LastOfType) }
  @psc @l@e@f@t                                        { constoken (AtomicPseudoClassT P.Left) }
  @psc @l@i@n@k                                        { constoken (AtomicPseudoClassT Link) }
  @psc @l@o@c@a@l "-" @l@i@n@k                         { constoken (AtomicPseudoClassT LocalLink) }
  @psc @m@o@d@a@l                                      { constoken (AtomicPseudoClassT Modal) }
  @psc @m@u@t@e@d                                      { constoken (AtomicPseudoClassT Muted) }
  @psc @o@n@l@y "-" @c@h@i@l@d                         { constoken (AtomicPseudoClassT OnlyChild) }
  @psc @o@n@l@y "-" @o@f "-" @t@y@p@e                  { constoken (AtomicPseudoClassT OnlyOfType) }
  @psc @o@p@e@n                                        { constoken (AtomicPseudoClassT Open) }
  @psc @o@p@t@i@o@n@a@l                                { constoken (AtomicPseudoClassT Optional) }
  @psc @o@u@t "-" @o@f "-" @r@a@n@g@e                  { constoken (AtomicPseudoClassT OutOfRange) }
  @psc @p@a@s@t                                        { constoken (AtomicPseudoClassT Past) }
  @psc @p@a@u@s@e@d                                    { constoken (AtomicPseudoClassT Paused) }
  @psc @p@i@c@t@u@r@e "-" @i@n "-" @p@i@c@t@u@r@e      { constoken (AtomicPseudoClassT PictureInPicture) }
  @psc @p@l@a@c@e@h@o@l@d@e@r "-" @s@h@o@w@n           { constoken (AtomicPseudoClassT PlaceholderShown) }
  @psc @p@l@a@y@i@n@g                                  { constoken (AtomicPseudoClassT Playing) }
  @psc @p@o@p@o@v@e@r "-" @o@p@e@n                     { constoken (AtomicPseudoClassT PopoverOpen) }
  @psc @r@e@a@d "-" @o@n@l@y                           { constoken (AtomicPseudoClassT ReadOnly) }
  @psc @r@e@a@d "-" @w@r@i@t@e                         { constoken (AtomicPseudoClassT ReadWrite) }
  @psc @r@e@q@u@i@r@e@d                                { constoken (AtomicPseudoClassT Required) }
  @psc @r@i@g@h@t                                      { constoken (AtomicPseudoClassT P.Right) }
  @psc @r@o@o@t                                        { constoken (AtomicPseudoClassT Root) }
  @psc @s@c@o@p@e                                      { constoken (AtomicPseudoClassT Scope) }
  @psc @s@e@e@k@i@n@g                                  { constoken (AtomicPseudoClassT Seeking) }
  @psc @s@t@a@l@l@e@d                                  { constoken (AtomicPseudoClassT Stalled) }
  @psc @t@a@r@g@e@t                                    { constoken (AtomicPseudoClassT Target) }
  @psc @t@a@r@g@e@t "-" @a@f@t@e@r                     { constoken (AtomicPseudoClassT TargetAfter) }
  @psc @t@a@r@g@e@t "-" @b@e@f@o@r@e                   { constoken (AtomicPseudoClassT TargetBefore) }
  @psc @t@a@r@g@e@t "-" @c@u@r@r@e@n@t                 { constoken (AtomicPseudoClassT TargetCurrent) }
  @psc @u@s@e@r "-" @i@n@v@a@l@i@d                     { constoken (AtomicPseudoClassT UserInvalid) }
  @psc @u@s@e@r "-" @v@a@l@i@d                         { constoken (AtomicPseudoClassT UserValid) }
  @psc @v@a@l@i@d                                      { constoken (AtomicPseudoClassT Valid) }
  @psc @v@i@s@i@t@e@d                                  { constoken (AtomicPseudoClassT Visited) }
  @psc @v@o@l@u@m@e "-" @l@o@c@k@e@d                   { constoken (AtomicPseudoClassT VolumeLocked) }
  @psc @x@r "-" @o@v@e@r@l@a@y                         { constoken (AtomicPseudoClassT XrOverlay) }

  @psc @l@a@n@g "("                       { constAndBegin TLang lang_state }
  @psc @nthh@child "("                    { constAndBegin (PseudoFunction NthFChild) nth_state }
  @psc @nthh@lasth@child "("              { constAndBegin (PseudoFunction NthFLastChild) nth_state }
  @psc @nthh@lasth@oftype "("             { constAndBegin (PseudoFunction NthFLastOfType) nth_state }
  @psc @nthh@oftype "("                   { constAndBegin (PseudoFunction NthFOfType) nth_state }
  @psc @not                               { constoken TNot }
  @psc @where                             { constoken TWhere }
  @psc @has                               { constoken THas }
  @psc @is                                { constoken TIs }
  @wo ")"                                 { constoken TClose }
  "("                                     { constoken TOpen }

  @psc @wo                                { constoken Colon }
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
    | FontFeatureValuesT
    | FontPaletteValuesT
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
    | Rem Integer
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

    | SelectorFunT
    | SupportsT
    | ScopeT
    | ViewTransitionT
    | StartingStyleT
    | PositionTryT
    | ContainerT
    | ToT
    | AtT
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
    | DivT
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
    | PseudoFunction NthF
    | PseudoElementT PseudoElement
    | TN
    | TNth Nth
    | TPM TpmF
    | TInt Int
    | TOpen
    | TClose
    | TNot
    | TWhere
    | TIs
    | THas
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
