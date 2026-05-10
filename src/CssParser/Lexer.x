{
module CssParser.Lexer where

import Control.Monad ((<=<))
import CssParser.At.Function (AtomicCssType)
import CssParser.At.Function qualified as F
import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page
import CssParser.Descriptor
import CssParser.Fun
import CssParser.Ident qualified as I
import CssParser.Lexer.Token
import CssParser.Prelude hiding (Space)
import CssParser.Rule hiding (Heading, Host)
import CssParser.Rule.Pseudo hiding (Left, Right, ViewTransition)
import CssParser.Rule.Pseudo qualified as P
import CssParser.Rule.Value (Ratio(..), readRatio)
import CssParser.TextMarshal
import CssParser.Utils(readCssString, readIdentifier)
import Data.Text (pack)
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

@name    = @nmchar+
@dec     = [0-9]
@int     = @dec+
@uint    = @dec+
@hexdig  = [0-9a-fA-F]
@updig  = [0-9a-fA-F\?]
@hexdigs = @hexdig+
@string1 = \'([^\n\r\f\\\'] | \\@nl | @nonaesc )*\'   -- strings with single quote
@string2 = \"([^\n\r\f\\\"] | \\@nl | @nonaesc )*\"   -- strings with double quotes
@string  = @string1 | @string2

-- @a       = a
-- @b       = b
-- @c       = c
-- @d       = d
-- @e       = e
-- @f       = f
-- @g       = g
-- @h       = h
-- @i       = i
-- @j       = j
-- @k       = k
-- @l       = l
-- @m       = m
-- @n       = n
-- @o       = o
-- @p       = p
-- @q       = q
-- @r       = r
-- @s       = s
-- @t       = t
-- @u       = u|U
-- @v       = v
-- @w       = w
-- @x       = x
-- @y       = y
-- @z       = z

@a       = a|A
@b       = b|B
@c       = c|C
@d       = d|D
@e       = e|E
@f       = f|F
@g       = g|G
@h       = h|H
@i       = i|I
@j       = j|J
@k       = k|K
@l       = l|L
@m       = m|M
@n       = n|N
@o       = o|O
@p       = p|P
@q       = q|Q
@r       = r|R
@s       = s|S
@t       = t|T
@u       = u|U
@v       = v|V
@w       = w|W
@x       = x|X
@y       = y|Y
@z       = z|Z

@moz     = "-" @m@o@z "-"
@webkit  = "-" @w@e@b@k@i@t "-"
@ms      = "-" @m@s "-"
@apple   = "-" @a@p@p@l@e "-"
@opera   = "-" @o "-"

@browserPrefix = [\-](@m@o@z|@w@e@b@k@i@t|@m@s|@o|@a@p@p@l@e)[\-]
@ident   = @browserPrefix? @nmstart @nmchar*


@anum    = [\-\+]? ( @dec+ ([\.]@dec+)? (@e [\-\+]? @dec+)? | [\.]@dec+ )

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
@from    = @f@r@o@m
@is      = @i@s
@has     = @h@a@s
@all     = @a@l@l
@screen  = @s@c@r@e@e@n
@print   = @p@r@i@n@t
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
  \\ "0"                                               ;
  @wo "=" @wo                                          { constoken TEqual }
  @wo "~=" @wo                                         { constoken TIncludes }
  @wo "|=" @wo                                         { constoken TDashMatch }
  @wo "^=" @wo                                         { constoken TPrefixMatch }
  @wo "$=" @wo                                         { constoken TSuffixMatch }
  @wo "*=" @wo                                         { constoken TSubstringMatch }
  @wo ","  @wo                                         { constoken Comma }
  (@wo ";" @wo)+                                       { constoken Semicolon }

  "!" @wo @i@m@p@o@r@t@a@n@t                           { constoken ImportantT }
  @supports                                            { constoken SupportsT }

  "@"                                                  { constoken (AtT I.Na) }
  "@" @moz                                             { constoken (AtT I.Moz) }
  "@" @ms                                              { constoken (AtT I.Microsoft) }
  "@" @apple                                           { constoken (AtT I.Apple) }
  "@" @opera                                           { constoken (AtT I.Opera) }
  "@" @webkit                                          { constoken (AtT I.WebKit) }

  @f@u@n@c@t@i@o@n                                     { constoken FunctionT }
  @font "-" @face                                      { constoken FontFaceT }
  @position "-" @try                                   { constoken PositionTryT }
  @page $w @wo                                         { constoken PageT }

  @top "-" @left "-" @corner                           { constoken (PageMarginT TopLeftCorner) }
  @bottom "-" @right "-" @corner                       { constoken (PageMarginT BottomRightCorner) }
  @top "-" @right "-" @corner                          { constoken (PageMarginT TopRightCorner) }
  @bottom "-" @left "-" @corner                        { constoken (PageMarginT BottomLeftCorner) }

  @top "-" @left                                       { constoken (PageMarginT TopLeft) }
  @top "-" @center                                     { constoken (PageMarginT TopCenter) }
  @top "-" @right                                      { constoken (PageMarginT TopRight) }
  @bottom "-" @left                                    { constoken (PageMarginT BottomLeft) }
  @bottom "-" @center                                  { constoken (PageMarginT BottomCenter) }
  @bottom "-" @right                                   { constoken (PageMarginT BottomRight) }

  @left "-" @top                                       { constoken (PageMarginT LeftTop) }
  @left "-" @middle                                    { constoken (PageMarginT LeftMiddle) }
  @left "-" @bottom                                    { constoken (PageMarginT LeftBottom) }
  @right "-" @top                                      { constoken (PageMarginT RightTop) }
  @right "-" @middle                                   { constoken (PageMarginT RightMiddle) }
  @right "-" @bottom                                   { constoken (PageMarginT RightBottom) }

  @scope                                               { constoken ScopeT }
  @view "-" @transition                                { constoken ViewTransitionT }
  @starting "-" @style                                 { constoken StartingStyleT }
  @container                                           { constoken ContainerT }
  @font "-" @palette "-" @values                       { constoken FontPaletteValuesT }
  @font "-" @feature "-" @values                       { constoken FontFeatureValuesT }
  @color "-" @profile                                  { constoken ColorProfileT }
  @property $w @wo                                     { constoken PropertyT }
  @counter "-" @style $w @wo                           { constoken CounterStyleT }
  @charset $w @wo                                      { constoken CharsetT }
  @namespace $w @wo                                    { constoken NamespaceT }
  @media                                               { constoken MediaT }
  @import $w @wo                                       { constoken ImportT }
  @keyframes $w @wo                                    { constoken KeyframesT }
  @layer                                               { constoken LayerT }
  -- end of at tokens

  @r@e@t@u@r@n@s                                       { constoken ReturnsT }


  @from                                                { constoken FromT }
  @to                                                  { constoken ToT }
  @only @wo                                            { constoken OnlyT }
  @all                                                 { constoken (MediaTypeT AllMt     ) }
  @print                                               { constoken (MediaTypeT Print     ) }
  @screen                                              { constoken (MediaTypeT Screen    ) }
  @t@t@y                                               { constoken (MediaTypeT Tty       ) }
  @t@v                                                 { constoken (MediaTypeT Tv        ) }
  @p@r@o@j@e@c@t@i@o@n                                 { constoken (MediaTypeT Projection) }
  @h@a@n@d@h@e@l@d                                     { constoken (MediaTypeT Handheld  ) }
  @b@r@a@i@l@l@e                                       { constoken (MediaTypeT Braille   ) }
  @e@m@b@o@s@s@e@d                                     { constoken (MediaTypeT Embossed  ) }
  @a@u@r@a@l                                           { constoken (MediaTypeT Aural     ) }
  @s@p@e@e@c@h                                         { constoken (MediaTypeT Speech    ) }

  @not @wo                                             { constoken NotT }
  @or @wo                                              { constoken OrT }
  @and @wo                                             { constoken AndT }
  @selector "("                                        { constoken SelectorFunT }
  @c@a@l@c "("                                         { constoken CalcFunT }
  @t@y@p@e "("                                         { constoken TypeFunT }
  @url "("                                             { constoken UrlT }
  @url "(" @wo [^\"\'][^\)]* ")"                       { tokenize (UnquotedUrlT . readUnquotedUrl) }
  "."                                                  { constoken Dot }
  "." @ident                                           { tokenize (ClassT . readIdentifier . drop 1) }
  "*"                                                  { constoken Asterisk }
  "&"                                                  { constoken Ampersand }
  "|"                                                  { constoken Pipe }
  @wo "/"                                              { constoken DivT }
  @ident                                               { tokenize (IdentT . readIdentifier) }
  @string                                              { tokenize (String . readCssString) }
  @u "+" ("?" | "1")? ("?" | "0")? @updig{1,4} ("-" ("?" | "1")? ("?" | "0")? @updig{1,4})?
                                                       { tokenize (UnicodeRangeVal . drop 2) }
  @var @name                                           { tokenize (Var . readIdentifier . drop 2) }
  "#"                                                  { constoken SharpT }
  "#" @name                                            { tokenize (THash . readIdentifier . drop 1) }

  @anum                                                { tokenize UnitLessNum }
  @anum @c@a@p                                         { tokenize (Cap      . dropEnd 3) }
  @anum @c@h                                           { tokenize (Ch       . dropEnd 2) }
  @anum @c@m                                           { tokenize (Cm       . dropEnd 2) }
  @anum @c@q@b                                         { tokenize (Cqb      . dropEnd 3) }
  @anum @c@q@h                                         { tokenize (Cqh      . dropEnd 3) }
  @anum @c@q@i                                         { tokenize (Cqi      . dropEnd 3) }
  @anum @c@q@m@a@x                                     { tokenize (Cqmax    . dropEnd 5) }
  @anum @c@q@m@i@n                                     { tokenize (Cqmin    . dropEnd 5) }
  @anum @c@q@w                                         { tokenize (Cqw      . dropEnd 3) }
  @anum @d@e@g                                         { tokenize (Deg      . dropEnd 3) }
  @anum @d@p@i                                         { tokenize (Dpi      . dropEnd 3) }
  @anum @d@v@b                                         { tokenize (Dvb      . dropEnd 3) }
  @anum @d@v@h                                         { tokenize (Dvh      . dropEnd 3) }
  @anum @d@v@i                                         { tokenize (Dvi      . dropEnd 3) }
  @anum @d@v@m@a@x                                     { tokenize (Dvmax    . dropEnd 5) }
  @anum @d@v@m@i@n                                     { tokenize (Dvmin    . dropEnd 5) }
  @anum @e@m                                           { tokenize (Em       . dropEnd 2) }
  @anum @e@x                                           { tokenize (Ex       . dropEnd 2) }
  @anum @f@r                                           { tokenize (Fr       . dropEnd 2) }
  @anum @g@r@a@d                                       { tokenize (Grad     . dropEnd 4) }
  @anum @h@z                                           { tokenize (Hz       . dropEnd 2) }
  @anum @i@c                                           { tokenize (Ic       . dropEnd 2) }
  @anum @i@n                                           { tokenize (In       . dropEnd 2) }
  @anum @k@h@z                                         { tokenize (KHz      . dropEnd 3) }
  @anum @l@h                                           { tokenize (Lh       . dropEnd 2) }
  @anum @l@v@b                                         { tokenize (Lvb      . dropEnd 3) }
  @anum @l@v@h                                         { tokenize (Lvh      . dropEnd 3) }
  @anum @l@v@i                                         { tokenize (Lvi      . dropEnd 3) }
  @anum @l@v@m@a@x                                     { tokenize (Lvmax    . dropEnd 5) }
  @anum @l@v@m@i@n                                     { tokenize (Lvmin    . dropEnd 5) }
  @anum @m@m                                           { tokenize (Mm       . dropEnd 2) }
  @anum @m@s                                           { tokenize (Ms       . dropEnd 2) }
  @anum @p@c                                           { tokenize (Pc       . dropEnd 2) }
  @anum @p@t                                           { tokenize (Pt       . dropEnd 2) }
  @anum @percent                                       { tokenize (Percents . dropEnd 1) }
  @anum @p@x                                           { tokenize (Px       . dropEnd 2) }
  @anum @q                                             { tokenize (Q        . dropEnd 1) }
  @anum @r@a@d                                         { tokenize (Rad      . dropEnd 3) }
  @anum @r@c@a@p                                       { tokenize (Rcap     . dropEnd 4) }
  @anum @r@c@h                                         { tokenize (Rch      . dropEnd 3) }
  @anum @r@e@m                                         { tokenize (Rem      . dropEnd 3) }
  @anum @r@e@x                                         { tokenize (Rex      . dropEnd 3) }
  @anum @r@i@c                                         { tokenize (Ric      . dropEnd 3) }
  @anum @r@l@h                                         { tokenize (Rlh      . dropEnd 3) }
  @anum @s                                             { tokenize (Second   . dropEnd 1) }
  @anum @s@v@b                                         { tokenize (Svb      . dropEnd 3) }
  @anum @s@v@h                                         { tokenize (Svh      . dropEnd 3) }
  @anum @s@v@i                                         { tokenize (Svi      . dropEnd 3) }
  @anum @s@v@m@a@x                                     { tokenize (Svmax    . dropEnd 5) }
  @anum @s@v@m@i@n                                     { tokenize (Svmin    . dropEnd 5) }
  @anum @t@u@r@n                                       { tokenize (Turn     . dropEnd 4) }
  @anum @v@b                                           { tokenize (Vb       . dropEnd 2) }
  @anum @v@h                                           { tokenize (Vh       . dropEnd 2) }
  @anum @v@i                                           { tokenize (Vi       . dropEnd 2) }
  @anum @v@m@a@x                                       { tokenize (Vmax     . dropEnd 4) }
  @anum @v@m@i@n                                       { tokenize (Vmin     . dropEnd 4) }
  @anum @v@w                                           { tokenize (Vw       . dropEnd 2) }

  @uint "/" @uint                                      { tokenize2 ((pure . RatioT) <=< readRatio) }
  "+"                                                  { constoken Plus }
  "-"                                                  { constoken Minus }

  @wo "<" @a@n@g@l@e ">"                                   { constoken (SyntaxTypeT F.Angle) }
  @wo "<" @c@o@l@o@r ">"                                   { constoken (SyntaxTypeT F.Color) }
  @wo "<" @c@u@s@t@o@m "-" @i@d@e@n@t ">"                  { constoken (SyntaxTypeT F.CustomIdent) }
  @wo "<" @i@m@a@g@e ">"                                   { constoken (SyntaxTypeT F.Image) }
  @wo "<" @i@n@t@e@g@e@r ">"                               { constoken (SyntaxTypeT F.Integer) }
  @wo "<" @l@e@n@g@t@h ">"                                 { constoken (SyntaxTypeT F.Length) }
  @wo "<" @l@e@n@g@t@h "-" @p@e@r@c@e@n@t@a@g@e ">"        { constoken (SyntaxTypeT F.LengthPercentage) }
  @wo "<" @n@u@m@b@e@r ">"                                 { constoken (SyntaxTypeT F.Number) }
  @wo "<" @p@e@r@c@e@n@t@a@g@e ">"                         { constoken (SyntaxTypeT F.Percentage) }
  @wo "<" @r@e@s@o@l@u@t@i@o@n ">"                         { constoken (SyntaxTypeT F.Resolution) }
  @wo "<" @s@t@r@i@n@g ">"                                 { constoken (SyntaxTypeT F.String) }
  @wo "<" @t@i@m@e ">"                                     { constoken (SyntaxTypeT F.Time) }
  @wo "<" @t@r@a@n@f@o@r@m "-" @f@u@n@c@t@i@o@n ">"        { constoken (SyntaxTypeT F.TranformFunction) }
  @wo "<" @t@r@a@n@f@o@r@m "-" @l@i@s@t ">"                { constoken (SyntaxTypeT F.TranformList) }
  @wo "<" @u@r@l ">"                                       { constoken (SyntaxTypeT F.UrlType) }

  @wo ">" @wo                                          { constoken Greater }
  @wo ">=" @wo                                         { constoken GreaterEqual }
  @wo "<" @wo                                          { constoken Less }
  @wo "<=" @wo                                         { constoken LessEqual }
  @wo $tl @wo                                          { constoken Tilde }
  "[" @wo                                              { constoken BOpen }
  @wo "]"                                              { constoken BClose }
  @wo "{" @wo                                          { constoken COpen }
  @wo "}" @wo                                          { constoken CClose }
  @psb @a@f@t@e@r                                      { constoken (PseudoElementT After) }
  @pse @b@a@c@k@d@r@o@p                                { constoken (PseudoElementT Backdrop) }
  @psb @b@e@f@o@r@e                                    { constoken (PseudoElementT Before) }
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

  @pse  @h@i@g@h@l@i@g@h@t                             { constoken THighlight }
  @pse  @p@a@r@t                                       { constoken TPart }
  @pse  @p@i@c@k@e@r                                   { constoken TPicker }
  @pse  @s@c@r@o@l@l "-" @b@u@t@t@o@n                  { constoken TScrollButton }
  @pse  @s@l@o@t@t@e@d                                 { constoken TSlotted }
  @pse  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @g@r@o@u@p
                                                       { constoken TViewTransitionGroup }
  @pse  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @i@m@a@g@e "-" @p@a@i@r
                                                       { constoken TViewTransitionImagePair }
  @pse  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @n@e@w   { constoken TViewTransitionNew }
  @pse  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @o@l@d   { constoken TViewTransitionOld }

  @pse @ident                                          { tokenize (PseudoElementT . UnknownPe . I.Ident .
                                                                     pack . readIdentifier . drop 2)
                                                       }

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

  @psc @l@a@n@g "("                                    { constAndBegin TLang lang_state }
  @psc @nthh@child "("                                 { constAndBegin (PseudoFunction NthFChild) nth_state }
  @psc @nthh@lasth@child "("                           { constAndBegin (PseudoFunction NthFLastChild) nth_state }
  @psc @nthh@lasth@oftype "("                          { constAndBegin (PseudoFunction NthFLastOfType) nth_state }
  @psc @nthh@oftype "("                                { constAndBegin (PseudoFunction NthFOfType) nth_state }
  @psc @g@l@o@b@a@l "("                                { constoken GlobalT }
  @psc @not "("                                        { constoken TNot }
  @psc @where "("                                      { constoken TWhere }
  @psc @has "("                                        { constoken THas }
  @psc @is "("                                         { constoken TIs }
  @psc @a@c@t@i@v@e "-" @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @t@y@p@e
                                                       { constoken TActiveViewTransitionType }
  @psc @d@i@r "("                                      { constoken TDir }
  @psc @h@e@a@d@i@n@g                                  { constoken THeading }
  @psc @h@o@s@t                                        { constoken THost }
  @psc @s@t@a@t@e "("                                  { constoken TState }
  @psc @browserPrefix @nmstart @nmchar*                { tokenize (AtomicPseudoClassT . UnknownPc .
                                                                    BrowserSpecificIdent . I.Ident .
                                                                    pack . readIdentifier . drop 1)
                                                       }


  @wo ")"                                              { constoken TClose }
  "("                                                  { constoken TOpen }

  @psc @wo                                             { constoken Colon }
  $w @wo                                               { constoken Space }
  @wo @cmo                                             { begin comment }
  "<!--"                                               { begin htmlComment }
-- begin descriptor list
  "--" @name @psc                                      { tokenize (readCustomDescriptor) }
  @moz @name @psc                                      { tokenize (readBpDescriptor I.Moz) }
  @ms @name @psc                                       { tokenize (readBpDescriptor I.Microsoft) }
  @apple @name @psc                                    { tokenize (readBpDescriptor I.Apple) }
  @opera @name @psc                                    { tokenize (readBpDescriptor I.Opera) }
  @webkit @name @psc                                   { tokenize (readBpDescriptor I.WebKit) }

  @a@c@c@e@n@t "-" @c@o@l@o@r @psc                                         { constoken (DescriptorT AccentColorT) }
  @a@l@i@g@n "-" @c@o@n@t@e@n@t @psc                                       { constoken (DescriptorT AlignContentT) }
  @a@l@i@g@n "-" @i@t@e@m@s @psc                                           { constoken (DescriptorT AlignItemsT) }
  @a@l@i@g@n "-" @s@e@l@f @psc                                             { constoken (DescriptorT AlignSelfT) }
  @a@l@i@g@n@m@e@n@t "-" @b@a@s@e@l@i@n@e @psc                             { constoken (DescriptorT AlignmentBaselineT) }
  @a@l@l @psc                                                              { constoken (DescriptorT AllT) }
  @a@n@c@h@o@r "-" @n@a@m@e @psc                                           { constoken (DescriptorT AnchorNameT) }
  @a@n@c@h@o@r "-" @s@c@o@p@e @psc                                         { constoken (DescriptorT AnchorScopeT) }
  @a@n@i@m@a@t@i@o@n "-" @c@o@m@p@o@s@i@t@i@o@n @psc                       { constoken (DescriptorT AnimationCompositionT) }
  @a@n@i@m@a@t@i@o@n "-" @d@e@l@a@y @psc                                   { constoken (DescriptorT AnimationDelayT) }
  @a@n@i@m@a@t@i@o@n "-" @d@i@r@e@c@t@i@o@n @psc                           { constoken (DescriptorT AnimationDirectionT) }
  @a@n@i@m@a@t@i@o@n "-" @d@u@r@a@t@i@o@n @psc                             { constoken (DescriptorT AnimationDurationT) }
  @a@n@i@m@a@t@i@o@n "-" @f@i@l@l "-" @m@o@d@e @psc                        { constoken (DescriptorT AnimationFillModeT) }
  @a@n@i@m@a@t@i@o@n "-" @i@t@e@r@a@t@i@o@n "-" @c@o@u@n@t @psc            { constoken (DescriptorT AnimationIterationCountT) }
  @a@n@i@m@a@t@i@o@n "-" @n@a@m@e @psc                                     { constoken (DescriptorT AnimationNameT) }
  @a@n@i@m@a@t@i@o@n "-" @p@l@a@y "-" @s@t@a@t@e @psc                      { constoken (DescriptorT AnimationPlayStateT) }
  @a@n@i@m@a@t@i@o@n "-" @r@a@n@g@e "-" @e@n@d @psc                        { constoken (DescriptorT AnimationRangeEndT) }
  @a@n@i@m@a@t@i@o@n "-" @r@a@n@g@e "-" @s@t@a@r@t @psc                    { constoken (DescriptorT AnimationRangeStartT) }
  @a@n@i@m@a@t@i@o@n "-" @r@a@n@g@e @psc                                   { constoken (DescriptorT AnimationRangeT) }
  @a@n@i@m@a@t@i@o@n "-" @t@i@m@e@l@i@n@e @psc                             { constoken (DescriptorT AnimationTimelineT) }
  @a@n@i@m@a@t@i@o@n "-" @t@i@m@i@n@g "-" @f@u@n@c@t@i@o@n @psc            { constoken (DescriptorT AnimationTimingFunctionT) }
  @a@n@i@m@a@t@i@o@n @psc                                                  { constoken (DescriptorT AnimationT) }
  @a@p@p@e@a@r@a@n@c@e @psc                                                { constoken (DescriptorT AppearanceT) }
  @a@s@p@e@c@t "-" @r@a@t@i@o @psc                                         { constoken (DescriptorT AspectRatioT) }
  @b@a@c@k@d@r@o@p "-" @f@i@l@t@e@r @psc                                   { constoken (DescriptorT BackdropFilterT) }
  @b@a@c@k@f@a@c@e "-" @v@i@s@i@b@i@l@i@t@y @psc                           { constoken (DescriptorT BackfaceVisibilityT) }
  @b@a@c@k@g@r@o@u@n@d "-" @a@t@t@a@c@h@m@e@n@t @psc                       { constoken (DescriptorT BackgroundAttachmentT) }
  @b@a@c@k@g@r@o@u@n@d "-" @b@l@e@n@d "-" @m@o@d@e @psc                    { constoken (DescriptorT BackgroundBlendModeT) }
  @b@a@c@k@g@r@o@u@n@d "-" @c@l@i@p @psc                                   { constoken (DescriptorT BackgroundClipT) }
  @b@a@c@k@g@r@o@u@n@d "-" @c@o@l@o@r @psc                                 { constoken (DescriptorT BackgroundColorT) }
  @b@a@c@k@g@r@o@u@n@d "-" @i@m@a@g@e @psc                                 { constoken (DescriptorT BackgroundImageT) }
  @b@a@c@k@g@r@o@u@n@d "-" @o@r@i@g@i@n @psc                               { constoken (DescriptorT BackgroundOriginT) }
  @b@a@c@k@g@r@o@u@n@d "-" @p@o@s@i@t@i@o@n "-" @x @psc                    { constoken (DescriptorT BackgroundPositionXT) }
  @b@a@c@k@g@r@o@u@n@d "-" @p@o@s@i@t@i@o@n "-" @y @psc                    { constoken (DescriptorT BackgroundPositionYT) }
  @b@a@c@k@g@r@o@u@n@d "-" @p@o@s@i@t@i@o@n @psc                           { constoken (DescriptorT BackgroundPositionT) }
  @b@a@c@k@g@r@o@u@n@d "-" @r@e@p@e@a@t @psc                               { constoken (DescriptorT BackgroundRepeatT) }
  @b@a@c@k@g@r@o@u@n@d "-" @r@e@p@e@a@t "-" @x @psc                        { constoken (DescriptorT BackgroundRepeatXT) }
  @b@a@c@k@g@r@o@u@n@d "-" @r@e@p@e@a@t "-" @y @psc                        { constoken (DescriptorT BackgroundRepeatYT) }
  @b@a@c@k@g@r@o@u@n@d "-" @s@i@z@e @psc                                   { constoken (DescriptorT BackgroundSizeT) }
  @b@a@c@k@g@r@o@u@n@d @psc                                                { constoken (DescriptorT BackgroundT) }
  @b@a@s@e "-" @p@a@l@e@t@t@e @psc                                         { constoken (DescriptorT BasePaletteT) }
  @b@a@s@e@l@i@n@e "-" @s@h@i@f@t @psc                                     { constoken (DescriptorT BaselineShiftT) }
  @b@a@s@e@l@i@n@e "-" @s@o@u@r@c@e @psc                                   { constoken (DescriptorT BaselineSourceT) }
  @b@l@o@c@k "-" @s@i@z@e @psc                                             { constoken (DescriptorT BlockSizeT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @c@o@l@o@r @psc                          { constoken (DescriptorT BorderBlockColorT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @e@n@d "-" @c@o@l@o@r @psc               { constoken (DescriptorT BorderBlockEndColorT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @e@n@d "-" @s@t@y@l@e @psc               { constoken (DescriptorT BorderBlockEndStyleT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @e@n@d "-" @w@i@d@t@h @psc               { constoken (DescriptorT BorderBlockEndWidthT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @e@n@d @psc                              { constoken (DescriptorT BorderBlockEndT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @s@t@a@r@t "-" @c@o@l@o@r @psc           { constoken (DescriptorT BorderBlockStartColorT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @s@t@a@r@t "-" @s@t@y@l@e @psc           { constoken (DescriptorT BorderBlockStartStyleT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @s@t@a@r@t "-" @w@i@d@t@h @psc           { constoken (DescriptorT BorderBlockStartWidthT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @s@t@a@r@t @psc                          { constoken (DescriptorT BorderBlockStartT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @s@t@y@l@e @psc                          { constoken (DescriptorT BorderBlockStyleT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k "-" @w@i@d@t@h @psc                          { constoken (DescriptorT BorderBlockWidthT) }
  @b@o@r@d@e@r "-" @b@l@o@c@k @psc                                         { constoken (DescriptorT BorderBlockT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m "-" @c@o@l@o@r @psc                        { constoken (DescriptorT BorderBottomColorT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m "-" @l@e@f@t "-" @r@a@d@i@u@s @psc         { constoken (DescriptorT BorderBottomLeftRadiusT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m "-" @r@i@g@h@t "-" @r@a@d@i@u@s @psc       { constoken (DescriptorT BorderBottomRightRadiusT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m "-" @s@t@y@l@e @psc                        { constoken (DescriptorT BorderBottomStyleT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m "-" @w@i@d@t@h @psc                        { constoken (DescriptorT BorderBottomWidthT) }
  @b@o@r@d@e@r "-" @b@o@t@t@o@m @psc                                       { constoken (DescriptorT BorderBottomT) }
  @b@o@r@d@e@r "-" @c@o@l@l@a@p@s@e @psc                                   { constoken (DescriptorT BorderCollapseT) }
  @b@o@r@d@e@r "-" @c@o@l@o@r @psc                                         { constoken (DescriptorT BorderColorT) }
  @b@o@r@d@e@r "-" @e@n@d "-" @e@n@d "-" @r@a@d@i@u@s @psc                 { constoken (DescriptorT BorderEndEndRadiusT) }
  @b@o@r@d@e@r "-" @e@n@d "-" @s@t@a@r@t "-" @r@a@d@i@u@s @psc             { constoken (DescriptorT BorderEndStartRadiusT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e "-" @o@u@t@s@e@t @psc                        { constoken (DescriptorT BorderImageOutsetT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e "-" @r@e@p@e@a@t @psc                        { constoken (DescriptorT BorderImageRepeatT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e "-" @s@l@i@c@e @psc                          { constoken (DescriptorT BorderImageSliceT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e "-" @s@o@u@r@c@e @psc                        { constoken (DescriptorT BorderImageSourceT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e "-" @w@i@d@t@h @psc                          { constoken (DescriptorT BorderImageWidthT) }
  @b@o@r@d@e@r "-" @i@m@a@g@e @psc                                         { constoken (DescriptorT BorderImageT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @c@o@l@o@r @psc                        { constoken (DescriptorT BorderInlineColorT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @e@n@d "-" @c@o@l@o@r @psc             { constoken (DescriptorT BorderInlineEndColorT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @e@n@d "-" @s@t@y@l@e @psc             { constoken (DescriptorT BorderInlineEndStyleT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @e@n@d "-" @w@i@d@t@h @psc             { constoken (DescriptorT BorderInlineEndWidthT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @e@n@d @psc                            { constoken (DescriptorT BorderInlineEndT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @s@t@a@r@t "-" @c@o@l@o@r @psc         { constoken (DescriptorT BorderInlineStartColorT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @s@t@a@r@t "-" @s@t@y@l@e @psc         { constoken (DescriptorT BorderInlineStartStyleT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @s@t@a@r@t "-" @w@i@d@t@h @psc         { constoken (DescriptorT BorderInlineStartWidthT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc                        { constoken (DescriptorT BorderInlineStartT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @s@t@y@l@e @psc                        { constoken (DescriptorT BorderInlineStyleT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e "-" @w@i@d@t@h @psc                        { constoken (DescriptorT BorderInlineWidthT) }
  @b@o@r@d@e@r "-" @i@n@l@i@n@e @psc                                       { constoken (DescriptorT BorderInlineT) }
  @b@o@r@d@e@r "-" @l@e@f@t "-" @c@o@l@o@r @psc                            { constoken (DescriptorT BorderLeftColorT) }
  @b@o@r@d@e@r "-" @l@e@f@t "-" @s@t@y@l@e @psc                            { constoken (DescriptorT BorderLeftStyleT) }
  @b@o@r@d@e@r "-" @l@e@f@t "-" @w@i@d@t@h @psc                            { constoken (DescriptorT BorderLeftWidthT) }
  @b@o@r@d@e@r "-" @l@e@f@t @psc                                           { constoken (DescriptorT BorderLeftT) }
  @b@o@r@d@e@r "-" @r@a@d@i@u@s @psc                                       { constoken (DescriptorT BorderRadiusT) }
  @b@o@r@d@e@r "-" @r@i@g@h@t "-" @c@o@l@o@r @psc                          { constoken (DescriptorT BorderRightColorT) }
  @b@o@r@d@e@r "-" @r@i@g@h@t "-" @s@t@y@l@e @psc                          { constoken (DescriptorT BorderRightStyleT) }
  @b@o@r@d@e@r "-" @r@i@g@h@t "-" @w@i@d@t@h @psc                          { constoken (DescriptorT BorderRightWidthT) }
  @b@o@r@d@e@r "-" @r@i@g@h@t @psc                                         { constoken (DescriptorT BorderRightT) }
  @b@o@r@d@e@r "-" @s@p@a@c@i@n@g @psc                                     { constoken (DescriptorT BorderSpacingT) }
  @b@o@r@d@e@r "-" @s@t@a@r@t "-" @e@n@d "-" @r@a@d@i@u@s @psc             { constoken (DescriptorT BorderStartEndRadiusT) }
  @b@o@r@d@e@r "-" @s@t@a@r@t "-" @s@t@a@r@t "-" @r@a@d@i@u@s @psc         { constoken (DescriptorT BorderStartStartRadiusT) }
  @b@o@r@d@e@r "-" @s@t@y@l@e @psc                                         { constoken (DescriptorT BorderStyleT) }
  @b@o@r@d@e@r "-" @t@o@p "-" @c@o@l@o@r @psc                              { constoken (DescriptorT BorderTopColorT) }
  @b@o@r@d@e@r "-" @t@o@p "-" @l@e@f@t "-" @r@a@d@i@u@s @psc               { constoken (DescriptorT BorderTopLeftRadiusT) }
  @b@o@r@d@e@r "-" @t@o@p "-" @r@i@g@h@t "-" @r@a@d@i@u@s @psc             { constoken (DescriptorT BorderTopRightRadiusT) }
  @b@o@r@d@e@r "-" @t@o@p "-" @s@t@y@l@e @psc                              { constoken (DescriptorT BorderTopStyleT) }
  @b@o@r@d@e@r "-" @t@o@p "-" @w@i@d@t@h @psc                              { constoken (DescriptorT BorderTopWidthT) }
  @b@o@r@d@e@r "-" @t@o@p @psc                                             { constoken (DescriptorT BorderTopT) }
  @b@o@r@d@e@r "-" @w@i@d@t@h @psc                                         { constoken (DescriptorT BorderWidthT) }
  @b@o@r@d@e@r @psc                                                        { constoken (DescriptorT BorderT) }
  @b@o@t@t@o@m @psc                                                        { constoken (DescriptorT BottomT) }
  @b@o@x "-" @a@l@i@g@n @psc                                               { constoken (DescriptorT BoxAlignT) }
  @b@o@x "-" @d@e@c@o@r@a@t@i@o@n "-" @b@r@e@a@k @psc                      { constoken (DescriptorT BoxDecorationBreakT) }
  @b@o@x "-" @d@i@r@e@c@t@i@o@n @psc                                       { constoken (DescriptorT BoxDirectionT) }
  @b@o@x "-" @f@l@e@x "-" @g@r@o@u@p @psc                                  { constoken (DescriptorT BoxFlexGroupT) }
  @b@o@x "-" @f@l@e@x @psc                                                 { constoken (DescriptorT BoxFlexT) }
  @b@o@x "-" @l@i@n@e@s @psc                                               { constoken (DescriptorT BoxLinesT) }
  @b@o@x "-" @o@r@d@i@n@a@l "-" @g@r@o@u@p @psc                            { constoken (DescriptorT BoxOrdinalGroupT) }
  @b@o@x "-" @o@r@i@e@n@t @psc                                             { constoken (DescriptorT BoxOrientT) }
  @b@o@x "-" @p@a@c@k @psc                                                 { constoken (DescriptorT BoxPackT) }
  @b@o@x "-" @s@h@a@d@o@w @psc                                             { constoken (DescriptorT BoxShadowT) }
  @b@o@x "-" @s@i@z@i@n@g @psc                                             { constoken (DescriptorT BoxSizingT) }
  @b@r@e@a@k "-" @a@f@t@e@r @psc                                           { constoken (DescriptorT BreakAfterT) }
  @b@r@e@a@k "-" @b@e@f@o@r@e @psc                                         { constoken (DescriptorT BreakBeforeT) }
  @b@r@e@a@k "-" @i@n@s@i@d@e @psc                                         { constoken (DescriptorT BreakInsideT) }
  @c@a@p@t@i@o@n "-" @s@i@d@e @psc                                         { constoken (DescriptorT CaptionSideT) }
  @c@a@r@e@t "-" @a@n@i@m@a@t@i@o@n @psc                                   { constoken (DescriptorT CaretAnimationT) }
  @c@a@r@e@t "-" @c@o@l@o@r @psc                                           { constoken (DescriptorT CaretColorT) }
  @c@a@r@e@t "-" @s@h@a@p@e @psc                                           { constoken (DescriptorT CaretShapeT) }
  @c@a@r@e@t @psc                                                          { constoken (DescriptorT CaretT) }
  @c@l@e@a@r @psc                                                          { constoken (DescriptorT ClearT) }
  @c@l@i@p "-" @p@a@t@h @psc                                               { constoken (DescriptorT ClipPathT) }
  @c@l@i@p "-" @r@u@l@e @psc                                               { constoken (DescriptorT ClipRuleT) }
  @c@l@i@p @psc                                                            { constoken (DescriptorT ClipT) }
  @c@o@l@o@r "-" @a@d@j@u@s@t @psc                                         { constoken (DescriptorT ColorAdjustT) }
  @c@o@l@o@r "-" @i@n@t@e@r@p@o@l@a@t@i@o@n "-" @f@i@l@t@e@r@s @psc        { constoken (DescriptorT
                                                                                          ColorInterpolationFiltersT) }
  @c@o@l@o@r "-" @i@n@t@e@r@p@o@l@a@t@i@o@n @psc                           { constoken (DescriptorT ColorInterpolationT) }
  @c@o@l@o@r "-" @s@c@h@e@m@e @psc                                         { constoken (DescriptorT ColorSchemeT) }
  @c@o@l@o@r @psc                                                          { constoken (DescriptorT ColorT) }
  @c@o@l@u@m@n "-" @c@o@u@n@t @psc                                         { constoken (DescriptorT ColumnCountT) }
  @c@o@l@u@m@n "-" @f@i@l@l @psc                                           { constoken (DescriptorT ColumnFillT) }
  @c@o@l@u@m@n "-" @g@a@p @psc                                             { constoken (DescriptorT ColumnGapT) }
  @c@o@l@u@m@n "-" @h@e@i@g@h@t @psc                                       { constoken (DescriptorT ColumnHeightT) }
  @c@o@l@u@m@n "-" @r@u@l@e "-" @c@o@l@o@r @psc                            { constoken (DescriptorT ColumnRuleColorT) }
  @c@o@l@u@m@n "-" @r@u@l@e "-" @s@t@y@l@e @psc                            { constoken (DescriptorT ColumnRuleStyleT) }
  @c@o@l@u@m@n "-" @r@u@l@e "-" @w@i@d@t@h @psc                            { constoken (DescriptorT ColumnRuleWidthT) }
  @c@o@l@u@m@n "-" @r@u@l@e @psc                                           { constoken (DescriptorT ColumnRuleT) }
  @c@o@l@u@m@n "-" @s@p@a@n @psc                                           { constoken (DescriptorT ColumnSpanT) }
  @c@o@l@u@m@n "-" @w@i@d@t@h @psc                                         { constoken (DescriptorT ColumnWidthT) }
  @c@o@l@u@m@n "-" @w@r@a@p @psc                                           { constoken (DescriptorT ColumnWrapT) }
  @c@o@l@u@m@n@s @psc                                                      { constoken (DescriptorT ColumnsT) }
  @c@o@n@t@a@i@n "-" @i@n@t@r@i@n@s@i@c "-" @b@l@o@c@k "-" @s@i@z@e @psc   { constoken (DescriptorT ContainIntrinsicBlockSizeT) }
  @c@o@n@t@a@i@n "-" @i@n@t@r@i@n@s@i@c "-" @h@e@i@g@h@t @psc              { constoken (DescriptorT ContainIntrinsicHeightT) }
  @c@o@n@t@a@i@n "-" @i@n@t@r@i@n@s@i@c "-" @i@n@l@i@n@e "-" @s@i@z@e @psc { constoken (DescriptorT ContainIntrinsicInlineSizeT) }
  @c@o@n@t@a@i@n "-" @i@n@t@r@i@n@s@i@c "-" @s@i@z@e @psc                  { constoken (DescriptorT ContainIntrinsicSizeT) }
  @c@o@n@t@a@i@n "-" @i@n@t@r@i@n@s@i@c "-" @w@i@d@t@h @psc                { constoken (DescriptorT ContainIntrinsicWidthT) }
  @c@o@n@t@a@i@n @psc                                                      { constoken (DescriptorT ContainT) }
  @c@o@n@t@a@i@n@e@r "-" @n@a@m@e @psc                                     { constoken (DescriptorT ContainerNameT) }
  @c@o@n@t@a@i@n@e@r "-" @t@y@p@e @psc                                     { constoken (DescriptorT ContainerTypeT) }
  @c@o@n@t@a@i@n@e@r @psc                                                  { constoken (DescriptorT ContainerD) }
  @c@o@n@t@e@n@t "-" @v@i@s@i@b@i@l@i@t@y @psc                             { constoken (DescriptorT ContentVisibilityT) }
  @c@o@n@t@e@n@t @psc                                                      { constoken (DescriptorT ContentT) }
  @c@o@r@n@e@r "-" @b@l@o@c@k "-" @e@n@d "-" @s@h@a@p@e @psc               { constoken (DescriptorT CornerBlockEndShapeT) }
  @c@o@r@n@e@r "-" @b@l@o@c@k "-" @s@t@a@r@t "-" @s@h@a@p@e @psc           { constoken (DescriptorT CornerBlockStartShapeT) }
  @c@o@r@n@e@r "-" @b@o@t@t@o@m "-" @l@e@f@t "-" @s@h@a@p@e @psc           { constoken (DescriptorT CornerBottomLeftShapeT) }
  @c@o@r@n@e@r "-" @b@o@t@t@o@m "-" @r@i@g@h@t "-" @s@h@a@p@e @psc         { constoken (DescriptorT CornerBottomRightShapeT) }
  @c@o@r@n@e@r "-" @b@o@t@t@o@m "-" @s@h@a@p@e @psc                        { constoken (DescriptorT CornerBottomShapeT) }
  @c@o@r@n@e@r "-" @e@n@d "-" @e@n@d "-" @s@h@a@p@e @psc                   { constoken (DescriptorT CornerEndEndShapeT) }
  @c@o@r@n@e@r "-" @e@n@d "-" @s@t@a@r@t "-" @s@h@a@p@e @psc               { constoken (DescriptorT CornerEndStartShapeT) }
  @c@o@r@n@e@r "-" @i@n@l@i@n@e "-" @e@n@d "-" @s@h@a@p@e @psc             { constoken (DescriptorT CornerInlineEndShapeT) }
  @c@o@r@n@e@r "-" @i@n@l@i@n@e "-" @s@t@a@r@t "-" @s@h@a@p@e @psc         { constoken (DescriptorT CornerInlineStartShapeT) }
  @c@o@r@n@e@r "-" @l@e@f@t "-" @s@h@a@p@e @psc                            { constoken (DescriptorT CornerLeftShapeT) }
  @c@o@r@n@e@r "-" @r@i@g@h@t "-" @s@h@a@p@e @psc                          { constoken (DescriptorT CornerRightShapeT) }
  @c@o@r@n@e@r "-" @s@h@a@p@e @psc                                         { constoken (DescriptorT CornerShapeT) }
  @c@o@r@n@e@r "-" @s@t@a@r@t "-" @e@n@d "-" @s@h@a@p@e @psc               { constoken (DescriptorT CornerStartEndShapeT) }
  @c@o@r@n@e@r "-" @s@t@a@r@t "-" @s@t@a@r@t "-" @s@h@a@p@e @psc           { constoken (DescriptorT CornerStartStartShapeT) }
  @c@o@r@n@e@r "-" @t@o@p "-" @l@e@f@t "-" @s@h@a@p@e @psc                 { constoken (DescriptorT CornerTopLeftShapeT) }
  @c@o@r@n@e@r "-" @t@o@p "-" @r@i@g@h@t "-" @s@h@a@p@e @psc               { constoken (DescriptorT CornerTopRightShapeT) }
  @c@o@r@n@e@r "-" @t@o@p "-" @s@h@a@p@e @psc                              { constoken (DescriptorT CornerTopShapeT) }
  @c@o@u@n@t@e@r "-" @i@n@c@r@e@m@e@n@t @psc                               { constoken (DescriptorT CounterIncrementT) }
  @c@o@u@n@t@e@r "-" @r@e@s@e@t @psc                                       { constoken (DescriptorT CounterResetT) }
  @c@o@u@n@t@e@r "-" @s@e@t @psc                                           { constoken (DescriptorT CounterSetT) }
  @c@u@r@s@o@r @psc                                                        { constoken (DescriptorT CursorT) }
  @c@x @psc                                                                { constoken (DescriptorT CxT) }
  @c@y @psc                                                                { constoken (DescriptorT CyT) }
  @d @psc                                                                  { constoken (DescriptorT DT) }
  @d@i@r@e@c@t@i@o@n @psc                                                  { constoken (DescriptorT DirectionT) }
  @d@i@s@p@l@a@y @psc                                                      { constoken (DescriptorT DisplayT) }
  @d@o@m@i@n@a@n@t "-" @b@a@s@e@l@i@n@e @psc                               { constoken (DescriptorT DominantBaselineT) }
  @d@y@n@a@m@i@c "-" @r@a@n@g@e "-" @l@i@m@i@t @psc                        { constoken (DescriptorT DynamicRangeLimitT) }
  @e@m@p@t@y "-" @c@e@l@l@s @psc                                           { constoken (DescriptorT EmptyCellsT) }
  @f@i@e@l@d "-" @s@i@z@i@n@g @psc                                         { constoken (DescriptorT FieldSizingT) }
  @f@i@l@l "-" @o@p@a@c@i@t@y @psc                                         { constoken (DescriptorT FillOpacityT) }
  @f@i@l@l "-" @r@u@l@e @psc                                               { constoken (DescriptorT FillRuleT) }
  @f@i@l@l @psc                                                            { constoken (DescriptorT FillT) }
  @f@i@l@t@e@r @psc                                                        { constoken (DescriptorT FilterT) }
  @f@l@e@x "-" @b@a@s@i@s @psc                                             { constoken (DescriptorT FlexBasisT) }
  @f@l@e@x "-" @d@i@r@e@c@t@i@o@n @psc                                     { constoken (DescriptorT FlexDirectionT) }
  @f@l@e@x "-" @f@l@o@w @psc                                               { constoken (DescriptorT FlexFlowT) }
  @f@l@e@x "-" @g@r@o@w @psc                                               { constoken (DescriptorT FlexGrowT) }
  @f@l@e@x "-" @s@h@r@i@n@k @psc                                           { constoken (DescriptorT FlexShrinkT) }
  @f@l@e@x "-" @w@r@a@p @psc                                               { constoken (DescriptorT FlexWrapT) }
  @f@l@e@x @psc                                                            { constoken (DescriptorT FlexT) }
  @f@l@o@a@t @psc                                                          { constoken (DescriptorT FloatT) }
  @f@l@o@o@d "-" @c@o@l@o@r @psc                                           { constoken (DescriptorT FloodColorT) }
  @f@l@o@o@d "-" @o@p@a@c@i@t@y @psc                                       { constoken (DescriptorT FloodOpacityT) }
  @f@o@n@t "-" @d@i@s@p@l@a@y @psc                                         { constoken (DescriptorT FontDisplayT) }
  @f@o@n@t "-" @f@a@m@i@l@y @psc                                           { constoken (DescriptorT FontFamilyT) }
  @f@o@n@t "-" @f@e@a@t@u@r@e "-" @s@e@t@t@i@n@g@s @psc                    { constoken (DescriptorT FontFeatureSettingsT) }
  @f@o@n@t "-" @k@e@r@n@i@n@g @psc                                         { constoken (DescriptorT FontKerningT) }
  @f@o@n@t "-" @l@a@n@g@u@a@g@e "-" @o@v@e@r@r@i@d@e @psc                  { constoken (DescriptorT FontLanguageOverrideT) }
  @f@o@n@t "-" @o@p@t@i@c@a@l "-" @s@i@z@i@n@g @psc                        { constoken (DescriptorT FontOpticalSizingT) }
  @f@o@n@t "-" @p@a@l@e@t@t@e @psc                                         { constoken (DescriptorT FontPaletteT) }
  @f@o@n@t "-" @s@i@z@e "-" @a@d@j@u@s@t @psc                              { constoken (DescriptorT FontSizeAdjustT) }
  @f@o@n@t "-" @s@i@z@e @psc                                               { constoken (DescriptorT FontSizeT) }
  @f@o@n@t "-" @s@m@o@o@t@h @psc                                           { constoken (DescriptorT FontSmoothT) }
  @f@o@n@t "-" @s@t@r@e@t@c@h @psc                                         { constoken (DescriptorT FontStretchT) }
  @f@o@n@t "-" @s@t@y@l@e @psc                                             { constoken (DescriptorT FontStyleT) }
  @f@o@n@t "-" @s@y@n@t@h@e@s@i@s "-" @p@o@s@i@t@i@o@n @psc                { constoken (DescriptorT FontSynthesisPositionT) }
  @f@o@n@t "-" @s@y@n@t@h@e@s@i@s "-" @s@m@a@l@l "-" @c@a@p@s @psc         { constoken (DescriptorT FontSynthesisSmallCapsT) }
  @f@o@n@t "-" @s@y@n@t@h@e@s@i@s "-" @s@t@y@l@e @psc                      { constoken (DescriptorT FontSynthesisStyleT) }
  @f@o@n@t "-" @s@y@n@t@h@e@s@i@s "-" @w@e@i@g@h@t @psc                    { constoken (DescriptorT FontSynthesisWeightT) }
  @f@o@n@t "-" @s@y@n@t@h@e@s@i@s @psc                                     { constoken (DescriptorT FontSynthesisT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @a@l@t@e@r@n@a@t@e@s @psc                { constoken (DescriptorT FontVariantAlternatesT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @c@a@p@s @psc                            { constoken (DescriptorT FontVariantCapsT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @e@a@s@t "-" @a@s@i@a@n @psc             { constoken (DescriptorT FontVariantEastAsianT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @e@m@o@j@i @psc                          { constoken (DescriptorT FontVariantEmojiT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @l@i@g@a@t@u@r@e@s @psc                  { constoken (DescriptorT FontVariantLigaturesT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @n@u@m@e@r@i@c @psc                      { constoken (DescriptorT FontVariantNumericT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t "-" @p@o@s@i@t@i@o@n @psc                    { constoken (DescriptorT FontVariantPositionT) }
  @f@o@n@t "-" @v@a@r@i@a@n@t @psc                                         { constoken (DescriptorT FontVariantT) }
  @f@o@n@t "-" @v@a@r@i@a@t@i@o@n "-" @s@e@t@t@i@n@g@s @psc                { constoken (DescriptorT FontVariationSettingsT) }
  @f@o@n@t "-" @w@e@i@g@h@t @psc                                           { constoken (DescriptorT FontWeightT) }
  @f@o@n@t "-" @w@i@d@t@h @psc                                             { constoken (DescriptorT FontWidthT) }
  @f@o@n@t @psc                                                            { constoken (DescriptorT FontT) }
  @f@o@r@c@e@d "-" @c@o@l@o@r "-" @a@d@j@u@s@t @psc                        { constoken (DescriptorT ForcedColorAdjustT) }
  @g@a@p @psc                                                              { constoken (DescriptorT GapT) }
  @g@r@i@d "-" @a@r@e@a @psc                                               { constoken (DescriptorT GridAreaT) }
  @g@r@i@d "-" @a@u@t@o "-" @c@o@l@u@m@n@s @psc                            { constoken (DescriptorT GridAutoColumnsT) }
  @g@r@i@d "-" @a@u@t@o "-" @f@l@o@w @psc                                  { constoken (DescriptorT GridAutoFlowT) }
  @g@r@i@d "-" @a@u@t@o "-" @r@o@w@s @psc                                  { constoken (DescriptorT GridAutoRowsT) }
  @g@r@i@d "-" @c@o@l@u@m@n "-" @e@n@d @psc                                { constoken (DescriptorT GridColumnEndT) }
  @g@r@i@d "-" @c@o@l@u@m@n "-" @s@t@a@r@t @psc                            { constoken (DescriptorT GridColumnStartT) }
  @g@r@i@d "-" @c@o@l@u@m@n @psc                                           { constoken (DescriptorT GridColumnT) }
  @g@r@i@d "-" @g@a@p @psc                                                 { constoken (DescriptorT GridGapT) }
  @g@r@i@d "-" @r@o@w "-" @e@n@d @psc                                      { constoken (DescriptorT GridRowEndT) }
  @g@r@i@d "-" @r@o@w "-" @s@t@a@r@t @psc                                  { constoken (DescriptorT GridRowStartT) }
  @g@r@i@d "-" @r@o@w @psc                                                 { constoken (DescriptorT GridRowT) }
  @g@r@i@d "-" @t@e@m@p@l@a@t@e "-" @a@r@e@a@s @psc                        { constoken (DescriptorT GridTemplateAreasT) }
  @g@r@i@d "-" @t@e@m@p@l@a@t@e "-" @c@o@l@u@m@n@s @psc                    { constoken (DescriptorT GridTemplateColumnsT) }
  @g@r@i@d "-" @t@e@m@p@l@a@t@e "-" @r@o@w@s @psc                          { constoken (DescriptorT GridTemplateRowsT) }
  @g@r@i@d "-" @t@e@m@p@l@a@t@e @psc                                       { constoken (DescriptorT GridTemplateT) }
  @g@r@i@d @psc                                                            { constoken (DescriptorT GridT) }
  @h@a@n@g@i@n@g "-" @p@u@n@c@t@u@a@t@i@o@n @psc                           { constoken (DescriptorT HangingPunctuationT) }
  @h@e@i@g@h@t @psc                                                        { constoken (DescriptorT HeightT) }
  @h@y@p@h@e@n@a@t@e "-" @c@h@a@r@a@c@t@e@r @psc                           { constoken (DescriptorT HyphenateCharacterT) }
  @h@y@p@h@e@n@a@t@e "-" @l@i@m@i@t "-" @c@h@a@r@s @psc                    { constoken (DescriptorT HyphenateLimitCharsT) }
  @h@y@p@h@e@n@s @psc                                                      { constoken (DescriptorT HyphensT) }
  @i@m@a@g@e "-" @o@r@i@e@n@t@a@t@i@o@n @psc                               { constoken (DescriptorT ImageOrientationT) }
  @i@m@a@g@e "-" @r@e@n@d@e@r@i@n@g @psc                                   { constoken (DescriptorT ImageRenderingT) }
  @i@m@a@g@e "-" @r@e@s@o@l@u@t@i@o@n @psc                                 { constoken (DescriptorT ImageResolutionT) }
  @i@n@h@e@r@i@t@s @psc                                                    { constoken (DescriptorT InheritsT) }
  @i@n@i@t@i@a@l "-" @l@e@t@t@e@r @psc                                     { constoken (DescriptorT InitialLetterT) }
  @i@n@l@i@n@e "-" @s@i@z@e @psc                                           { constoken (DescriptorT InlineSizeT) }
  @i@n@i@t@i@a@l "-" @v@a@l@u@e @psc                                       { constoken (DescriptorT InitialValueT) }
  @i@n@s@e@t "-" @b@l@o@c@k "-" @e@n@d @psc                                { constoken (DescriptorT InsetBlockEndT) }
  @i@n@s@e@t "-" @b@l@o@c@k "-" @s@t@a@r@t @psc                            { constoken (DescriptorT InsetBlockStartT) }
  @i@n@s@e@t "-" @b@l@o@c@k @psc                                           { constoken (DescriptorT InsetBlockT) }
  @i@n@s@e@t "-" @i@n@l@i@n@e "-" @e@n@d @psc                              { constoken (DescriptorT InsetInlineEndT) }
  @i@n@s@e@t "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc                          { constoken (DescriptorT InsetInlineStartT) }
  @i@n@s@e@t "-" @i@n@l@i@n@e @psc                                         { constoken (DescriptorT InsetInlineT) }
  @i@n@s@e@t @psc                                                          { constoken (DescriptorT InsetT) }
  @i@n@t@e@r@a@c@t@i@v@i@t@y @psc                                          { constoken (DescriptorT InteractivityT) }
  @i@n@t@e@r@e@s@t "-" @d@e@l@a@y @psc                                     { constoken (DescriptorT InterestDelayT) }
  @i@n@t@e@r@e@s@t "-" @d@e@l@a@y "-" @e@n@d @psc                          { constoken (DescriptorT InterestDelayEndT) }
  @i@n@t@e@r@e@s@t "-" @d@e@l@a@y "-" @s@t@a@r@t @psc                      { constoken (DescriptorT InterestDelayStartT) }
  @i@n@t@e@r@p@o@l@a@t@e "-" @s@i@z@e @psc                                 { constoken (DescriptorT InterpolateSizeT) }
  @i@s@o@l@a@t@i@o@n @psc                                                  { constoken (DescriptorT IsolationT) }
  @j@u@s@t@i@f@y "-" @c@o@n@t@e@n@t @psc                                   { constoken (DescriptorT JustifyContentT) }
  @j@u@s@t@i@f@y "-" @i@t@e@m@s @psc                                       { constoken (DescriptorT JustifyItemsT) }
  @j@u@s@t@i@f@y "-" @s@e@l@f @psc                                         { constoken (DescriptorT JustifySelfT) }
  @l@e@f@t @psc                                                            { constoken (DescriptorT LeftT) }
  @l@e@t@t@e@r "-" @s@p@a@c@i@n@g @psc                                     { constoken (DescriptorT LetterSpacingT) }
  @l@i@g@h@t@i@n@g "-" @c@o@l@o@r @psc                                     { constoken (DescriptorT LightingColorT) }
  @l@i@n@e "-" @b@r@e@a@k @psc                                             { constoken (DescriptorT LineBreakT) }
  @l@i@n@e "-" @c@l@a@m@p @psc                                             { constoken (DescriptorT LineClampT) }
  @l@i@n@e "-" @h@e@i@g@h@t "-" @s@t@e@p @psc                              { constoken (DescriptorT LineHeightStepT) }
  @l@i@n@e "-" @h@e@i@g@h@t @psc                                           { constoken (DescriptorT LineHeightT) }
  @l@i@s@t "-" @s@t@y@l@e "-" @i@m@a@g@e @psc                              { constoken (DescriptorT ListStyleImageT) }
  @l@i@s@t "-" @s@t@y@l@e "-" @p@o@s@i@t@i@o@n @psc                        { constoken (DescriptorT ListStylePositionT) }
  @l@i@s@t "-" @s@t@y@l@e "-" @t@y@p@e @psc                                { constoken (DescriptorT ListStyleTypeT) }
  @l@i@s@t "-" @s@t@y@l@e @psc                                             { constoken (DescriptorT ListStyleT) }
  @m@a@r@g@i@n "-" @b@l@o@c@k "-" @e@n@d @psc                              { constoken (DescriptorT MarginBlockEndT) }
  @m@a@r@g@i@n "-" @b@l@o@c@k "-" @s@t@a@r@t @psc                          { constoken (DescriptorT MarginBlockStartT) }
  @m@a@r@g@i@n "-" @b@l@o@c@k @psc                                         { constoken (DescriptorT MarginBlockT) }
  @m@a@r@g@i@n "-" @b@o@t@t@o@m @psc                                       { constoken (DescriptorT MarginBottomT) }
  @m@a@r@g@i@n "-" @i@n@l@i@n@e "-" @e@n@d @psc                            { constoken (DescriptorT MarginInlineEndT) }
  @m@a@r@g@i@n "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc                        { constoken (DescriptorT MarginInlineStartT) }
  @m@a@r@g@i@n "-" @i@n@l@i@n@e @psc                                       { constoken (DescriptorT MarginInlineT) }
  @m@a@r@g@i@n "-" @l@e@f@t @psc                                           { constoken (DescriptorT MarginLeftT) }
  @m@a@r@g@i@n "-" @r@i@g@h@t @psc                                         { constoken (DescriptorT MarginRightT) }
  @m@a@r@g@i@n "-" @t@o@p @psc                                             { constoken (DescriptorT MarginTopT) }
  @m@a@r@g@i@n "-" @t@r@i@m @psc                                           { constoken (DescriptorT MarginTrimT) }
  @m@a@r@g@i@n @psc                                                        { constoken (DescriptorT MarginT) }
  @m@a@r@k@e@r "-" @e@n@d @psc                                             { constoken (DescriptorT MarkerEndT) }
  @m@a@r@k@e@r "-" @m@i@d @psc                                             { constoken (DescriptorT MarkerMidT) }
  @m@a@r@k@e@r "-" @s@t@a@r@t @psc                                         { constoken (DescriptorT MarkerStartT) }
  @m@a@r@k@e@r @psc                                                        { constoken (DescriptorT MarkerT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @m@o@d@e @psc                              { constoken (DescriptorT MaskBorderModeT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @o@u@t@s@e@t @psc                          { constoken (DescriptorT MaskBorderOutsetT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @r@e@p@e@a@t @psc                          { constoken (DescriptorT MaskBorderRepeatT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @s@l@i@c@e @psc                            { constoken (DescriptorT MaskBorderSliceT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @s@o@u@r@c@e @psc                          { constoken (DescriptorT MaskBorderSourceT) }
  @m@a@s@k "-" @b@o@r@d@e@r "-" @w@i@d@t@h @psc                            { constoken (DescriptorT MaskBorderWidthT) }
  @m@a@s@k "-" @b@o@r@d@e@r @psc                                           { constoken (DescriptorT MaskBorderT) }
  @m@a@s@k "-" @c@l@i@p @psc                                               { constoken (DescriptorT MaskClipT) }
  @m@a@s@k "-" @c@o@m@p@o@s@i@t@e @psc                                     { constoken (DescriptorT MaskCompositeT) }
  @m@a@s@k "-" @i@m@a@g@e @psc                                             { constoken (DescriptorT MaskImageT) }
  @m@a@s@k "-" @m@o@d@e @psc                                               { constoken (DescriptorT MaskModeT) }
  @m@a@s@k "-" @o@r@i@g@i@n @psc                                           { constoken (DescriptorT MaskOriginT) }
  @m@a@s@k "-" @p@o@s@i@t@i@o@n @psc                                       { constoken (DescriptorT MaskPositionT) }
  @m@a@s@k "-" @r@e@p@e@a@t @psc                                           { constoken (DescriptorT MaskRepeatT) }
  @m@a@s@k "-" @s@i@z@e @psc                                               { constoken (DescriptorT MaskSizeT) }
  @m@a@s@k "-" @t@y@p@e @psc                                               { constoken (DescriptorT MaskTypeT) }
  @m@a@s@k @psc                                                            { constoken (DescriptorT MaskT) }
  @m@a@t@h "-" @d@e@p@t@h @psc                                             { constoken (DescriptorT MathDepthT) }
  @m@a@t@h "-" @s@h@i@f@t @psc                                             { constoken (DescriptorT MathShiftT) }
  @m@a@t@h "-" @s@t@y@l@e @psc                                             { constoken (DescriptorT MathStyleT) }
  @m@a@x "-" @b@l@o@c@k "-" @s@i@z@e @psc                                  { constoken (DescriptorT MaxBlockSizeT) }
  @m@a@x "-" @h@e@i@g@h@t @psc                                             { constoken (DescriptorT MaxHeightT) }
  @m@a@x "-" @i@n@l@i@n@e "-" @s@i@z@e @psc                                { constoken (DescriptorT MaxInlineSizeT) }
  @m@a@x "-" @w@i@d@t@h @psc                                               { constoken (DescriptorT MaxWidthT) }
  @m@i@n "-" @b@l@o@c@k "-" @s@i@z@e @psc                                  { constoken (DescriptorT MinBlockSizeT) }
  @m@i@n "-" @h@e@i@g@h@t @psc                                             { constoken (DescriptorT MinHeightT) }
  @m@i@n "-" @i@n@l@i@n@e "-" @s@i@z@e @psc                                { constoken (DescriptorT MinInlineSizeT) }
  @m@i@n "-" @w@i@d@t@h @psc                                               { constoken (DescriptorT MinWidthT) }
  @m@i@x "-" @b@l@e@n@d "-" @m@o@d@e @psc                                  { constoken (DescriptorT MixBlendModeT) }
  @o@b@j@e@c@t "-" @f@i@t @psc                                             { constoken (DescriptorT ObjectFitT) }
  @o@b@j@e@c@t "-" @p@o@s@i@t@i@o@n @psc                                   { constoken (DescriptorT ObjectPositionT) }
  @o@b@j@e@c@t "-" @v@i@e@w "-" @b@o@x @psc                                { constoken (DescriptorT ObjectViewBoxT) }
  @o@f@f@s@e@t "-" @a@n@c@h@o@r @psc                                       { constoken (DescriptorT OffsetAnchorT) }
  @o@f@f@s@e@t "-" @d@i@s@t@a@n@c@e @psc                                   { constoken (DescriptorT OffsetDistanceT) }
  @o@f@f@s@e@t "-" @p@a@t@h @psc                                           { constoken (DescriptorT OffsetPathT) }
  @o@f@f@s@e@t "-" @p@o@s@i@t@i@o@n @psc                                   { constoken (DescriptorT OffsetPositionT) }
  @o@f@f@s@e@t "-" @r@o@t@a@t@e @psc                                       { constoken (DescriptorT OffsetRotateT) }
  @o@f@f@s@e@t @psc                                                        { constoken (DescriptorT OffsetT) }
  @o@p@a@c@i@t@y @psc                                                      { constoken (DescriptorT OpacityT) }
  @o@r@d@e@r @psc                                                          { constoken (DescriptorT OrderT) }
  @o@r@p@h@a@n@s @psc                                                      { constoken (DescriptorT OrphansT) }
  @o@u@t@l@i@n@e "-" @c@o@l@o@r @psc                                       { constoken (DescriptorT OutlineColorT) }
  @o@u@t@l@i@n@e "-" @o@f@f@s@e@t @psc                                     { constoken (DescriptorT OutlineOffsetT) }
  @o@u@t@l@i@n@e "-" @s@t@y@l@e @psc                                       { constoken (DescriptorT OutlineStyleT) }
  @o@u@t@l@i@n@e "-" @w@i@d@t@h @psc                                       { constoken (DescriptorT OutlineWidthT) }
  @o@u@t@l@i@n@e @psc                                                      { constoken (DescriptorT OutlineT) }
  @o@v@e@r@f@l@o@w "-" @a@n@c@h@o@r @psc                                   { constoken (DescriptorT OverflowAnchorT) }
  @o@v@e@r@f@l@o@w "-" @b@l@o@c@k @psc                                     { constoken (DescriptorT OverflowBlockT) }
  @o@v@e@r@f@l@o@w "-" @c@l@i@p "-" @m@a@r@g@i@n @psc                      { constoken (DescriptorT OverflowClipMarginT) }
  @o@v@e@r@f@l@o@w "-" @i@n@l@i@n@e @psc                                   { constoken (DescriptorT OverflowInlineT) }
  @o@v@e@r@f@l@o@w "-" @w@r@a@p @psc                                       { constoken (DescriptorT OverflowWrapT) }
  @o@v@e@r@f@l@o@w "-" @x @psc                                             { constoken (DescriptorT OverflowXT) }
  @o@v@e@r@f@l@o@w "-" @y @psc                                             { constoken (DescriptorT OverflowYT) }
  @o@v@e@r@f@l@o@w @psc                                                    { constoken (DescriptorT OverflowT) }
  @o@v@e@r@l@a@y @psc                                                      { constoken (DescriptorT OverlayT) }
  @o@v@e@r@r@i@d@e "-" @c@o@l@o@r@s @psc                                   { constoken (DescriptorT OverrideColorsT) }
  @o@v@e@r@s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r "-" @b@l@o@c@k @psc            { constoken (DescriptorT OverscrollBehaviorBlockT) }
  @o@v@e@r@s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r "-" @i@n@l@i@n@e @psc          { constoken (DescriptorT OverscrollBehaviorInlineT) }
  @o@v@e@r@s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r "-" @x @psc                    { constoken (DescriptorT OverscrollBehaviorXT) }
  @o@v@e@r@s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r "-" @y @psc                    { constoken (DescriptorT OverscrollBehaviorYT) }
  @o@v@e@r@s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r @psc                           { constoken (DescriptorT OverscrollBehaviorT) }
  @p@a@d@d@i@n@g "-" @b@l@o@c@k "-" @e@n@d @psc                            { constoken (DescriptorT PaddingBlockEndT) }
  @p@a@d@d@i@n@g "-" @b@l@o@c@k "-" @s@t@a@r@t @psc                        { constoken (DescriptorT PaddingBlockStartT) }
  @p@a@d@d@i@n@g "-" @b@l@o@c@k @psc                                       { constoken (DescriptorT PaddingBlockT) }
  @p@a@d@d@i@n@g "-" @b@o@t@t@o@m @psc                                     { constoken (DescriptorT PaddingBottomT) }
  @p@a@d@d@i@n@g "-" @i@n@l@i@n@e "-" @e@n@d @psc                          { constoken (DescriptorT PaddingInlineEndT) }
  @p@a@d@d@i@n@g "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc                      { constoken (DescriptorT PaddingInlineStartT) }
  @p@a@d@d@i@n@g "-" @i@n@l@i@n@e @psc                                     { constoken (DescriptorT PaddingInlineT) }
  @p@a@d@d@i@n@g "-" @l@e@f@t @psc                                         { constoken (DescriptorT PaddingLeftT) }
  @p@a@d@d@i@n@g "-" @r@i@g@h@t @psc                                       { constoken (DescriptorT PaddingRightT) }
  @p@a@d@d@i@n@g "-" @t@o@p @psc                                           { constoken (DescriptorT PaddingTopT) }
  @p@a@d@d@i@n@g @psc                                                      { constoken (DescriptorT PaddingT) }
  @p@a@g@e "-" @b@r@e@a@k "-" @a@f@t@e@r @psc                              { constoken (DescriptorT PageBreakAfterT) }
  @p@a@g@e "-" @b@r@e@a@k "-" @b@e@f@o@r@e @psc                            { constoken (DescriptorT PageBreakBeforeT) }
  @p@a@g@e "-" @b@r@e@a@k "-" @i@n@s@i@d@e @psc                            { constoken (DescriptorT PageBreakInsideT) }
  @p@a@g@e @psc                                                            { constoken (DescriptorT PageD) }
  @p@a@i@n@t "-" @o@r@d@e@r @psc                                           { constoken (DescriptorT PaintOrderT) }
  @p@e@r@s@p@e@c@t@i@v@e "-" @o@r@i@g@i@n @psc                             { constoken (DescriptorT PerspectiveOriginT) }
  @p@e@r@s@p@e@c@t@i@v@e @psc                                              { constoken (DescriptorT PerspectiveT) }
  @p@l@a@c@e "-" @c@o@n@t@e@n@t @psc                                       { constoken (DescriptorT PlaceContentT) }
  @p@l@a@c@e "-" @i@t@e@m@s @psc                                           { constoken (DescriptorT PlaceItemsT) }
  @p@l@a@c@e "-" @s@e@l@f @psc                                             { constoken (DescriptorT PlaceSelfT) }
  @p@o@i@n@t@e@r "-" @e@v@e@n@t@s @psc                                     { constoken (DescriptorT PointerEventsT) }
  @p@o@s@i@t@i@o@n "-" @a@n@c@h@o@r @psc                                   { constoken (DescriptorT PositionAnchorT) }
  @p@o@s@i@t@i@o@n "-" @a@r@e@a @psc                                       { constoken (DescriptorT PositionAreaT) }
  @p@o@s@i@t@i@o@n "-" @t@r@y "-" @f@a@l@l@b@a@c@k@s @psc                  { constoken (DescriptorT PositionTryFallbacksT) }
  @p@o@s@i@t@i@o@n "-" @t@r@y "-" @o@r@d@e@r @psc                          { constoken (DescriptorT PositionTryOrderT) }
  @p@o@s@i@t@i@o@n "-" @t@r@y @psc                                         { constoken (DescriptorT PositionTryD) }
  @p@o@s@i@t@i@o@n "-" @v@i@s@i@b@i@l@i@t@y @psc                           { constoken (DescriptorT PositionVisibilityT) }
  @p@o@s@i@t@i@o@n @psc                                                    { constoken (DescriptorT PositionT) }
  @p@r@i@n@t "-" @c@o@l@o@r "-" @a@d@j@u@s@t @psc                          { constoken (DescriptorT PrintColorAdjustT) }
  @q@u@o@t@e@s @psc                                                        { constoken (DescriptorT QuotesT) }
  @r @psc                                                                  { constoken (DescriptorT RT) }
  @r@e@a@d@i@n@g "-" @f@l@o@w @psc                                         { constoken (DescriptorT ReadingFlowT) }
  @r@e@a@d@i@n@g "-" @o@r@d@e@r @psc                                       { constoken (DescriptorT ReadingOrderT) }
  @r@e@s@i@z@e @psc                                                        { constoken (DescriptorT ResizeT) }
  @r@e@s@u@l@t @psc                                                        { constoken (DescriptorT ResultT) }
  @r@i@g@h@t @psc                                                          { constoken (DescriptorT RightT) }
  @r@o@t@a@t@e @psc                                                        { constoken (DescriptorT RotateT) }
  @r@o@w "-" @g@a@p @psc                                                   { constoken (DescriptorT RowGapT) }
  @r@u@b@y "-" @a@l@i@g@n @psc                                             { constoken (DescriptorT RubyAlignT) }
  @r@u@b@y "-" @o@v@e@r@h@a@n@g @psc                                       { constoken (DescriptorT RubyOverhangT) }
  @r@u@b@y "-" @p@o@s@i@t@i@o@n @psc                                       { constoken (DescriptorT RubyPositionT) }
  @r@x @psc                                                                { constoken (DescriptorT RxT) }
  @r@y @psc                                                                { constoken (DescriptorT RyT) }
  @s@c@a@l@e @psc                                                          { constoken (DescriptorT ScaleT) }
  @s@c@r@o@l@l "-" @b@e@h@a@v@i@o@r @psc                                   { constoken (DescriptorT ScrollBehaviorT) }
  @s@c@r@o@l@l "-" @i@n@i@t@i@a@l "-" @t@a@r@g@e@t @psc                    { constoken (DescriptorT ScrollInitialTargetT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @b@l@o@c@k "-" @e@n@d @psc             { constoken (DescriptorT ScrollMarginBlockEndT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @b@l@o@c@k "-" @s@t@a@r@t @psc         { constoken (DescriptorT ScrollMarginBlockStartT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @b@l@o@c@k @psc                        { constoken (DescriptorT ScrollMarginBlockT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @b@o@t@t@o@m @psc                      { constoken (DescriptorT ScrollMarginBottomT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @i@n@l@i@n@e "-" @e@n@d @psc           { constoken (DescriptorT ScrollMarginInlineEndT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc       { constoken (DescriptorT ScrollMarginInlineStartT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @i@n@l@i@n@e @psc                      { constoken (DescriptorT ScrollMarginInlineT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @l@e@f@t @psc                          { constoken (DescriptorT ScrollMarginLeftT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @r@i@g@h@t @psc                        { constoken (DescriptorT ScrollMarginRightT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n "-" @t@o@p @psc                            { constoken (DescriptorT ScrollMarginTopT) }
  @s@c@r@o@l@l "-" @m@a@r@g@i@n @psc                                       { constoken (DescriptorT ScrollMarginT) }
  @s@c@r@o@l@l "-" @m@a@r@k@e@r "-" @g@r@o@u@p @psc                        { constoken (DescriptorT ScrollMarkerGroupT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @b@l@o@c@k "-" @e@n@d @psc           { constoken (DescriptorT ScrollPaddingBlockEndT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @b@l@o@c@k "-" @s@t@a@r@t @psc       { constoken (DescriptorT ScrollPaddingBlockStartT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @b@l@o@c@k @psc                      { constoken (DescriptorT ScrollPaddingBlockT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @b@o@t@t@o@m @psc                    { constoken (DescriptorT ScrollPaddingBottomT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @i@n@l@i@n@e "-" @e@n@d @psc         { constoken (DescriptorT ScrollPaddingInlineEndT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @i@n@l@i@n@e "-" @s@t@a@r@t @psc     { constoken (DescriptorT ScrollPaddingInlineStartT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @i@n@l@i@n@e @psc                    { constoken (DescriptorT ScrollPaddingInlineT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @l@e@f@t @psc                        { constoken (DescriptorT ScrollPaddingLeftT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @r@i@g@h@t @psc                      { constoken (DescriptorT ScrollPaddingRightT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g "-" @t@o@p @psc                          { constoken (DescriptorT ScrollPaddingTopT) }
  @s@c@r@o@l@l "-" @p@a@d@d@i@n@g @psc                                     { constoken (DescriptorT ScrollPaddingT) }
  @s@c@r@o@l@l "-" @s@n@a@p "-" @a@l@i@g@n @psc                            { constoken (DescriptorT ScrollSnapAlignT) }
  @s@c@r@o@l@l "-" @s@n@a@p "-" @s@t@o@p @psc                              { constoken (DescriptorT ScrollSnapStopT) }
  @s@c@r@o@l@l "-" @s@n@a@p "-" @t@y@p@e @psc                              { constoken (DescriptorT ScrollSnapTypeT) }
  @s@c@r@o@l@l "-" @t@a@r@g@e@t "-" @g@r@o@u@p @psc                        { constoken (DescriptorT ScrollTargetGroupT) }
  @s@c@r@o@l@l "-" @t@i@m@e@l@i@n@e "-" @a@x@i@s @psc                      { constoken (DescriptorT ScrollTimelineAxisT) }
  @s@c@r@o@l@l "-" @t@i@m@e@l@i@n@e "-" @n@a@m@e @psc                      { constoken (DescriptorT ScrollTimelineNameT) }
  @s@c@r@o@l@l "-" @t@i@m@e@l@i@n@e @psc                                   { constoken (DescriptorT ScrollTimelineT) }
  @s@c@r@o@l@l@b@a@r "-" @c@o@l@o@r @psc                                   { constoken (DescriptorT ScrollbarColorT) }
  @s@c@r@o@l@l@b@a@r "-" @g@u@t@t@e@r @psc                                 { constoken (DescriptorT ScrollbarGutterT) }
  @s@c@r@o@l@l@b@a@r "-" @w@i@d@t@h @psc                                   { constoken (DescriptorT ScrollbarWidthT) }
  @s@h@a@p@e "-" @i@m@a@g@e "-" @t@h@r@e@s@h@o@l@d @psc                    { constoken (DescriptorT ShapeImageThresholdT) }
  @s@h@a@p@e "-" @m@a@r@g@i@n @psc                                         { constoken (DescriptorT ShapeMarginT) }
  @s@h@a@p@e "-" @o@u@t@s@i@d@e @psc                                       { constoken (DescriptorT ShapeOutsideT) }
  @s@h@a@p@e "-" @r@e@n@d@e@r@i@n@g @psc                                   { constoken (DescriptorT ShapeRenderingT) }
  @s@p@e@a@k "-" @a@s @psc                                                 { constoken (DescriptorT SpeakAsT) }
  @s@r@c @psc                                                              { constoken (DescriptorT SrcT) }
  @s@t@o@p "-" @c@o@l@o@r @psc                                             { constoken (DescriptorT StopColorT) }
  @s@t@o@p "-" @o@p@a@c@i@t@y @psc                                         { constoken (DescriptorT StopOpacityT) }
  @s@t@r@o@k@e "-" @d@a@s@h@a@r@r@a@y @psc                                 { constoken (DescriptorT StrokeDasharrayT) }
  @s@t@r@o@k@e "-" @d@a@s@h@o@f@f@s@e@t @psc                               { constoken (DescriptorT StrokeDashoffsetT) }
  @s@t@r@o@k@e "-" @l@i@n@e@c@a@p @psc                                     { constoken (DescriptorT StrokeLinecapT) }
  @s@t@r@o@k@e "-" @l@i@n@e@j@o@i@n @psc                                   { constoken (DescriptorT StrokeLinejoinT) }
  @s@t@r@o@k@e "-" @m@i@t@e@r@l@i@m@i@t @psc                               { constoken (DescriptorT StrokeMiterlimitT) }
  @s@t@r@o@k@e "-" @o@p@a@c@i@t@y @psc                                     { constoken (DescriptorT StrokeOpacityT) }
  @s@t@r@o@k@e "-" @w@i@d@t@h @psc                                         { constoken (DescriptorT StrokeWidthT) }
  @s@t@r@o@k@e @psc                                                        { constoken (DescriptorT StrokeT) }
  @s@y@n@t@a@x @psc                                                        { constoken (DescriptorT SyntaxT) }
  @t@a@b "-" @s@i@z@e @psc                                                 { constoken (DescriptorT TabSizeT) }
  @t@a@b@l@e "-" @l@a@y@o@u@t @psc                                         { constoken (DescriptorT TableLayoutT) }
  @t@e@x@t "-" @a@l@i@g@n "-" @l@a@s@t @psc                                { constoken (DescriptorT TextAlignLastT) }
  @t@e@x@t "-" @a@l@i@g@n @psc                                             { constoken (DescriptorT TextAlignT) }
  @t@e@x@t "-" @a@n@c@h@o@r @psc                                           { constoken (DescriptorT TextAnchorT) }
  @t@e@x@t "-" @a@u@t@o@s@p@a@c@e @psc                                     { constoken (DescriptorT TextAutospaceT) }
  @t@e@x@t "-" @b@o@x "-" @e@d@g@e @psc                                    { constoken (DescriptorT TextBoxEdgeT) }
  @t@e@x@t "-" @b@o@x "-" @t@r@i@m @psc                                    { constoken (DescriptorT TextBoxTrimT) }
  @t@e@x@t "-" @b@o@x @psc                                                 { constoken (DescriptorT TextBoxT) }
  @t@e@x@t "-" @c@o@m@b@i@n@e "-" @u@p@r@i@g@h@t @psc                      { constoken (DescriptorT TextCombineUprightT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @c@o@l@o@r @psc                    { constoken (DescriptorT TextDecorationColorT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @i@n@s@e@t @psc                    { constoken (DescriptorT TextDecorationInsetT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @l@i@n@e @psc                      { constoken (DescriptorT TextDecorationLineT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @s@k@i@p "-" @i@n@k @psc           { constoken (DescriptorT TextDecorationSkipInkT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @s@k@i@p @psc                      { constoken (DescriptorT TextDecorationSkipT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @s@t@y@l@e @psc                    { constoken (DescriptorT TextDecorationStyleT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n "-" @t@h@i@c@k@n@e@s@s @psc            { constoken (DescriptorT TextDecorationThicknessT) }
  @t@e@x@t "-" @d@e@c@o@r@a@t@i@o@n @psc                                   { constoken (DescriptorT TextDecorationT) }
  @t@e@x@t "-" @e@m@p@h@a@s@i@s "-" @c@o@l@o@r @psc                        { constoken (DescriptorT TextEmphasisColorT) }
  @t@e@x@t "-" @e@m@p@h@a@s@i@s "-" @p@o@s@i@t@i@o@n @psc                  { constoken (DescriptorT TextEmphasisPositionT) }
  @t@e@x@t "-" @e@m@p@h@a@s@i@s "-" @s@t@y@l@e @psc                        { constoken (DescriptorT TextEmphasisStyleT) }
  @t@e@x@t "-" @e@m@p@h@a@s@i@s @psc                                       { constoken (DescriptorT TextEmphasisT) }
  @t@e@x@t "-" @i@n@d@e@n@t @psc                                           { constoken (DescriptorT TextIndentT) }
  @t@e@x@t "-" @j@u@s@t@i@f@y @psc                                         { constoken (DescriptorT TextJustifyT) }
  @t@e@x@t "-" @o@r@i@e@n@t@a@t@i@o@n @psc                                 { constoken (DescriptorT TextOrientationT) }
  @t@e@x@t "-" @o@v@e@r@f@l@o@w @psc                                       { constoken (DescriptorT TextOverflowT) }
  @t@e@x@t "-" @r@e@n@d@e@r@i@n@g @psc                                     { constoken (DescriptorT TextRenderingT) }
  @t@e@x@t "-" @s@h@a@d@o@w @psc                                           { constoken (DescriptorT TextShadowT) }
  @t@e@x@t "-" @s@i@z@e "-" @a@d@j@u@s@t @psc                              { constoken (DescriptorT TextSizeAdjustT) }
  @t@e@x@t "-" @s@p@a@c@i@n@g "-" @t@r@i@m @psc                            { constoken (DescriptorT TextSpacingTrimT) }
  @t@e@x@t "-" @t@r@a@n@s@f@o@r@m @psc                                     { constoken (DescriptorT TextTransformT) }
  @t@e@x@t "-" @u@n@d@e@r@l@i@n@e "-" @o@f@f@s@e@t @psc                    { constoken (DescriptorT TextUnderlineOffsetT) }
  @t@e@x@t "-" @u@n@d@e@r@l@i@n@e "-" @p@o@s@i@t@i@o@n @psc                { constoken (DescriptorT TextUnderlinePositionT) }
  @t@e@x@t "-" @w@r@a@p "-" @m@o@d@e @psc                                  { constoken (DescriptorT TextWrapModeT) }
  @t@e@x@t "-" @w@r@a@p "-" @s@t@y@l@e @psc                                { constoken (DescriptorT TextWrapStyleT) }
  @t@e@x@t "-" @w@r@a@p @psc                                               { constoken (DescriptorT TextWrapT) }
  @t@i@m@e@l@i@n@e "-" @s@c@o@p@e @psc                                     { constoken (DescriptorT TimelineScopeT) }
  @t@o@p @psc                                                              { constoken (DescriptorT TopT) }
  @t@o@u@c@h "-" @a@c@t@i@o@n @psc                                         { constoken (DescriptorT TouchActionT) }
  @t@r@a@n@s@f@o@r@m "-" @b@o@x @psc                                       { constoken (DescriptorT TransformBoxT) }
  @t@r@a@n@s@f@o@r@m "-" @o@r@i@g@i@n @psc                                 { constoken (DescriptorT TransformOriginT) }
  @t@r@a@n@s@f@o@r@m "-" @s@t@y@l@e @psc                                   { constoken (DescriptorT TransformStyleT) }
  @t@r@a@n@s@f@o@r@m @psc                                                  { constoken (DescriptorT TransformT) }
  @t@r@a@n@s@i@t@i@o@n "-" @b@e@h@a@v@i@o@r @psc                           { constoken (DescriptorT TransitionBehaviorT) }
  @t@r@a@n@s@i@t@i@o@n "-" @d@e@l@a@y @psc                                 { constoken (DescriptorT TransitionDelayT) }
  @t@r@a@n@s@i@t@i@o@n "-" @d@u@r@a@t@i@o@n @psc                           { constoken (DescriptorT TransitionDurationT) }
  @t@r@a@n@s@i@t@i@o@n "-" @p@r@o@p@e@r@t@y @psc                           { constoken (DescriptorT TransitionPropertyT) }
  @t@r@a@n@s@i@t@i@o@n "-" @t@i@m@i@n@g "-" @f@u@n@c@t@i@o@n @psc          { constoken
                                                                               (DescriptorT TransitionTimingFunctionT) }
  @t@r@a@n@s@i@t@i@o@n @psc                                                { constoken (DescriptorT TransitionT) }
  @t@r@a@n@s@l@a@t@e @psc                                                  { constoken (DescriptorT TranslateT) }
  @u@n@i@c@o@d@e "-" @b@i@d@i @psc                                         { constoken (DescriptorT UnicodeBidiT) }
  @u@n@i@c@o@d@e "-" @r@a@n@g@e @psc                                       { constoken (DescriptorT UnicodeRangeT) }
  @u@s@e@r "-" @m@o@d@i@f@y @psc                                           { constoken (DescriptorT UserModifyT) }
  @u@s@e@r "-" @s@e@l@e@c@t @psc                                           { constoken (DescriptorT UserSelectT) }
  @v@e@c@t@o@r "-" @e@f@f@e@c@t @psc                                       { constoken (DescriptorT VectorEffectT) }
  @v@e@r@t@i@c@a@l "-" @a@l@i@g@n @psc                                     { constoken (DescriptorT VerticalAlignT) }
  @v@i@e@w "-" @t@i@m@e@l@i@n@e "-" @a@x@i@s @psc                          { constoken (DescriptorT ViewTimelineAxisT) }
  @v@i@e@w "-" @t@i@m@e@l@i@n@e "-" @i@n@s@e@t @psc                        { constoken (DescriptorT ViewTimelineInsetT) }
  @v@i@e@w "-" @t@i@m@e@l@i@n@e "-" @n@a@m@e @psc                          { constoken (DescriptorT ViewTimelineNameT) }
  @v@i@e@w "-" @t@i@m@e@l@i@n@e @psc                                       { constoken (DescriptorT ViewTimelineT) }
  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @c@l@a@s@s @psc                    { constoken (DescriptorT ViewTransitionClassT) }
  @v@i@e@w "-" @t@r@a@n@s@i@t@i@o@n "-" @n@a@m@e @psc                      { constoken (DescriptorT ViewTransitionNameT) }
  @v@i@s@i@b@i@l@i@t@y @psc                                                { constoken (DescriptorT VisibilityT) }
  @w@h@i@t@e "-" @s@p@a@c@e "-" @c@o@l@l@a@p@s@e @psc                      { constoken (DescriptorT WhiteSpaceCollapseT) }
  @w@h@i@t@e "-" @s@p@a@c@e @psc                                           { constoken (DescriptorT WhiteSpaceT) }
  @w@i@d@o@w@s @psc                                                        { constoken (DescriptorT WidowsT) }
  @w@i@d@t@h @psc                                                          { constoken (DescriptorT WidthT) }
  @w@i@l@l "-" @c@h@a@n@g@e @psc                                           { constoken (DescriptorT WillChangeT) }
  @w@o@r@d "-" @b@r@e@a@k @psc                                             { constoken (DescriptorT WordBreakT) }
  @w@o@r@d "-" @s@p@a@c@i@n@g @psc                                         { constoken (DescriptorT WordSpacingT) }
  @w@o@r@d "-" @w@r@a@p @psc                                               { constoken (DescriptorT WordWrapT) }
  @w@r@i@t@i@n@g "-" @m@o@d@e @psc                                         { constoken (DescriptorT WritingModeT) }
  @x @psc                                                                  { constoken (DescriptorT XT) }
  @y @psc                                                                  { constoken (DescriptorT YT) }
  @z "-" @i@n@d@e@x @psc                                                   { constoken (DescriptorT ZIndexT) }
  @z@o@o@m @psc                                                            { constoken (DescriptorT ZoomT) }
-- end descriptor list
 }
 <comment> {
  [.\n]                                                ;
  @cmc                                                 { begin state_initial }
 }
 <htmlComment> {
  [.\n]                                                ;
  "-->"                                                { begin state_initial }
 }
 <nth_state> {
  $w @wo                                               { constoken Space }
  @e@v@e@n                                             { constoken (TNth Even) }
  @o@d@d                                               { constoken (TNth Odd) }
  @n                                                   { constoken TN }
  "+"                                                  { constoken (TPM TpmIdF) }
  "-"                                                  { constoken (TPM TpmNegF) }
  @int                                                 { tokenize (TInt . read) }
  ")"                                                  { constAndBegin TClose state_initial }
 }
 <lang_state> {
  @lang                                                { tokenize String }
  $w @wo                                               { skip }
  ")"                                                  { constAndBegin TClose state_initial }
 }

{

data TokenLoc = TokenLoc Token String (Maybe AlexPosn) deriving (Show, Eq)

getToken :: TokenLoc -> Token
getToken (TokenLoc t _ _) = t

type AlexUserState = ()

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
