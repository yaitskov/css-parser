--vim:ft=haskell
{
module CssParser.Lexer where

import Control.Monad ((<=<))
import CssParser.At.Function (AtomicCssType)
import CssParser.At.Function qualified as F
import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page
import CssParser.Fun
import CssParser.Ident qualified as I
import CssParser.Rule hiding (Heading, Host)
import CssParser.Rule.Pseudo hiding (Left, Right, ViewTransition)
import CssParser.Rule.Pseudo qualified as P
import CssParser.Rule.Value (Ratio(..), readRatio)
import CssParser.TextMarshal
import CssParser.Utils(readCssString, readIdentifier, dropEnd)
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

  @r@e@s@u@l@t                                         { constoken ResultT }
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
  "*"                                                  { constoken Asterisk }
  "&"                                                  { constoken Ampersand }
  "|"                                                  { constoken Pipe }
  @wo "/"                                              { constoken DivT }
  @ident                                               { tokenize (Ident . readIdentifier) }
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
  @anum @g@r@a@d                                       { tokenize (Grad     . dropEnd 4) }
  @anum @i@c                                           { tokenize (Ic       . dropEnd 2) }
  @anum @i@n                                           { tokenize (In       . dropEnd 2) }
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
type NumberStr = String

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
    | Var String
    | THash String
    | UnitLessNum String

    | Deg NumberStr
    | Rad NumberStr
    | Grad NumberStr
    | Turn NumberStr

    | Px NumberStr
    | Mm NumberStr
    | Ms NumberStr
    | Cm NumberStr
    | Em NumberStr
    | Vh NumberStr
    | Vw NumberStr
    | Rem NumberStr
    | Dpi NumberStr
    | Rcap NumberStr
    | Cap NumberStr
    | Ch NumberStr
    | Rch NumberStr
    | Ex NumberStr
    | Rex NumberStr
    | Lh NumberStr
    | Rlh NumberStr
    | Ic NumberStr
    | Ric NumberStr
    | Pc NumberStr
    | In NumberStr
    | Pt NumberStr
    | Q NumberStr
    | Second NumberStr
    | Svh NumberStr
    | Dvh NumberStr
    | Lvh NumberStr
    | Vb NumberStr
    | Lvb NumberStr
    | Dvb NumberStr
    | Svb NumberStr
    | Vi NumberStr
    | Lvi NumberStr
    | Dvi NumberStr
    | Svi NumberStr
    | Vmax NumberStr
    | Lvmax NumberStr
    | Dvmax NumberStr
    | Svmax NumberStr
    | Vmin NumberStr
    | Lvmin NumberStr
    | Dvmin NumberStr
    | Svmin NumberStr
    | Cqw NumberStr
    | Cqh NumberStr
    | Cqi NumberStr
    | Cqb NumberStr
    | Cqmax NumberStr
    | Cqmin NumberStr
    | Percents NumberStr

    | RatioT Ratio
    | Comma
    | Ampersand
    | Colon
    | Semicolon
    | Pipe
    | Plus
    | SharpT
    | Minus
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Tilde
    | Dot

    | PageT
    | PageMarginT PageMargin

    | ResultT
    | ReturnsT
    | SelectorFunT
    | CalcFunT
    | TypeFunT
    | SyntaxTypeT AtomicCssType
    | FunctionT
    | ImportantT
    | SupportsT
    | ScopeT
    | ViewTransitionT
    | StartingStyleT
    | PositionTryT
    | ContainerT
    | FromT
    | ToT
    | AtT I.BrowserPrefix
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
    | MediaTypeT MediaType
    | UnquotedUrlT String
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
    | TActiveViewTransitionType

    | TDir
    | THeading
    | THost
    | TState

    | THighlight
    | TPart
    | TPicker
    | TScrollButton
    | TSlotted
    | TViewTransitionGroup
    | TViewTransitionImagePair
    | TViewTransitionNew
    | TViewTransitionOld

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
