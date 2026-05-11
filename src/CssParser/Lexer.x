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
import Data.HashMap.Strict qualified as HM
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
@attrName = @nmstart @nmchar*

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
  @wo ","  @wo                                         { constoken Comma }
  (@wo ";" @wo)+                                       { constoken Semicolon }

  "!" @wo @i@m@p@o@r@t@a@n@t                           { constoken ImportantT }
  "@"                                                  { constoken (AtT I.Na) }
  "@" @moz                                             { constoken (AtT I.Moz) }
  "@" @ms                                              { constoken (AtT I.Microsoft) }
  "@" @apple                                           { constoken (AtT I.Apple) }
  "@" @opera                                           { constoken (AtT I.Opera) }
  "@" @webkit                                          { constoken (AtT I.WebKit) }

  @property $w @wo                                     { constoken PropertyT }
  @counter "-" @style $w @wo                           { constoken CounterStyleT }
  @charset $w @wo                                      { constoken CharsetT }
  @namespace $w @wo                                    { constoken NamespaceT }

  @import $w @wo                                       { constoken ImportT }
  @keyframes $w @wo                                    { constoken KeyframesT }
  @only @wo                                            { constoken OnlyT }
  @not @wo                                             { constoken NotT }
  @or @wo                                              { constoken OrT }
  @and @wo                                             { constoken AndT }
  @selector "("                                        { constoken SelectorFunT }
  @c@a@l@c "("                                         { constoken CalcFunT }
  @t@y@p@e "("                                         { constoken TypeFunT }
  @url "("                                             { constoken UrlT }
  @url "(" [^\"\'\)]* ")"                              { tokenize (UnquotedUrlT . readUnquotedUrl) }
  "."                                                  { constoken Dot }
  "." @ident                                           { tokenize (ClassT . readIdentifier . drop 1) }
  "*"                                                  { constoken Asterisk }
  "&"                                                  { constoken Ampersand }
  "|"                                                  { constoken Pipe }
  @wo "/"                                              { constoken DivT }
  @ident                                               { tokenizeDescriptor }
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
  "[" @wo                                              { constAndBegin BOpen attr_st }
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

  "--" @name @psc                                      { tokenize (readCustomDescriptor) }
  @moz @name @psc                                      { tokenize (readBpDescriptor I.Moz) }
  @ms @name @psc                                       { tokenize (readBpDescriptor I.Microsoft) }
  @apple @name @psc                                    { tokenize (readBpDescriptor I.Apple) }
  @opera @name @psc                                    { tokenize (readBpDescriptor I.Opera) }
  @webkit @name @psc                                   { tokenize (readBpDescriptor I.WebKit) }
 }
 <comment> {
  [.\n]                                                ;
  @cmc                                                 { begin start }
 }
 <attr_st> {
  @attrName                                            { tokenize (IdentT . pack . readIdentifier) }
  @wo "]"                                              { constAndBegin BClose start }
  "*"                                                  { constoken Asterisk }
  "|"                                                  { constoken Pipe }
  @wo "=" @wo                                          { constAndBegin TEqual attr_pat_st }
  @wo "~=" @wo                                         { constAndBegin TIncludes attr_pat_st }
  @wo "|=" @wo                                         { constAndBegin TDashMatch attr_pat_st }
  @wo "^=" @wo                                         { constAndBegin TPrefixMatch attr_pat_st }
  @wo "$=" @wo                                         { constAndBegin TSuffixMatch attr_pat_st }
  @wo "*=" @wo                                         { constAndBegin TSubstringMatch attr_pat_st }
  @wo @cmo                                             { begin comment }
  "<!--"                                               { begin htmlComment }
 }
 <attr_pat_st> {
  @string                                              { tokenize (String . readCssString) }
  @name                                                { tokenize (AttrPatT . pack . readIdentifier) }
  $w @wo                                               { constoken Space }
  @wo @cmo                                             { begin comment }
  "<!--"                                               { begin htmlComment }
  @wo "]"                                              { constAndBegin BClose start }
 }
 <htmlComment> {
  [.\n]                                                ;
  "-->"                                                { begin start }
 }
 <nth_state> {
  $w @wo                                               { constoken Space }
  @e@v@e@n                                             { constoken (TNth Even) }
  @o@d@d                                               { constoken (TNth Odd) }
  @n                                                   { constoken TN }
  "+"                                                  { constoken (TPM TpmIdF) }
  "-"                                                  { constoken (TPM TpmNegF) }
  @int                                                 { tokenize (TInt . read) }
  ")"                                                  { constAndBegin TClose start }
 }
 <lang_state> {
  @lang                                                { tokenize String }
  $w @wo                                               { skip }
  ")"                                                  { constAndBegin TClose start }
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

moveRightBy :: AlexPosn -> Int -> AlexPosn
moveRightBy (AlexPn a l c) n = AlexPn (a+n) l (c+n)

tokenizeDescriptor :: AlexInput -> Int -> Alex TokenLoc
tokenizeDescriptor (pos, _pc, _bs, cis) len =
  case splitAt len cis of
    (i, ':':cis') ->
      let it = pack i in
        case HM.lookup it knownDescriptorMap of
          Just d -> do
            alexSetInput (pos `moveRightBy` (len + 1), ':', [], cis')
            pure (TokenLoc (DescriptorT $ KnownDescriptor d) i (Just pos))
          Nothing -> pure (TokenLoc (IdentT . pack $ readIdentifier i) i (Just pos))
    (i, _) ->
      let it = pack i in
        case HM.lookup it descriptorKeywords of
          Just d ->
            pure (TokenLoc d i (Just pos))
          Nothing ->
            pure (TokenLoc (IdentT . pack $ readIdentifier i) i (Just pos))

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

start :: Int
start = 0

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
