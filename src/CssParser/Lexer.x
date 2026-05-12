{
module CssParser.Lexer where

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
import CssParser.Rule.Type qualified as F
import CssParser.Rule.TypedNum (PropValType(..))
import CssParser.Rule.Value (Ratio(..), readRatio)
import CssParser.Show (smartLookup)
import CssParser.TextMarshal
import CssParser.Utils(readCssString, readIdentifier)
import Data.Text (pack)
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
@selector = @s@e@l@e@c@t@o@r
@view    = @v@i@e@w
@try     = @t@r@y
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
@property = @p@r@o@p@e@r@t@y
@to      = @t@o
@from    = @f@r@o@m
@is      = @i@s
@has     = @h@a@s
@all     = @a@l@l
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
@lang    = [A-Za-z\-]+

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
  @a@t@t@r "("                                         { constAndBegin AttrFunT attr_fun_st }
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

  @anum ([a-zA-Z]+ | "%")?                             { tokenize TypedNum }

  @uint "/" @uint                                      { tokenize2 ((pure . RatioT) <=< readRatio) }
  "+"                                                  { constoken Plus }
  "-"                                                  { constoken Minus }

  @wo "<" [a-zA-Z\-]+ ">"                              { tokenizeE SyntaxTypeT (F.readSyntaxType . dropWhile isSpace) }

  @wo ">" @wo                                          { constoken Greater }
  @wo ">=" @wo                                         { constoken GreaterEqual }
  @wo "<" @wo                                          { constoken Less }
  @wo "<=" @wo                                         { constoken LessEqual }
  @wo $tl @wo                                          { constoken Tilde }
  "[" @wo                                              { constAndBegin BOpen attr_st }
  @wo "{" @wo                                          { constoken COpen }
  @wo "}" @wo                                          { constoken CClose }

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

  @pse [a-zA-Z0-9_\-]+                                 { tokenize (tokenizePseudoElement) }

  @psc @a@f@t@e@r                                      { constoken (PseudoElementT After) }
  @psc @b@e@f@o@r@e                                    { constoken (PseudoElementT Before) }

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

  @psc [a-zA-Z0-9_\-]+                                 { tokenize (tokenizePseudoClass) }

  @wo ")"                                              { constoken TClose }
  "("                                                  { constoken TOpen }

  @psc @wo                                             { constoken Colon }
  $w @wo                                               { constoken Space }
  @wo @cmo                                             { begin comment }
  "<!--"                                               { begin htmlComment }

  "--" @name @psc                                      { tokenize readCustomDescriptor }
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
 <attr_fun_st> {
  @r@a@w "-" @s@t@r@i@n@g                              { constoken RawStringT }
  @attrName                                            { tokenize (IdentT . pack . readIdentifier) }
  "%"                                                  { constoken PercentT }
  @wo ")"                                              { constAndBegin TClose start }
  "*"                                                  { constoken Asterisk }
  "|"                                                  { constoken Pipe }
  $w @wo                                               { constoken Space }
  @t@y@p@e "("                                         { constAndBegin TypeFunT start }
  @wo ","  @wo                                         { constAndBegin Comma start }
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
        case smartLookup it knownDescriptorMap of
          Just d -> do
            alexSetInput (pos `moveRightBy` (len + 1), ':', [], cis')
            pure (TokenLoc (DescriptorT $ KnownDescriptor d) i (Just pos))
          Nothing -> pure (TokenLoc (IdentT . pack $ readIdentifier i) i (Just pos))
    (i, _) ->
      let it = pack i in
        case smartLookup it descriptorKeywords of
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
