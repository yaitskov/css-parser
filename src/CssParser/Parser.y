{
module CssParser.Parser where

import CssParser.At.Container
import CssParser.At.CustomMedia
import CssParser.At.FontFeatureValues
import CssParser.At.FontPaletteValues
import CssParser.At.Function qualified as F
import CssParser.At.Import
import CssParser.At.Keyframe
import CssParser.At.MediaQuery
import CssParser.At.Page
import CssParser.At.Supports hiding (FeatureQuery)
import CssParser.Descriptor (Descriptor, toPropertyName)
import CssParser.File
import CssParser.FixRule
import CssParser.Ident hiding (Ident, Namespace, Var)
import CssParser.Ident qualified as R
import CssParser.Lexer (AlexPosn(AlexPn), TokenLoc(TokenLoc))
import CssParser.Lexer.Token qualified as L
import CssParser.Lexer.Token
  ( Token
    ( TIncludes, TEqual, TDashMatch, TPrefixMatch, TSuffixMatch, TSubstringMatch, IdentT
    , Comma, Plus, SharpT, Minus, Tilde, Dot, Asterisk, Space, BOpen, BClose
    , PseudoElementT, TInt, TNot, TLang, String, THash
    , COpen, CClose, Colon, Semicolon, Var, Pipe, AtomicPseudoClassT, Ampersand
    , CharsetT, ImportT, MediaT, LayerT, NamespaceT, CounterStyleT, PropertyT
    , NotT, OfT, OrT, AndT, OnlyT, ReturnsT, GlobalT, RawStringT, DefineMixinT
    , TOpen, TClose, DescriptorT, ClassT, AttrPatT, PercentT, MixinT
    , Greater, Less, LessEqual, GreaterEqual, AttrFunT, AlphaT
    , RatioT, ImportantT, MediaTypeT, CalcFunT, TypeFunT, FunctionT, SyntaxTypeT
    , UrlT, UnquotedUrlT, TWhere, THas, TIs, PageT, PageMarginT, CustomSelectorT
    , KeyframesT, ColorProfileT, FontFaceT, UnicodeRangeVal, CustomMediaT, TrueT, FalseT
    , FontFeatureValuesT, AtT, FontPaletteValuesT, ContainerT, DivT, PositionTryT
    , StartingStyleT, ViewTransitionT, ScopeT, ToT, FromT, SupportsT, SelectorFunT
    , TActiveViewTransitionType, TDir, THeading, THost, TState, EvenT, OddT
    , THighlight, TPart, TPicker, TScrollButton, TSlotted, TViewTransitionGroup
    , TViewTransitionImagePair, TViewTransitionNew, TViewTransitionOld
    , NthChildT, NthOfTypeT, NthLastChildT, NthLastOfTypeT
    )
  )
import CssParser.MonoPair
import CssParser.Norm
import CssParser.Parser.Monad
import CssParser.Prelude
  ( mapMaybe, prependList, NonEmpty((:|)), (<|), leftToMaybe
  , rightToMaybe, These(..), fromMaybe
  )
import CssParser.Rule
import CssParser.Rule.Pseudo qualified as P
import CssParser.Rule.Pseudo hiding (Left, Right, ViewTransition, Heading, Host)
import CssParser.Rule.Type qualified as T
import CssParser.Rule.TypedNum
import CssParser.Rule.Value hiding (UnicodeRangeVal)
import CssParser.Rule.Value qualified as Vl
import CssParser.Show
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Prelude
}

%monad { P } { thenP } { returnP }
%name cssParser
%tokentype { TokenLoc }
%error { happyError }

%token
    ','         { TokenLoc Comma _ _ }
    ':'         { TokenLoc Colon _ _ }
    ';'         { TokenLoc Semicolon _ _ }
    '>'         { TokenLoc Greater _ _ }
    '>='        { TokenLoc GreaterEqual _ _ }
    '<'         { TokenLoc Less _ _ }
    '<='        { TokenLoc LessEqual _ _ }
    '+'         { TokenLoc Plus _ _ }
    '%'         { TokenLoc PercentT _ _ }
    '#'         { TokenLoc SharpT _ _ }
    '-'         { TokenLoc Minus _ _ }
    '|'         { TokenLoc Pipe _ _ }
    '~'         { TokenLoc Tilde _ _ }
    '.'         { TokenLoc Dot _ _ }
    ' '         { TokenLoc Space _ _ }
    '*'         { TokenLoc Asterisk _ _ }
    '&'         { TokenLoc Ampersand _ _ }
    '['         { TokenLoc BOpen _ _ }
    ']'         { TokenLoc BClose _ _ }
    '{'         { TokenLoc COpen _ _ }
    '}'         { TokenLoc CClose _ _ }
    '='         { TokenLoc TEqual _ _ }
    attrPat     { TokenLoc (AttrPatT $$) _ _ }
    mediaType   { TokenLoc (MediaTypeT $$) _ _ }
    charset     { TokenLoc CharsetT _ _ }
    alpha       { TokenLoc AlphaT _ _ }
    '@'         { TokenLoc (AtT $$) _ _ }
    important   { TokenLoc ImportantT _ _ }
    supports    { TokenLoc SupportsT _ _ }

    returns     { TokenLoc ReturnsT _ _ }
    viewTransition
                { TokenLoc ViewTransitionT _ _ }
    startingStyle
                { TokenLoc StartingStyleT _ _ }
    positionTry { TokenLoc PositionTryT _ _ }
    fontPaletteValues
                { TokenLoc FontPaletteValuesT _ _ }
    fontFeatureValues
                { TokenLoc FontFeatureValuesT _ _ }
    'to'        { TokenLoc ToT _ _ }
    from        { TokenLoc FromT _ _ }
    scope       { TokenLoc ScopeT _ _ }
    container   { TokenLoc ContainerT _ _ }
    property    { TokenLoc PropertyT _ _ }
    colorProfile
                { TokenLoc ColorProfileT _ _ }
    namespace   { TokenLoc NamespaceT _ _ }
    keyframes   { TokenLoc KeyframesT _ _ }
    counterStyle
                { TokenLoc CounterStyleT _ _ }
    fontFace    { TokenLoc FontFaceT _ _ }
    import      { TokenLoc ImportT _ _ }
    layer       { TokenLoc LayerT _ _ }
    page        { TokenLoc PageT _ _ }
    pageMargin  { TokenLoc (PageMarginT $$) _ _ }
    media       { TokenLoc MediaT _ _ }
    mixin       { TokenLoc MixinT _ _ }
    defineMixin { TokenLoc DefineMixinT _ _ }
    true        { TokenLoc TrueT _ _ }
    false       { TokenLoc FalseT _ _ }
    customMedia { TokenLoc CustomMediaT _ _ }
    customSelector
                { TokenLoc CustomSelectorT _ _ }
    only        { TokenLoc OnlyT _ _ }
    'not'       { TokenLoc NotT _ _ }
    or          { TokenLoc OrT _ _ }
    of          { TokenLoc OfT _ _ }
    and         { TokenLoc AndT _ _ }
    'url('      { TokenLoc UrlT _ _ }
    'uqUrl'     { TokenLoc (UnquotedUrlT $$) _ _ }
    'selector(' { TokenLoc SelectorFunT _ _ }
    calcFns     { TokenLoc (CalcFunT $$) _ _ }
    'attr('     { TokenLoc AttrFunT _ _ }
    rawString   { TokenLoc RawStringT _ _ }
    'type('     { TokenLoc TypeFunT _ _ }
    function    { TokenLoc FunctionT _ _ }
    syntaxType  { TokenLoc (SyntaxTypeT $$) _ _ }
    '^='        { TokenLoc TPrefixMatch _ _ }
    '$='        { TokenLoc TSuffixMatch _ _ }
    '*='        { TokenLoc TSubstringMatch _ _ }
    '|='        { TokenLoc TDashMatch _ _ }
    '~='        { TokenLoc TIncludes _ _ }
    unRangeVal  { TokenLoc (UnicodeRangeVal $$) _ _ }
    class       { TokenLoc (ClassT $$) _ _ }
    ident       { TokenLoc (IdentT $$) _ _ }
    string      { TokenLoc (String $$) _ _ }
    hash        { TokenLoc (THash $$) _ _ }
    descriptor  { TokenLoc (DescriptorT $$) _ _ }
    pseude      { TokenLoc (PseudoElementT $$) _ _ }
    highlight   { TokenLoc THighlight _ _ }
    part        { TokenLoc TPart _ _ }
    picker      { TokenLoc TPicker _ _ }
    scrollButton
                { TokenLoc  TScrollButton _ _ }
    slotted     { TokenLoc  TSlotted _ _ }
    viewTransitionGroup
                { TokenLoc TViewTransitionGroup _ _ }
    viewTransitionImagePair
                { TokenLoc TViewTransitionImagePair _ _ }
    viewTransitionNew
                { TokenLoc TViewTransitionNew _ _ }
    viewTransitionOld
                { TokenLoc TViewTransitionOld _ _ }
    even        { TokenLoc EvenT _ _ }
    odd         { TokenLoc OddT _ _ }
    nthChild    { TokenLoc NthChildT _ _ }
    nthLastChild
                { TokenLoc NthLastChildT _ _ }
    nthOfType   { TokenLoc NthOfTypeT _ _ }
    nthLastOfType
                { TokenLoc NthLastOfTypeT _ _ }
    pseudc      { TokenLoc (AtomicPseudoClassT $$) _ _ }
    int         { TokenLoc (TInt $$) _ _ }
    'ratio'     { TokenLoc (RatioT $$) _ _ }
    typedNum    { TokenLoc (L.TypedNum $$) _ _ }
    var         { TokenLoc (Var $$) _ _ }
    ':not'      { TokenLoc TNot _ _ }
    ':global'   { TokenLoc GlobalT _ _ }
    ':where'    { TokenLoc TWhere _ _ }
    ':is'       { TokenLoc TIs _ _ }
    ':has'      { TokenLoc THas _ _ }
    ':lang('    { TokenLoc TLang _ _ }
    ':dir'      { TokenLoc TDir _ _ }
    heading     { TokenLoc THeading _ _ }
    host        { TokenLoc THost _ _ }
    ':state('   { TokenLoc TState _ _ }
    activeViewTransitionType
                { TokenLoc TActiveViewTransitionType _ _ }
    '('         { TokenLoc TOpen _ _ }
    ')'         { TokenLoc TClose _ _ }
    '/'         { TokenLoc DivT _ _ }

%%

CssFile :: { CssFile }
    : CssFileBody                                 { CssFile $1 }
Import -- :: { Import SelectorList }
    : Source Os                                   { ImportUrlSupports $1 Nothing [] }
    | Source Os layer Os                          { ImportDefaultLayer $1 }
    | Source Os layer LayerNameMb Os              { ImportUrlLayer $1 $4 Nothing [] }
    | Source Os layer LayerNameMb Os Supports Os  { ImportUrlLayer $1 $4 (Just $6) [] }
    | Source Os layer LayerNameMb Os Supports Os MediaQueryList Os
                                                  { ImportUrlLayer $1 $4 (Just $6) $8 }
    | Source Os layer LayerNameMb Os MediaQueryList Os
                                                  { ImportUrlLayer $1 $4 Nothing $6 }
    | Source Os Supports Os                       { ImportUrlSupports $1 (Just $3) [] }
    | Source Os Supports Os MediaQueryList Os     { ImportUrlSupports $1 (Just $3) $5 }
    | Source Os MediaQueryList Os                 { ImportUrlSupports $1 Nothing $3 }
LayerNameMb :: { Maybe LayerName }
    : P(Maybe(LayerName))                         { $1 }
Supports :: { FeatureQuery }
    : supports Op MediaFeature ')'                { FqMediaFeature $3 }
    | supports Op FeatureQuery ')'                { normalize $3 }
Source
    : Url                                         { UrlSource $1 }
    | Str                                         { StrSource $1 }
LayerNames :: { [LayerName] }
    : LayerName                                   { [$1] }
    | LayerName ',' LayerNames                    { $1 : $3 }
LayerName :: { LayerName }
    : Os IdKwd                                    { LayerName $2 }
CssFileBody :: { [ CssRule ] }
    :                                             { [] }
    | Os CssRule Os CssFileBody                   { $2 : $4 }
CssRule :: { CssRule }
    : SelectorList ERB                            { CssRule $1 $2 }
    | '@' AtRule                                  { AtRule $1 $2 }
AtRule :: { AtRule }
    : media Os '{' OsCssRuleBody '}'              { MediaRule (MediaQueryList []) $4 }
    | media Os MediaQueryList ERB                 { MediaRule (MediaQueryList $3) $4 }
    | customMedia Os Var Os CustomMediaQuery ';'  { CustomMedia $3 $5 }
    | namespace Os IdKwdMb Os Source ';'          { Namespace $3 $5 }
    | import Os Import ';'                        { ImportStmt $3 }
    | mixin Os IdKwd ';'                          { Mixin $3 }
    | defineMixin Os IdKwd ERB                    { DefineMixin $3 $4 }
    | customSelector Os pseudc Os SelectorList ';'
                                                  {% mkCustomSelector $3 $5 }
    | charset Os Str ';'                          { CharsetStmt (Charset $3) }
    | layer ' ' LayerName ';'                     { LayerStmt (pure $3) }
    | layer ' ' LayerName ',' LayerNames Os ';'   { LayerStmt ($3 :| $5) }
    | layer ' ' LayerName ERB                     { LayerBlock (Just $3) $4 }
    | layer ' ' '{' OsCssRuleBody '}'             { LayerBlock Nothing $4 }
    | layer '{' OsCssRuleBody '}'                 { LayerBlock Nothing $3 }
    | page Os '{' OsCssRuleBody '}'               { Page (PageSelectorList []) $4 }
    | page Os PageSelectorList ERB                { Page (PageSelectorList $3) $4 }
    | pageMargin ERB                              { PageMarginBlock $1 $2 }
    | counterStyle Os IdKwd ERB                   { CounterStyle $3 $4 }
    | property Os Var ERB                         { Property $3 $4 }
    | keyframes Os IdKwd Ocb SepList(Os, Keyframe) '}'
                                                  { Keyframes (KeyframeSet (KeyframeSetName $3) $5) }
    | colorProfile Os PropN Os ERB                { ColorProfile $3 $5 }
    | fontFace Os ERB                             { FontFaceBlock $3 }
    | fontFeatureValues ' ' StrEitherIds Os Ocb FontFeatureValBlocks '}'
                                                  { FontFeatureValuesBlock
                                                      (FontFeatureValues
                                                        $3
                                                        (mapMaybe leftToMaybe $6)
                                                        (mapMaybe rightToMaybe $6))
                                                  }
    | fontPaletteValues ' ' Var Os Ocb PropEntries '}'
                                                  { FontPaletteValuesBlock (FontPaletteValues $3 $6) }
    | container Os ContainerQueryMap ERB          { Container (ContainerQueryMap $3) $4 }
    | positionTry Os Var Ocb PropEntries '}'      { PositionTry $3 $5 }
    | startingStyle Os ERB                        { StartingStyle $3 }
    | viewTransition Os ERB                       { ViewTransition $3 }
    | scope Os SelectorPair ERB                   { ScopeBlock $3 $4 }
    | function Os Function                        { FunctionBlock $3 }
    | supports Os FeatureQuery ERB                { Supports (normalize $3) $4 }
    | Ident Os CommaSeparatedList Os ERB          { UnknownGramma $1 (Just (CommaSeparatedList $3)) $5 }
    | Ident Os ERB                                { UnknownGramma $1 Nothing $3 }
Bool :: { Bool }
    : true                                        { True }
    | false                                       { False }
CustomMediaQuery :: { CustomMediaQuery }
    : Bool                                        { CustomMediaFlag $1 }
    | MediaQueryList                              { CustomMediaQuery (MediaQueryList $1) }
Function :: { CssFunction }
    : Var Op FunArgs Os ')' Os RetType Os Ocb FunEntries CssFileBody '}'
                                                  { F.Function $1 $3 $7 (snd $10) (fst $10) $11 }
FunArgs :: { [ F.FunArg ] }
    :                                             { [] }
    | FunArg                                      { [ $1 ] }
    | FunArg Os ',' Os FunArgs                    { $1 : $5 }
FunArg :: { F.FunArg }
    : Var                                         { F.FunArg $1 Nothing Nothing }
    | Var Os TypeFun                              { F.FunArg $1 (Just $3) Nothing }
    | Var Os TypeFun Os ':' Os PropVal            { F.FunArg $1 (Just $3) (Just $7) }
    | Var Os ':' Os PropVal                       { F.FunArg $1 Nothing (Just $5) }
FunEntries :: { (NonEmpty PropVals, [ F.ConstEntry ]) }
    : LocalConst                                  {% findResult $1 }
LocalConst :: { [Either F.ConstEntry (NonEmpty PropVals)] }
    :                                             {% pure [] }
    | descriptor Os PropParValsList               {% fmap (:[]) (varOrResult $1 $3) }
    | descriptor Os PropParValsList ';' LocalConst
                                                  {% fmap (:$5) (varOrResult $1 $3) }
RetType :: { Maybe T.CssType }
    :                                             { Nothing }
    | returns Os TypeFun                          { Just $3 }
TypeFun :: { T.CssType }
    : 'type(' Os CssType Os ')'                   { $3 }
    | CssLeaf                                     { T.Once $1 }
    | '*'                                         { T.AnyCssType }
CssType :: { T.CssType }
    : '*'                                         { T.AnyCssType }
    | CssLeaf                                     { T.Once $1 }
    | CssLeaf '#'                                 { T.CommaSeparated $1 }
    | CssLeaf '+'                                 { T.SpaceSeparated $1 }
    | CssLeaf Os '|' Os CssType                   { T.OrLeaf $1 $5 }
CssLeaf :: { T.CssLeafType }
    : syntaxType                                  { T.AtomicCssType $1 }
FeatureQuery :: { FeatureQuery }
    : Op MediaFeature ')'                         { FqMediaFeature $2 }
    | Op FeatureQuery ')'                         { FqParen $2 }
    | FeatureQuery Os BOP FeatureQuery            { FqBop $3 $1 $4 }
    | 'not' Os FeatureQuery                       { FqNot $3 }
    | 'not' Os FeatureQuery Os BOP FeatureQuery   { FqBop $5 (FqNot $3) $6 }
    | 'selector(' SelectorList ')'                { FqApp (FqSelectorFun $2) }
    | Ident Op PropVals ')'                       { FqApp (FqSomeFun $1 $3) }
SL :: { SelectorList }
    :  SelectorList ')'                           { $1 }
ESL :: { SelectorList }
    : '(' SelectorList ')'                        { $2 }
SelectorPair :: { MonoPair SelectorList }
    :                                             { EmptyPair }
    | ESL Os                                      { HalfPair $1 }
    | ESL Os 'to' Os ESL Os                       { FullPair $1 $5 }
OsCssRuleBody :: { [CssRuleBodyItem] }
    : Os CssRuleBody                              { $2 }
ERB :: { [CssRuleBodyItem] } -- Embraced Rule Body
    : '{' OsCssRuleBody '}'                       { $2 }
ContainerQueryMap :: { NonEmpty (These R.Ident ContainerQuery) }
    : NonEmpty(',', IdContainerQuery)             { $1 }
IdContainerQuery :: { These R.Ident ContainerQuery }
    : Ident                                       { This $1 }
    | Ident ' ' Ident Os Op CQ ')'                { These $1 (CqFeature (AsIs (CqApp $3 $6))) }
    | Ident ' ' Ident Os Op CQ ')' Os BOP CQ      { These $1
                                                      (CqBin
                                                        $9
                                                        (AsIs
                                                          (CqApp $3 $6))
                                                        $10)
                                                  }
    | Ident ' ' CQ                                { These $1 $3 }
    | Ident Op CQ ')'                             { That (CqFeature (AsIs (CqApp $1 $3))) }
    | Ident Op CQ ')' Os BOP CQ                   { That
                                                      (CqBin
                                                        $6
                                                        (AsIs
                                                          (CqApp $1 $3))
                                                        $7)
                                                  }
    | CQ                                          { That $1 }
DescAsPropName :: { PropertyName }
    : descriptor                                  { toPropertyName $1 }
CQ :: { ContainerQuery }
    : Ident Os Op CQ ')'                          { CqFeature (AsIs (CqApp $1 $4)) }
    | Ident Os Op CQ ')' Os BOP CQ                { CqBin
                                                      $7
                                                      (AsIs (CqApp $1 $4))
                                                      $8
                                                  }
    | DescAsPropName Os PropParVals               { CqFeature (AsIs (CqOpFeature (PlainMf $1 $3))) }
    | PropN Os ':' PropParVals                    { CqFeature (AsIs (CqOpFeature (PlainMf $1 $4))) }
    | 'not' Os Op MediaFeature ')' Os BOP CQ      { CqBin $7 (Not (CqOpFeature $4)) $8 }
    | 'not' Os Op MediaFeature ')'                { CqFeature (Not (CqOpFeature $4)) }
    | 'not' Os Ident Os Op CQ ')'                 { CqFeature (Not (CqApp $3 $6)) }
    | 'not' Os Ident Os Op CQ ')' Os BOP CQ       { CqBin $9 (Not (CqApp $3 $6)) $10 }
    | Op MediaFeature ')' Os BOP CQ               { CqBin $5 (AsIs (CqOpFeature $2)) $6 }
    | Op MediaFeature ')'                         { CqFeature (AsIs (CqOpFeature $2)) }
BOP :: { BinOp }
    : and Os                                      { And }
    | or Os                                       { Or }
FontFeatureValBlocks :: { [ Either PropEntry FontFeatureValuesSubBlock ] }
    :                                             { [] }
    | FontFeatureValBlock FontFeatureValBlocks    { Right $1 : $2 }
    | PropEntry FontFeatureValBlocks              { Left $1 : $2 }
FontFeatureValBlock :: { FontFeatureValuesSubBlock }
    : '@' IdKwd Os Ocb FontFeatureEntries '}'     { FontFeatureValuesSubBlock $1 $2 $5 }
FontFeatureEntries :: { [ FontFeatureEntry ] }
    :                                             { [] }
    | FontFeatureEntry                            { [$1] }
    | FontFeatureEntry ';'                        { [$1] }
    | FontFeatureEntry ';' FontFeatureEntries     { $1 : $3 }
FontFeatureEntry :: { FontFeatureEntry }
    : IdKwd Os ':' Os NonEmpty(' ', Unsigned)     { FontFeatureEntry $1 (SslNe $5) }
    | Desc Os NonEmpty(' ', Unsigned)             {% identOnly
                                                       (toPropertyName $1)
                                                       (`FontFeatureEntry` (SslNe $3))
                                                  }
StrEitherIds :: { Either LiteralString IdentList }
    : Str                                         { Left (LiteralString $1) }
    | NonEmpty(' ', IdKwd)                        { Right (IdentList $1) }
CommaSeparatedList :: { NonEmpty PropVals }
    : CssPropertyVals Important                   { (PropVals $1 $2) :| [] }
    | CssPropertyVals Important ',' CommaSeparatedList
                                                  { (PropVals $1 $2) <| $4 }
UnicodeRange :: { UnicodeRange }
    : unRangeVal                                  { UnicodeRange (pack $1) }
Keyframe :: { Keyframe }
    : Os NonEmpty(',', KeyframeAdr) Os Ocb PropEntries '}'
                                                  { Keyframe (CslNe $2) $5 }
TypedNum :: { TypedNum }
    : typedNum                                    {% parseTypedNum $1 }
KeyframeAdr
    : from                                        { KeyframeStart }
    | 'to'                                        { KeyframeEnd }
    | TypedNum                                    {% fmap KeyframePercentAdr (tryGet Percent $1) }
PropEntries :: { [PropEntry] }
    : List(PropEntry)                             { $1 }
Desc :: { Descriptor }
    : descriptor                                  { $1 }
PropN :: { PropertyName }
    : IdKwd                                       { PropertyName $1 }
    | Var                                         { VarProp $1 }
PropEntry :: { PropEntry }
    : descriptor Os PropParVals ';'               { PropEntry $1 $3 }
    | descriptor Os PropParVals                   { PropEntry $1 $3 }
PageSelectorList
    : PageSelector                                { [ $1 ] }
    | PageSelector Os PageSelectorList            { $1 : $3 }
PageSelector
    : IdKwd PseudoPageList                        { PageSelector (Just (PageName $1)) $2 }
    | IdKwd                                       { PageSelector (Just (PageName $1)) [] }
    | PseudoPageList                              { PageSelector Nothing $1 }
PseudoPageList
    : pseudc                                      { [$1] }
    | heading                                     { [P.Heading] }
    | host                                        { [P.Host] }
    | pseudc PseudoPageList                       { $1 : $2 }
    | heading PseudoPageList                      { P.Heading : $2 }
    | host PseudoPageList                         { P.Host : $2 }
IdKwdMb
    :                                             { Nothing }
    | IdKwd                                       { Just $1 }
MediaQueryList :: { [ MediaQuery ] }
    : MediaQuery                                  { [ $1 ] }
    | MediaQuery ',' MediaQueryList               { $1 : $3 }
    | MediaQuery or Os MediaQueryList             { $1 : $4 }
MediaQuery :: { MediaQuery }
    : 'not' Os MediaCondition                     {% massageExpr MediaQueryConditionOnly (NotMc $3) }
    | 'not' Os MediaCondition Os MediaAnds        {% massageExpr MediaQueryConditionOnly
                                                                 (mkBopMcTree And (NotMc $3 :| $5)) }
    | 'not' Os MediaCondition Os MediaOrs         {% massageExpr MediaQueryConditionOnly
                                                                 (mkBopMcTree Or  (NotMc $3 :| $5)) }
    | 'not' Os MediaType Os and Os MediaCondition {% massageExpr (MediaQueryWithMt (Just MtNot) $3 . Just) $7 }
    | 'not' Os MediaType Os                       { MediaQueryWithMt (Just MtNot) $3 Nothing }
    | only Os MediaType Os                        { MediaQueryWithMt (Just MtOnly) $3 Nothing }
    | only Os MediaType Os and Os MediaCondition  {% massageExpr (MediaQueryWithMt (Just MtOnly) $3 . Just) $7 }
    | MediaType Os                                { MediaQueryWithMt Nothing $1 Nothing }
    | MediaType Os and Os MediaCondition          {% massageExpr (MediaQueryWithMt Nothing $1 . Just) $5 }
    | MediaCondition                              {% massageExpr MediaQueryConditionOnly $1 }
MediaType :: { MediaType }
    : mediaType                                   { $1 }
MediaCondition :: { MediaCondition }
    : MediaNot                                    { $1 }
    | MediaNot Os MediaAnds                       { mkBopMcTree And ($1 :| $3) }
    | MediaNot Os MediaOrs                        { mkBopMcTree Or ($1 :| $3) }
    | MediaInParens                               { $1 }
    | MediaInParens Os MediaAnds                  { mkBopMcTree And ($1 :| $3) }
    | MediaInParens Os MediaOrs                   { mkBopMcTree Or ($1 :| $3) }
MediaNot :: { MediaCondition }
    : 'not' Os MediaInParens                      { NotMc $3 }
MediaAnds :: { [MediaCondition] }
    : MediaAnd                                    { [$1] }
    | MediaAnd Os MediaAnds                       { $1 : $3 }
MediaAnd :: { MediaCondition }
    : and Os MediaInParens                        { $3 }
    | and Os MediaNot                             { $3 }
MediaOrs :: { [MediaCondition] }
    : MediaOr                                     { [$1] }
    | MediaOr Os MediaOrs                         { $1 : $3 }
MediaOr :: { MediaCondition }
    : or Os MediaInParens                         { $3 }
    | or Os MediaNot                              { $3 }
MediaInParens
    : Op MediaCondition ')'                       { ParenMc $2 }
    | Op MediaFeature ')'                         { FeatureMc $2 }
MediaFeature :: { MediaFeature }
    : DescAsPropName Os PropParVals               { PlainMf $1 $3 }
    | PropN Os ':' Os PropParVals                 { PlainMf $1 $5 }
    | PropN MfRel PropParVal                      { OpenRangeFeature $1 $2 $3 }
    | PropN MfRel PropN MfRel PropParVal          { MfClosedRange (propRef $1) $2 $3 $4 $5 }
    | PropN Op PropParVals ')' MfRel PropN        { OpenRangeFeatureFlipped
                                                      (AppFun $1 $3)
                                                      $5
                                                      $6
                                                  }
    | PropN Op PropParVals ')' '/' Os PropParVal MfRel PropN
                                                  { OpenRangeFeatureFlipped
                                                      (Div
                                                        (AppFun $1 $3)
                                                        $7)
                                                      $8 $9
                                                  }
    | PropN Op PropParVals ')' '/' Os PropParVal MfRel PropN MfRel PropParVal
                                                  { MfClosedRange
                                                      (Div
                                                        (AppFun $1 $3)
                                                        $7)
                                                      $8 $9 $10 $11
                                                  }
    | PropN Op PropParVals ')' MfRel PropN MfRel PropParVal
                                                  { MfClosedRange
                                                      (AppFun $1 $3)
                                                      $5
                                                      $6
                                                      $7
                                                      $8
                                                  }
    | PropN                                       { BooleanMf $1 }
    | PropVal MfRel PropN                         { OpenRangeFeatureFlipped $1 $2 $3 }
    | PropVal MfRel PropN MfRel PropParVal        { MfClosedRange $1 $2 $3 $4 $5 }
MfRel :: { MfRelation }
    : '<'                                         { MfLt }
    | '>'                                         { MfGt }
    | '<='                                        { MfLe }
    | '>='                                        { MfGe }
    | '='                                         { MfEq }
Url :: { Url }
    : 'url(' Str ')'                              { Url $2 }
    | 'uqUrl'                                     { UnquotedUrl (pack $1) }
PropVal :: { PropVal }
    : TypedNum                                    { IntVal $1 }
    | hash                                        { HexColor (HC (pack $1)) }
    | PropN                                       { propRef $1 }
    | Str                                         { StrVal $1 }
    | PropVal '/' Os PropVal                      { Div $1 $4 }
    | PropN Op PropParValsList ')'                { mkAppFun $1 $3 }
    | PropN Op ')'                                { AppConst $1 }
    | '.'                                         { DotVal }
    | Url                                         { UrlVal $1 }
    | Bool                                        { BoolVal $1 }
    | 'attr(' Os AttrName Os Maybe(AttrType) AttrDefVal Os ')'
                                                  { AttrFun $3 $5 $6 }
    | calcFns Op CalcExprList Os ')'              {% massageExpr CalcFun (CalcCe $1 $3) }
    | Op Os CalcExprList Os ')'                   {% massageExpr CalcFun (CalcCe NoFn $3) }
    | 'ratio'                                     { RatioVal $1 }
    | UnicodeRange                                { Vl.UnicodeRangeVal $1 }
    | alpha Op Ident Os '=' Os Unsigned Os ')'    { AlphaF $7 }
    | '[' Ident ']'                               { BracketVal $2 }
PropParVal :: { PropVal }
    : TypedNum                                    { IntVal $1 }
    | hash                                        { HexColor (HC (pack $1)) }
    | PropN                                       { propRef $1 }
    | Str                                         { StrVal $1 }
    | PropParVal '/' Os PropParVal                { Div $1 $4 }
    | PropN Op PropParValsList ')' Os             { mkAppFun $1 $3 }
    | PropN Op PropParValsList ')' Os CalcASP Os CalcExpr
                                                  {% massageExpr
                                                       CalcFun
                                                       (BinOpCe
                                                         (AppCe $1 (PropValsList $3))
                                                         $6
                                                         $8)
                                                  }
    | PropN Op ')' Os                             { AppConst $1 }
    | PropN Op ')' Os CalcASP Os CalcExpr         {% massageExpr CalcFun (BinOpCe (AppCe0 $1) $5 $7) }
    | '.'                                         { DotVal }
    | Url                                         { UrlVal $1 }
    | Bool                                        { BoolVal $1 }
    | 'attr(' Os AttrName Os Maybe(AttrType) AttrDefVal Os ')'
                                                  { AttrFun $3 $5 $6 }
    | calcFns Op CalcExprList Os ')' Os           {% massageExpr CalcFun (CalcCe $1 $3) }
    | calcFns Op CalcExprList Os ')' Os CalcASP Os CalcExpr
                                                  {% massageExpr CalcFun
                                                                 (BinOpCe (CalcCe $1 $3) $7 $9) }
    | Op Os CalcExprList Os ')' Os                {% massageExpr CalcFun (CalcCe NoFn $3) }
    | Op Os CalcExprList Os ')' Os CalcASP Os CalcExpr
                                                  {% massageExpr
                                                       CalcFun
                                                       (BinOpCe (CalcCe NoFn $3) $7 $9)
                                                  }
    | 'ratio'                                     { RatioVal $1 }
    | UnicodeRange                                { Vl.UnicodeRangeVal $1 }
    | alpha Op Ident Os '=' Os Unsigned Os ')'    { AlphaF $7 }
    | '[' Ident ']'                               { BracketVal $2 }

AttrType :: { AttrType }
    : TypeFun                                     { CssTypeAt $1 }
    | UnitType                                    { UnitAt $1 }
    | rawString                                   { RawString }
UnitType :: { PropValType }
    : ident                                       {% parseAsUnitType $1 }
    | '%'                                         { Percent }
AttrDefVal :: { Maybe PropVal }
    :                                             { Nothing }
    | ',' Os PropParVal                           { Just $3 }
CalcASP :: { CalcOp }
    : '+'                                         { PlusCe  }
    | '-'                                         { MinusCe }
    | '*'                                         { ProdCe  }
CalcOp :: { CalcOp }
    : CalcASP                                     { $1  }
    | '/'                                         { DivCe }
CalcExpr :: { CalcExpr }
   : Op CalcExprList ')' Os                      { CalcCe NoFn $2 }
   | '-' Os CalcExpr                             { CalcNeg $3 }
   | CalcExpr CalcOp Os CalcExpr                 { BinOpCe $1 $2 $4 }
   | CalcExpr CalcExpr                           {% recoverCalcBinOp $1 $2 }
   | PropN Op PropParValsList Os ')' Os          { AppCe $1 (PropValsList $3) }
   | PropN Op ')' Os                             { AppCe0 $1 }
   | PropN Os                                    { VarCe $1 }
   | TypedNum Os                                 { ValCe $1 }
   | calcFns Op CalcExprList ')' Os              { CalcCe $1 $3 }
CalcExprList :: { CalcExprList }
    : NonEmpty(',', CalcExpr)                     { CalcExprList $1 }
Unsigned :: { Unsigned }
    : TypedNum                                    {% fmap Unsigned (tryGet K $1) }
ContinueRule :: { CssRule }
    : SelectorList '{' Os CssRuleBody '}'         { CssRule $1 $4 }
CssRuleBody :: { [ CssRuleBodyItem ] }
    :                                             { [] }
    | Desc Os PropParValsList ';' OsCssRuleBody   { mkLeaf $1 $3 : $5 }
    | Desc Os PropParValsList                     { [ mkLeaf $1 $3 ] }
    | Desc Os ';' OsCssRuleBody                   { $4 }
    | Desc Os                                     { [] }
    | CssRule OsCssRuleBody                       { CssNestedRule $1 : $2 }
PropParValsList :: { NonEmpty PropVals }
    : NonEmpty(',', PropParVals)                  { $1 }
PropValsList :: { NonEmpty PropVals }
    : NonEmpty(',', PropVals)                     { $1 }
PropParVals :: { PropVals }
    : CssPropertyParVals Important                { PropVals $1 $2 }
PropVals :: { PropVals }
    : CssPropertyVals Important                   { PropVals $1 $2 }
Important :: { Maybe Important }
    : important                                   { Just Important }
    |                                             { Nothing }
CssPropertyParVals :: { NonEmpty PropVal }
    : PropParVal Os                               { $1 :| [] }
    | PropParVal Os CssPropertyParVals            { $1 <| $3 }
CssPropertyVals :: { NonEmpty PropVal }
    : PropVal Os                                  { $1 :| [] }
    | PropVal Os CssPropertyVals                  { $1 <| $3 }
SelectorList :: { NonEmpty Selector }
    : NonEmpty(',', Selector)                     { $1 }
Selector :: { Selector }
    : TagRelMb TagSel ZipTagRelAndTagSel          { Selector $1 $2 $3 }
    | TagRelMb TagSel ZipTagRelAndTagSel PsTgSel  { PeSelector $1 $2 $3 $4 }
    | PsTgSel                                     { PeSelectorOnly $1 }
PsTgSel :: { PseudeTagSelector }
    : CompositePe TagClasses                      { PseudeTagSelector $1 $2 }
CompositePe :: { CompositePe }
    : pseude                                      { AtomicPe $1 }
    | highlight Op IdKwd ')'                      { Highlight (Embraced $3) }
    | part Op SslNeOfIdents ')'                   { Part (Embraced $3) }
    | picker Op IdKwd Os ')'                      { Picker (Embraced $3) }
    | scrollButton Op TagName Os ')'              { ScrollButton (Embraced $3) }
    | slotted ESL                                 { Slotted (Embraced $2) }
    | viewTransitionGroup ESL                     { ViewTransitionGroup (Embraced $2) }
    | viewTransitionImagePair ESL                 { ViewTransitionImagePair (Embraced $2) }
    | viewTransitionNew ESL                       { ViewTransitionNew (Embraced $2) }
    | viewTransitionOld ESL                       { ViewTransitionOld (Embraced $2) }
TagRelMb :: { Maybe TagRelation }
    : Maybe(TagRelation)                          { $1 }
TagSel :: { TagSelector }
    : Ident '|' TagName TagClasses                { TagSelector (R.Namespace $1) $3 $4 }
    | Ident TagClasses                            { TagSelector NoBar (TagName $1) $2 }
    | '&' TagClasses                              { TagSelector NoBar AmpersandTag $2 }
    | '*' '|' TagName TagClasses                  { TagSelector AsteriskNs $3 $4  }
    | '*' TagClasses                              { TagSelector NoBar AsteriskTag $2 }
    | '|' TagName TagClasses                      { TagSelector NoNs $2 $3  }
    | TagClasses                                  { TagSelector NoBar NoTag $1 }
TagName :: { TagName }
    :                                             { NoTag }
    | '&'                                         { AmpersandTag }
    | '*'                                         { AsteriskTag }
    | Ident                                       { TagName $1 }
Hash :: { TagSubSelector }
    : hash                                        { Hash (R.Ident (pack $1)) }
TagClasses :: { [ TagSubSelector ] }
    : List(TagClass)                              { $1 }
TagClass :: { TagSubSelector }
    : Class                                       { AtomicClass $1 }
    | pseudc                                      { AtomicPseudoClass $1 }
    | pseudc ESL                                  {% mkUnknownPseudoF $1 $2 }
    | ':not' '(' SL                               { NotClass $3 }
    | ':lang(' Str ')'                            { Lang (Language $2) }
    | activeViewTransitionType Op CslOfIdents Os ')'
                                                  { ActiveViewTransitionType (Embraced $3) }
    | ':dir' Op IdKwd Os ')'                      { Dir (Embraced $3) }
    | heading Op CslOfInts Os ')'                 { Heading (Embraced $3) }
    | heading                                     { AtomicPseudoClass P.Heading }
    | host ESL                                    { Host (Embraced $2) }
    | host                                        { AtomicPseudoClass P.Host }
    | ':state(' Os IdKwd Os ')'                   { State (Embraced $3) }
    | ':global' Op SL                             { Global $3 }
    | ':where' Op SL                              { Where $3 }
    | ':is' Op SL                                 { Is $3 }
    | ':has' Op SL                                { Has $3 }
    | nthChild Op NthFormula OfTagSel Os ')'      { NthChild $3 $4 }
    | nthLastChild Op NthFormula OfTagSel Os ')'  { NthLastChild $3 $4 }
    | nthOfType Op NthFormula ')'                 { NthOfType $3 }
    | nthLastOfType Op NthFormula ')'             { NthLastOfType $3 }
    | '[' Attr ']'                                { $2 }
    | Hash                                        { $1 }
NthFormula :: { NthFormula }
    : CalcExpr                                    {% massageExpr NthExpr $1 }
    | odd Os                                      { NthEven False }
    | even Os                                     { NthEven True }
OfTagSel :: { Maybe TagSelector }
    :                                             { Nothing }
    | of Os TagSel                                { Just $3 }
CslOfIdents :: { CslNe R.Ident }
    : NonEmpty(',', IdKwd)                        { CslNe $1 }
SslNeOfIdents :: { SslNe R.Ident }
    : NonEmpty(' ', IdKwd)                        { SslNe $1 }
CslOfInts :: { CslNe Unsigned }
    : NonEmpty(Embraced(Os, ',', Os), Unsigned)   { CslNe $1 }
ZipTagRelAndTagSel :: { [ (TagRelation, TagSelector) ] }
    :                                             { [] }
    | TagRelation TagSel ZipTagRelAndTagSel       { ($1, $2) : $3 }
TagRelation :: { TagRelation }
    : ' ' '+' Os                                  { NextSibling }
    | ' ' '>' Os                                  { Child }
    | ' ' '~' Os                                  { GeneralSibling }
    | ' ' Os                                      { Descendant }
    | '+' Os                                      { NextSibling }
    | '>' Os                                      { Child }
    | '~' Os                                      { GeneralSibling }
Ocb : '{'                                         { () }
Op  :: { () }
    : '(' Os                                      { () }
Os  :                                             { () }
    | ' '                                         { () }
AtrPat :: { Maybe AtrPat }
    : Str AtrPatCase                              { Just (StrAtrPat $1 $2) }
    | attrPat AtrPatCase                          { Just (IdtAtrPat (R.Ident $1) $2) }
    |                                             { Nothing }
AtrPatCase :: { Maybe CaseSensetivity }
    :                                             { Nothing }
    | Os attrPat                                  {% fmap Just (atrCaseSensetivity (R.Ident $2)) }
AttrName :: { AttrName }
    : IdKwd                                       { AttrName NoBar $1 }
    | IdKwd '|' IdKwd                             { AttrName (R.Namespace $1) $3 }
    | '|' IdKwd                                   { AttrName NoNs $2 }
    | '*' '|' IdKwd                               { AttrName AsteriskNs $3 }
Attr :: { TagSubSelector }
    : AttrName AttrOp AtrPat                      { Attr $1 $2 $3 }
    | AttrName                                    { HasAttr $1 }
AttrOp ::  { AttrOp }
    : '='                                         { Exact }
    | '~='                                        { Include }
    | '|='                                        { DashMatch }
    | '^='                                        { PrefixMatch }
    | '$='                                        { SuffixMatch }
    | '*='                                        { SubstringMatch }
IdKwd :: { R.Ident }
    : Ident                                       { $1 }
    | MediaKeywordAsIdent                         { $1 }
    | 'to'                                        { R.Ident "to" }
    | from                                        { R.Ident "from" }
    | returns                                     { R.Ident "returns" }
    | AtId                                        { $1 }
AtId :: { R.Ident }
    : charset                                     { R.Ident "charset" }
    | colorProfile                                { R.Ident "color-profile" }
    | container                                   { R.Ident "container" }
    | counterStyle                                { R.Ident "counter-style" }
    | fontFace                                    { R.Ident "font-face" }
    | fontFeatureValues                           { R.Ident "font-feature-values" }
    | fontPaletteValues                           { R.Ident "font-palette-values" }
    | function                                    { R.Ident "function" }
    | import                                      { R.Ident "import" }
    | keyframes                                   { R.Ident "keyframes" }
    | layer                                       { R.Ident "layer" }
    | media                                       { R.Ident "media" }
    | mediaType                                   { R.Ident (toStrict (toCssText $1)) }
    | namespace                                   { R.Ident "namespace" }
    | page                                        { R.Ident "page" }
    | alpha                                       { R.Ident "alpha" }
    | pageMargin                                  { R.Ident "page-margin" }
    | positionTry                                 { R.Ident "position-try" }
    | property                                    { R.Ident "property" }
    | scope                                       { R.Ident "scope" }
    | startingStyle                               { R.Ident "starting-style" }
    | supports                                    { R.Ident "supports" }
    | viewTransition                              { R.Ident "view-transition" }
MediaKeywordAsIdent
    : or                                          { R.Ident "or" }
    | and                                         { R.Ident "and" }
    | only                                        { R.Ident "only" }
Ident :: { R.Ident }
    : ident                                       { R.Ident $1 }
Class :: { R.Ident }
    : class                                       { R.Ident (pack $1) }
IdTxt :: { Text }
    : ident                                       { $1 }
Str :: { Text }
    : string                                      { pack $1 }
Var :: { R.Var }
    : var                                         { R.Var (R.Ident (pack $1)) }
Embraced(o, p, c)
    : o p c                                       { $2 }
Clp : ')'                                         { $1 }
P(p): Embraced(Op, p, Clp)                        { $1 }
SepList(sep, elt)
    :                                             { [] }
    | elt sep List(elt)                           { $1 : $3 }
List(elt)
    :                                             { [] }
    | elt List(elt)                               { $1 : $2 }
Maybe(elt)
    :                                             { Nothing }
    | elt                                         { Just $1 }
NonEmpty(sep, elt)
    : elt                                         { $1 :| [] }
    | elt sep NonEmpty(sep, elt)                  { $1 <| $3 }
{
happyError :: [TokenLoc] -> P a
happyError (~(TokenLoc t s ~(Just (AlexPn _ l c))):_) =
  failP $ "Can not parse CSS: unpexected token \"" <>
    s <> "\" at (" <> show l <> ", " <> show c <> ")"
happyError _ = failP "Unexpected end of a CSS string"
}
