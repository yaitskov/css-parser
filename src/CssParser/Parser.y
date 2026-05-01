-- vim:ft=haskell
{
module CssParser.Parser where

import CssParser.At
import CssParser.At.Container
import CssParser.At.FontFace
import CssParser.At.FontFeatureValues
import CssParser.At.FontPaletteValues
import CssParser.At.Import
import CssParser.At.Keyframe
import CssParser.At.Layer
import CssParser.At.MediaQuery
import CssParser.At.Namespace
import CssParser.At.Page
import CssParser.At.Supports hiding (FeatureQuery)
import CssParser.Norm
import CssParser.Rule.Pseudo qualified as P
import CssParser.Rule.Pseudo hiding (Left, Right, ViewTransition, Heading, Host)
import CssParser.Rule.Value hiding (Mm, Cm, Dpi, Em, Deg, Grad, Rad, Turn, Rem)
import CssParser.Rule.Value qualified as Vl
import CssParser.Fun
import CssParser.File
import CssParser.FixRule
import CssParser.Ident hiding (Ident, Namespace, Var)
import CssParser.Ident qualified as R
import CssParser.Lexer qualified as L
import CssParser.Lexer
  ( AlexPosn(AlexPn), TokenLoc(TokenLoc)
  , Token
    ( TIncludes, TEqual, TDashMatch, TPrefixMatch, TSuffixMatch, TSubstringMatch, Ident
    , Integer, Comma, Plus, Tilde, Dot, Asterisk, Space, BOpen, BClose, PseudoFunction
    , PseudoElementT, TN, TNth, TPM, TInt, TNot, TLang, Decimal, String, THash
    , COpen, CClose, Colon, Semicolon, Var, Pipe, AtomicPseudoClassT, Ampersand
    , CharsetT, ImportT, MediaT, LayerT, NamespaceT, CounterStyleT, PropertyT
    , NotT, OrT, AndT, OnlyT
    , TOpen, TClose
    , Greater, Less, LessEqual, GreaterEqual
    , RatioT, Percents, Pixels
    , UrlT, TWhere, THas, TIs, PageT, PageMarginT
    , KeyframesT, ColorProfileT, FontFaceT, SrcPropT, UnicodeRangeT, UnicodeRangeVal
    , FontFeatureValuesT, AtT, FontPaletteValuesT, ContainerT, DivT, PositionTryT
    , StartingStyleT, ViewTransitionT, ScopeT, ToT, SupportsT, SelectorFunT
    , TActiveViewTransitionType, TDir, THeading, THost, TState
    )
  )
import CssParser.MonoPair
import CssParser.Parser.Monad
import CssParser.Prelude
  ( mapMaybe, prependList, NonEmpty((:|)), (<|), leftToMaybe, rightToMaybe, These(..)
  )
import CssParser.Rule
import CssParser.Show
import Data.Text (Text, pack)

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
    'charset'   { TokenLoc CharsetT _ _ }
    unRange     { TokenLoc UnicodeRangeT _ _ }
    '@'         { TokenLoc AtT _ _ }
    supports    { TokenLoc SupportsT _ _ }
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
    scope       { TokenLoc ScopeT _ _ }
    container   { TokenLoc ContainerT _ _ }
    property    { TokenLoc PropertyT _ _ }
    colorProf   { TokenLoc ColorProfileT _ _ }
    namespace   { TokenLoc NamespaceT _ _ }
    keyframes   { TokenLoc KeyframesT _ _ }
    counterStyle
                { TokenLoc CounterStyleT _ _ }
    fontFace    { TokenLoc FontFaceT _ _ }
    srcProp     { TokenLoc SrcPropT _ _ }
    'import'    { TokenLoc ImportT _ _ }
    'layer'     { TokenLoc LayerT _ _ }
    'page'      { TokenLoc PageT _ _ }

    pageMargin  { TokenLoc (PageMarginT $$) _ _ }

--     pseudoPage  { TokenLoc (PseudoPageT $$) _ _ }
    'media'     { TokenLoc MediaT _ _ }
-- media start
    'only'      { TokenLoc OnlyT _ _ }
    'not'       { TokenLoc NotT _ _ }
    'or'        { TokenLoc OrT _ _ }
    'and'       { TokenLoc AndT _ _ }
-- media end
    'url('      { TokenLoc UrlT _ _ }
    'selector(' { TokenLoc SelectorFunT _ _ }
    '^='        { TokenLoc TPrefixMatch _ _ }
    '$='        { TokenLoc TSuffixMatch _ _ }
    '*='        { TokenLoc TSubstringMatch _ _ }
    '|='        { TokenLoc TDashMatch _ _ }
    '~='        { TokenLoc TIncludes _ _ }
    unRangeVal  { TokenLoc (UnicodeRangeVal $$) _ _ }
    ident       { TokenLoc (Ident $$) _ _ }
    string      { TokenLoc (String $$) _ _ }
    hash        { TokenLoc (THash $$) _ _ }
    pseude      { TokenLoc (PseudoElementT $$) _ _ }
    pseudc      { TokenLoc (AtomicPseudoClassT $$) _ _ }
    pseudf      { TokenLoc (PseudoFunction $$) _ _ }
    pm          { TokenLoc (TPM $$) _ _ }
    'n'         { TokenLoc TN _ _ }
    int         { TokenLoc (TInt $$) _ _ }
    integer     { TokenLoc (Integer $$) _ _ }
    'ratio'     { TokenLoc (RatioT $$) _ _ }
    turn        { TokenLoc (L.Turn $$) _ _ }
    grad        { TokenLoc (L.Grad $$) _ _ }
    deg         { TokenLoc (L.Deg $$) _ _ }
    rad         { TokenLoc (L.Rad $$) _ _ }
    mm          { TokenLoc (L.Mm $$) _ _ }
    em          { TokenLoc (L.Em $$) _ _ }
    cm          { TokenLoc (L.Cm $$) _ _ }
    vw          { TokenLoc (L.Vw $$) _ _ }
    vh          { TokenLoc (L.Vh $$) _ _ }
    dpi         { TokenLoc (L.Dpi $$) _ _ }
    rem         { TokenLoc (L.Rem $$) _ _ }
    percents    { TokenLoc (Percents $$) _ _ }
    px          { TokenLoc (Pixels $$) _ _ }
    var         { TokenLoc (Var $$) _ _ }
    nth         { TokenLoc (TNth $$) _ _ }
    ':not'      { TokenLoc TNot _ _ }
    where       { TokenLoc TWhere _ _ }
    is          { TokenLoc TIs _ _ }
    has         { TokenLoc THas _ _ }
    'lang('     { TokenLoc TLang _ _ }
    dir         { TokenLoc TDir _ _ }
    heading     { TokenLoc THeading _ _ }
    host        { TokenLoc THost _ _ }
    state       { TokenLoc TState _ _ }
    activeViewTransitionType
                { TokenLoc TActiveViewTransitionType _ _ }
    '('         { TokenLoc TOpen _ _ }
    ')'         { TokenLoc TClose _ _ }
    '/'         { TokenLoc DivT _ _ }

%right 'not'
%left 'or' 'and'
%%

CssFile
    : Charset Headers Namespaces CssFileBody      { CssFile
                                                      $1
                                                      (mapMaybe rightToMaybe $2)
                                                      $3
                                                      (mapMaybe leftToMaybe $2 <> $4)
                                                  }
Charset
    :                                             { Nothing }
    | 'charset' Str ';'                           { Just (Charset $2) }
Headers :: { [ Either CssRule FileHeader ] }
    :                                             { [] }
    | Header Headers                              { $1 : $2 }
Header :: { Either CssRule FileHeader }
    : 'import' Source ';'                         { Right (HeaderImport (Import $2)) }
    | 'layer' LayerNames ';'                      { Right (HeaderLayers (LayerStmt $2)) }
    | 'layer' IdKwd '{' CssRuleBody '}'           { Left (LayerBlock (Just (LayerName $2)) $4) }
    | 'layer' '{' CssRuleBody '}'                 { Left (LayerBlock Nothing $3) }
Namespaces
    :                                             { [] }
    | Namespace ';' Namespaces                    { $1 : $3 }
Namespace
    : namespace IdKwdMb Os Source                 { Namespace $2 $4 }
Source
    : 'url(' Str ')'                              { UrlSource (Url $2) }
    | Str                                         { StrSource $1 }
LayerNames :: { NonEmpty LayerName }
    : IdKwd                                       { LayerName $1 :| [] }
    | IdKwd ',' LayerNames                        { LayerName $1 <| $3 }
CssFileBody
    :                                             { [] }
    | CssRule CssFileBody                         { $1 : $2 }
CssRule :: { CssRule }
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }
    | 'media' '{' CssRuleBody '}'                 { MediaRule (MediaQueryList []) $3 }
    | 'media' MediaQueryList '{' CssRuleBody '}'  { MediaRule (MediaQueryList $2) $4 }
    | 'layer' IdKwdMb '{' CssRuleBody '}'         { LayerBlock (fmap LayerName $2) $4 }
    | 'page' '{' CssRuleBody '}'                  { Page (PageSelectorList []) $3 }
    | 'page' PageSelectorList '{' CssRuleBody '}' { Page (PageSelectorList $2) $4 }
    | pageMargin '{' CssRuleBody '}'              { PageMarginBlock $1 $3 }
    | counterStyle IdKwd '{' CssRuleBody '}'      { CounterStyle $2 $4 }
    | property Var '{' CssRuleBody '}'            { Property (R.Var $2) $4 }
    | keyframes IdKwd Ocb KeyframeList '}'        { Keyframes (KeyframeSet (KeyframeSetName $2) $4) }
    | colorProf Os Var Ocb ColorPropEntries '}'   { ColorProfile (VarProp (R.Var $3)) $5 }
    | colorProf Os IdKwd Ocb ColorPropEntries '}' { ColorProfile (PropertyName $3) $5 }
    | fontFace Os Ocb FontFacePropEntries '}'     {% fmap FontFaceBlock (fromEitherM failP (mkFontFace $4)) }
    | fontFeatureValues ' ' StrEitherIds Os Ocb FontFeatureValBlocks '}'
                                                  { FontFeatureValuesBlock
                                                      (FontFeatureValues
                                                        $3
                                                        (mapMaybe leftToMaybe $6)
                                                        (mapMaybe rightToMaybe $6))
                                                  }
    | fontPaletteValues ' ' Var Os Ocb PropEntries '}'
                                                  { FontPaletteValuesBlock (FontPaletteValues (R.Var $3) $6) }
    | container Os ContainerQueryMap ERB          { Container (ContainerQueryMap $3) $4 }
    | positionTry Os Var Ocb PropEntries '}'      { PositionTry (R.Var $3) $5 }
    | startingStyle Os ERB                        { StartingStyle $3 }
    | viewTransition Os ERB                       { ViewTransition $3 }
    | scope Os SelectorPair ERB                   { ScopeBlock $3 $4 }
    | supports Os FeatureQuery ERB                { Supports (normalize $3) $4 }
    | '@' Ident Os CommaSeparatedList Os ERB      { UnknownGramma $2 (Just (CommaSeparatedList $4)) $6 }
    | '@' Ident Os ERB                            { UnknownGramma $2 Nothing $4 }
FeatureQuery :: { FeatureQuery }
    : Op MediaFeature ')'                         { FqMediaFeature $2 }
    | Op FeatureQuery ')'                         { FqParen $2 }
    | FeatureQuery Os BOP Os FeatureQuery         { FqBop $3 $1 $5 }
    | 'not' FeatureQuery                          { FqNot $2 }
    | 'not' FeatureQuery Os BOP Os FeatureQuery   { FqBop $4 (FqNot $2) $6 }
    | 'selector(' SelectorList ')'                { FqApp (FqSelectorFun $2) }
    | Ident Op PropVals ')'                       { FqApp (FqSomeFun $1 $3) }
ESL :: { SelectorList }
    : '(' SelectorList ')'                        { $2 }
SelectorPair :: { MonoPair SelectorList }
    :                                             { EmptyPair }
    | ESL Os                                      { HalfPair $1 }
    | ESL Os 'to' Os ESL Os                       { FullPair $1 $5 }
ERB :: { [CssRuleBodyItem] } -- Embraced Rule Body
    : '{' CssRuleBody '}'                         { $2 }
ContainerQueryMap :: { NonEmpty (These R.Ident ContainerQuery) }
    : IdContainerQuery                            { $1 :| [] }
    | IdContainerQuery ',' ContainerQueryMap      { $1 <| $3 }
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
CQ :: { ContainerQuery }
    : Ident Os Op CQ ')'                          { CqFeature (AsIs (CqApp $1 $4)) }
    | Ident Os Op CQ ')' Os BOP CQ                { CqBin
                                                      $7
                                                      (AsIs (CqApp $1 $4))
                                                      $8
                                                  }
    | Ident ':' PropVals                          { CqFeature (AsIs (CqOpFeature (PlainMf (PropertyName $1) $3))) }
    | Var   ':' PropVals                          { CqFeature (AsIs (CqOpFeature (PlainMf (VarProp (R.Var $1)) $3))) }
    | 'not' Op MediaFeature ')' Os BOP CQ         { CqBin $6 (Not (CqOpFeature $3)) $7 }
    | 'not' Op MediaFeature ')'                   { CqFeature (Not (CqOpFeature $3)) }
    | 'not' Os Ident Os Op CQ ')'                 { CqFeature (Not (CqApp $3 $6)) }
    | 'not' Os Ident Os Op CQ ')' Os BOP CQ       { CqBin $9 (Not (CqApp $3 $6)) $10 }
    | Op MediaFeature ')' Os BOP CQ               { CqBin $5 (AsIs (CqOpFeature $2)) $6 }
    | Op MediaFeature ')'                         { CqFeature (AsIs (CqOpFeature $2)) }
BOP :: { AndOr }
    : 'and'                                       { And }
    | 'or'                                        { Or }
FontFeatureValBlocks :: { [ Either PropEntry FontFeatureValuesSubBlock ] }
    :                                             { [] }
    | FontFeatureValBlock FontFeatureValBlocks    { Right $1 : $2 }
    | PropEntry FontFeatureValBlocks              { Left $1 : $2 }
FontFeatureValBlock
    : '@' Ident Os Ocb PropEntries '}'            { FontFeatureValuesSubBlock $2 $5 }
StrEitherIds :: { Either LiteralString IdentList }
    : Str                                         { Left (LiteralString $1) }
    | IdentList                                   { Right (IdentList $1) }
IdentList :: { NonEmpty R.Ident }
    : Ident                                       { $1 :| [] }
    | Ident ' ' IdentList                         { $1 <| $3 }
ColorPropEntries
    : srcProp ':' PropVals ';' ColorPropEntries   { PropEntry (PropertyName "src") $3 : $5 }
    | PropEntry ColorPropEntries                  { $1 : $2 }
    |                                             { [] }
CommaSeparatedList :: { NonEmpty PropVals }
    : CssPropertyVals                             { (PropVals $1) :| [] }
    | CssPropertyVals ',' CommaSeparatedList      { (PropVals $1) <| $3 }
FontFacePropEntries :: { NonEmpty (Either SrcVal FontFacePropEntry) }
    : FontFaceProp                                { $1 :| [] }
    | FontFaceProp FontFacePropEntries            { $1 <| $2 }
FontFaceProp
    : srcProp ':' CommaSeparatedList ';'          { Left (CommaSeparatedList $3) }
    | unRange ':' UnicodeRangeList ';'            { Right (UnicodeRangePropEntry $3) }
    | PropEntry                                   { Right (FontFaceCommonEntry $1) }
UnicodeRangeList
    : unRangeVal                                  { UnicodeRange (pack $1) :| [] }
    | unRangeVal ',' UnicodeRangeList             { UnicodeRange (pack $1) <| $3 }
KeyframeList
    :                                             { [] }
    | Keyframe KeyframeList                       { $1 : $2 }
Keyframe
    : KeyframeAdr Ocb PropEntries '}'             { Keyframe $1 $3 }
KeyframeAdr
    : IdKwd                                       { KeyframeLabel $1 }
    | percents                                    { KeyframePercentAdr (Unsigned $1) }
PropEntries :: { [PropEntry] }
    :                                             { [] }
    | PropEntry PropEntries                       { $1 : $2 }
PropertyName :: { PropertyName }
    : IdKwd                                       { PropertyName $1 }
    | Var                                         { VarProp (R.Var $1) }
PropEntry :: { PropEntry }
    : PropertyName ':' PropVals ';'               { PropEntry $1 $3 }
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

MediaQuery :: { MediaQuery }
    : 'not' Os Op MediaFeature ')'                { MediaQueryConditionOnly (MediaFeature (Not $4)) }
    | MtModifier MediaType Os 'and' Os MediaCondition
                                                  { MediaQueryWithMt
                                                      $1
                                                      $2
                                                      (Just $6)
                                                  }
    | MtModifier MediaType                        { MediaQueryWithMt $1 $2 Nothing }
    | MediaCondition                              { MediaQueryConditionOnly $1 }
MediaType
    : Ident                                       { MediaType $1 }
MtModifier :: { Maybe MtModifier }
    :                                             { Nothing }
    | 'not'                                       { Just MtNot }
    | 'only'                                      { Just MtOnly }
MediaCondition :: { MediaBoolExpr }
    : 'not' Op MediaFeature ')' Os BOP MediaCondition
                                                  { MediaBin $6 (Not $3) $7 }
    | 'not' Op MediaFeature ')'                   { MediaFeature (Not $3) }
    | Op MediaFeature ')' Os BOP MediaCondition   { MediaBin $5 (AsIs $2) $6 }
    | Op MediaFeature ')'                         { MediaFeature (AsIs $2) }
MediaFeature :: { MediaFeature }
    : PropertyName ':' PropVals                   { PlainMf $1 $3 }
    | PropertyName MfRel PropVal                  { OpenRangeFeature $1 $2 $3 }
    | PropertyName MfRel PropertyName MfRel PropVal
                                                  { MfClosedRange (propRef $1) $2 $3 $4 $5 }
    | PropertyName Op PropVals ')' MfRel PropertyName
                                                  { OpenRangeFeatureFlipped
                                                      (AppFun $1 $3)
                                                      $5
                                                      $6
                                                  }
    | PropertyName Op PropVals ')' MfRel PropertyName MfRel PropVal
                                                  { MfClosedRange
                                                      (AppFun $1 $3)
                                                      $5
                                                      $6
                                                      $7
                                                      $8
                                                  }
    | PropertyName                                { BooleanMf $1 }
    | PropVal MfRel PropertyName                  { OpenRangeFeatureFlipped $1 $2 $3 }
    | PropVal MfRel PropertyName MfRel PropVal    { MfClosedRange $1 $2 $3 $4 $5 }
MfRel :: { MfRelation }
    : '<'                                         { MfLt }
    | '>'                                         { MfGt }
    | '<='                                        { MfLe }
    | '>='                                        { MfGe }
    | '='                                         { MfEq }
PropVal :: { PropVal }
    : mm                                          { IntVal (Unsigned $1) Vl.Mm }
    | deg                                         { IntVal (Unsigned $1) Vl.Deg }
    | rad                                         { IntVal (Unsigned $1) Vl.Rad }
    | grad                                        { IntVal (Unsigned $1) Vl.Grad }
    | turn                                        { IntVal (Unsigned $1) Vl.Turn }
    | em                                          { IntVal (Unsigned $1) Vl.Em }
    | cm                                          { IntVal (Unsigned $1) Vl.Cm }
    | vw                                          { IntVal (Unsigned $1) Vl.Vw }
    | vh                                          { IntVal (Unsigned $1) Vl.Vh }
    | dpi                                         { IntVal (Unsigned $1) Vl.Dpi }
    | rem                                         { IntVal (Unsigned $1) Vl.Rem }
    | percents                                    { IntVal (Unsigned $1) Vl.Percent }
    | px                                          { IntVal (Unsigned $1) Vl.Px }
    | Unsigned                                    { IntVal $1 Vl.K }
    | 'ratio'                                     { RatioVal $1 }
    | PropertyName Os                             { propRef $1 }
    | PropertyName Os '/' Os PropertyName         { Div $1 $5 }
    | PropertyName Op PropVals ')'                { AppFun $1 $3 }
    | Str                                         { StrVal $1 }
    | 'url(' Str ')'                              { UrlVal (Url $2) }
    | hash                                        { HexColor (HC (pack $1)) }
Unsigned
    : integer                                     { Unsigned $1 }
ContinueRule :: { CssRule }
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }
CssRuleBody :: { [ CssRuleBodyItem ] }
    :                                             { [] }
    | PropertyName ':' PropVals ';' CssRuleBody   { CssLeafRule $1 $3 : $5 }
    | PropertyName ':' PropVals                   { [ CssLeafRule $1 $3 ] }
    | IdKwd '{' CssRuleBody '}' CssRuleBody       { CssNestedRule (tagNameRule $1 $3) : $5 }
    | IdKwd '>' Os ContinueRule CssRuleBody       {% fmap ((: $5) . CssNestedRule) (prependIdentToRule $1 Child $4) }
    | IdKwd ' ' ContinueRule CssRuleBody          {% fmap ((: $4) . CssNestedRule) (prependIdentToRule $1 Descendant $3) }
    | IdKwd '|' ContinueRule CssRuleBody          { CssNestedRule (updateTopTagSelector (setTsNs $1) $3) : $4 }
    | IdKwd '+' Os ContinueRule CssRuleBody       {% fmap ((: $5) . CssNestedRule) (prependIdentToRule $1 NextSibling $4) }
    | IdKwd '~' Os ContinueRule CssRuleBody       {% fmap ((: $5) . CssNestedRule) (prependIdentToRule $1 GeneralSibling $4) }
    | IdKwd '[' Attr ERB CssRuleBody              { CssNestedRule (tagAndAttrRule $1 $3 $4) : $5 }
    | IdKwd '[' Attr ContinueRule CssRuleBody     { upsertHeadTagSelector (setTag $1 . addAttr $3) $4 $5 }
    | IdKwd ',' ContinueRule CssRuleBody          { CssNestedRule (prependSelectorToRule $1 $3) : $4 }
    | IdKwd '.' ContinueRule CssRuleBody          { CssNestedRule (tagNameIsClass $1 $3) : $4 }
    | IdKwd Hash ContinueRule CssRuleBody         { upsertHeadTagSelector (setTag $1 . setHash $2) $3 $4 }
    | IdKwd Hash ERB CssRuleBody                  { newRule (setHash $2 . setTag $1) $3 $4 }
    | IdKwd pseudc ContinueRule CssRuleBody       { upsertHeadTagSelector
                                                      (setTag $1 . addClass (AtomicPseudoClass $2)) $3 $4 }
    | IdKwd pseudc ERB CssRuleBody                { newRule (addClass (AtomicPseudoClass $2) . setTag $1) $3 $4 }
    | IdKwd ':not' ESL ContinueRule CssRuleBody   { upsertHeadTagSelector (setTag $1 . addClass (NotClass $3)) $4 $5 }
    | IdKwd ':not' ESL ERB CssRuleBody            { newRule (setTag $1 . addClass (NotClass $3)) $4 $5 }
    | IdKwd where ESL ContinueRule CssRuleBody    { upsertHeadTagSelector (setTag $1 . addClass (Where $3)) $4 $5 }
    | IdKwd where ESL ERB CssRuleBody             { newRule (setTag $1 . addClass (Where $3)) $4 $5 }
    | IdKwd is    ESL ContinueRule CssRuleBody    { upsertHeadTagSelector (setTag $1 . addClass (Is $3)) $4 $5 }
    | IdKwd is    ESL ERB          CssRuleBody    { newRule (setTag $1 . addClass (Is $3)) $4 $5 }
    | IdKwd has   ESL ContinueRule CssRuleBody    { upsertHeadTagSelector (setTag $1 . addClass (Has $3)) $4 $5 }
    | IdKwd has   ESL ERB          CssRuleBody    { newRule (setTag $1 . addClass (Has $3)) $4 $5 }

    | IdKwd 'lang(' Str ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector (setTag $1 . addClass (Lang (Language $3))) $5 $6 }
    | IdKwd 'lang(' Str ')' ERB CssRuleBody       { newRule (setTag $1 . addClass (Lang (Language $3))) $5 $6 }
    | IdKwd pseudf Os Nth ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector (setTag $1 . addClass (call $2 $4)) $5 $6 }
    | IdKwd pseudf Os Nth ERB CssRuleBody         { newRule (setTag $1 . addClass (call $2 $4)) $5 $6 }

    | IdKwd activeViewTransitionType Op CslOfIdents Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (ActiveViewTransitionType (Embraced $4)))
                                                      $7 $8}
    | IdKwd activeViewTransitionType Op CslOfIdents Os ')' ERB CssRuleBody
                                                  { newRule
                                                      ( setTag $1
                                                      . addClass (ActiveViewTransitionType (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd dir Op Ident Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (Dir (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd dir Op Ident Os ')' ERB CssRuleBody   { newRule
                                                      ( setTag $1
                                                      . addClass (Dir (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd heading CslOfInts Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (Heading (Embraced $3)))
                                                      $6 $7
                                                  }
    | IdKwd heading CslOfInts Os ')' ERB CssRuleBody
                                                  { newRule
                                                      ( setTag $1
                                                      . addClass (Heading (Embraced $3)))
                                                      $6 $7
                                                  }
    | IdKwd heading ContinueRule CssRuleBody      { upsertHeadTagSelector
                                                      (setTag $1 . addClass (AtomicPseudoClass P.Heading)) $3 $4 }
    | IdKwd heading ERB CssRuleBody               { newRule (addClass (AtomicPseudoClass P.Heading) . setTag $1) $3 $4 }
    | IdKwd host ESL ContinueRule CssRuleBody     { upsertHeadTagSelector
                                                      (setTag $1 . addClass (Host (Embraced $3)))
                                                      $4 $5
                                                  }
    | IdKwd host ESL ERB CssRuleBody              { newRule
                                                      ( setTag $1
                                                      . addClass (Host (Embraced $3)))
                                                      $4 $5
                                                  }
    | IdKwd host ContinueRule CssRuleBody         { upsertHeadTagSelector
                                                      (setTag $1 . addClass (AtomicPseudoClass P.Host)) $3 $4 }
    | IdKwd host ERB CssRuleBody                  { newRule (addClass (AtomicPseudoClass P.Host) . setTag $1) $3 $4 }

    | IdKwd state Op Ident Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (State (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd state Op Ident Os ')' ERB CssRuleBody { newRule
                                                      ( setTag $1
                                                      . addClass (State (Embraced $4)))
                                                      $7 $8
                                                  }

    | IdKwd pseude ERB CssRuleBody                { newPseude (setTag $1) $2 $3 $4 }
    | IdKwd pseude ',' ContinueRule CssRuleBody   { CssNestedRule (pushPeSelector (setTag $1) $2 $4) : $5 }
    | CssRule CssRuleBody                         { CssNestedRule $1 : $2 }
PropVals :: { PropVals }
    : CssPropertyVals                             { PropVals $1 }
CssPropertyVals :: { NonEmpty PropVal }
    : PropVal                                     { $1 :| [] }
    | PropVal ' ' CssPropertyVals                 { $1 <| $3 }
    | PropVal CssPropertyVals                     { $1 <| $2 }
SelectorList :: { NonEmpty Selector }
    : Selector                                    { $1 :| [] }
    | Selector ',' SelectorList                   { $1 <| $3 }
Selector :: { Selector }
    : TagRelMb TagSelector ZipTagRelAndTagSel         { Selector $1 $2 $3 }
    | TagRelMb TagSelector ZipTagRelAndTagSel pseude  { PeSelector $1 $2 $3 $4 }
    | pseude                                          { PeSelectorOnly $1 }
TagRelMb :: { Maybe TagRelation }
    : TagRelation                                     { Just $1 }
    |                                                 { Nothing }
TagSelector :: { TagSelector }
    : Ident '|' TagName TagAttrs TagId TagClasses     { TagSelector (R.Namespace $1) $3 $4 $5 $6 }
    | Ident TagAttrs TagId TagClasses                 { TagSelector NoBar (TagName $1) $2 $3 $4 }
    | '&' TagAttrs TagId TagClasses                   { TagSelector NoBar AmpersandTag $2 $3 $4 }
    | '*' '|' TagName TagAttrs TagId TagClasses       { TagSelector AsteriskNs $3 $4 $5 $6 }
    | '*' TagAttrs TagId TagClasses                   { TagSelector NoBar AsteriskTag $2 $3 $4 }
    | '|' TagName TagAttrs TagId TagClasses           { TagSelector NoNs $2 $3 $4 $5 }
    | TagAttrs TagId TagClasses                       { TagSelector NoBar NoTag $1 $2 $3 }
TagName :: { TagName }
    :                                                 { NoTag }
    | '&'                                             { AmpersandTag }
    | '*'                                             { AsteriskTag }
    | Ident                                           { TagName $1 }
TagAttrs :: { [Attr] }
    :                                                 { [] }
    | TagAttrsNe                                      { $1 }
TagAttrsNe :: { [Attr] }
    : AttrBox                                         { [ $1 ] }
    | AttrBox TagAttrs                                { $1 : $2 }
TagId :: { Maybe Hash }
    :                                                 { Nothing }
    | Hash                                            { Just $1 }
Hash :: { Hash }
    : hash                                            { Hash (pack $1) }
TagClasses :: { [ Class ] }
    :                                                 { [] }
    | TagClass TagClasses                             { $1 : $2 }
TagClass :: { Class }
    : '.' Ident                                       { AtomicClass $2 }
    | pseudc                                          { AtomicPseudoClass $1 }
    | ':not' ESL                                      { NotClass $2 }
    | 'lang(' Str ')'                                 { Lang (Language $2) }
    | activeViewTransitionType Op CslOfIdents Os ')'  { ActiveViewTransitionType (Embraced $3) }
    | dir Op Ident Os ')'                             { Dir (Embraced $3) }
    | heading CslOfInts Os ')'                        { Heading (Embraced $2) }
    | heading                                         { AtomicPseudoClass P.Heading }
    | host ESL                                        { Host (Embraced $2) }
    | host                                            { AtomicPseudoClass P.Host }
    | state Op Ident Os ')'                           { State (Embraced $3) }
    | where ESL                                       { Where $2 }
    | is ESL                                          { Is $2 }
    | has ESL                                         { Has $2 }
    | pseudf Os Nth                                   { call $1 $3 }
CslOfIdents :: { CslNe R.Ident }
    : CslOfIdentsNe                                   { CslNe $1 }
CslOfIdentsNe :: { NonEmpty R.Ident }
    : Ident                                           { $1 :| [] }
    | Ident Os ',' Os CslOfIdentsNe                   { $1 <| $5 }
CslOfInts :: { CslNe Integer }
    : CslOfIntsNe                                     { CslNe $1 }
CslOfIntsNe :: { NonEmpty Integer }
    : integer                                         { $1 :| [] }
    | integer Os ',' Os CslOfIntsNe                   { $1 <| $5 }

ZipTagRelAndTagSel :: { [ (TagRelation, TagSelector) ] }
    :                                                 { [] }
    | TagRelation TagSelector ZipTagRelAndTagSel      { ($1, $2) : $3 }
TagRelation :: { TagRelation }
    : '+' Os { NextSibling }
    | '>' Os { Child }
    | '~' Os { GeneralSibling }
    | ' ' Os { Descendant }
Nth
    : nth Os ')'                                      { $1 }
    | PMOpt IntOpt 'n' Os ')'                         { Nth (call $1 $2) 0 }
    | PMOpt IntOpt 'n' Os pm Os int Os ')'            { Nth (call $1 $2) (call $5 $7) }
    | PMOpt int Os ')'                                { Nth 0 (call $1 $2) }
PMOpt
    :                                                 { TpmIdF }
    | pm                                              { $1 }
IntOpt
    :                                                 { 1 }
    | int                                             { $1 }
-- Ccb : '}' Os                                          { () }
Ocb : '{'                                             { () }
Op  : '(' Os                                          { () }
Os  :                                                 { () }
    | ' '                                             { () }
AttrBox
    : '[' Attr                                        { $2 }
Attr
    : Ident ']'                                       { HasAttr (AttrName NoBar $1) }
    | Ident '|' Ident ']'                             { HasAttr (AttrName (R.Namespace $1) $3) }
    | Ident '|' Ident AttrOp IdTxt ']'
                                                      { Attr (AttrName (R.Namespace $1) $3) $4 $5 }
    | Ident '|' Ident AttrOp Str ']'                  { Attr (AttrName (R.Namespace $1) $3) $4 $5 }
    | Ident AttrOp IdTxt ']'                          { Attr (AttrName NoBar $1) $2 $3 }
    | Ident AttrOp Str ']'                            { Attr (AttrName NoBar $1) $2 $3 }
    | '|' Ident ']'                                   { HasAttr (AttrName NoNs $2) }
    | '|' Ident AttrOp IdTxt ']'                      { Attr (AttrName NoNs $2) $3 $4 }
    | '|' Ident AttrOp Str ']'                        { Attr (AttrName NoNs $2) $3 $4 }
    | '*' '|' Ident ']'                               { HasAttr (AttrName AsteriskNs $3) }
    | '*' '|' Ident AttrOp IdTxt ']'                  { Attr (AttrName AsteriskNs $3) $4 $5 }
    | '*' '|' Ident AttrOp Str ']'                    { Attr (AttrName AsteriskNs $3) $4 $5 }
AttrOp ::  { AttrOp }
    : '='                                             { Exact }
    | '~='                                            { Include }
    | '|='                                            { DashMatch }
    | '^='                                            { PrefixMatch }
    | '$='                                            { SuffixMatch }
    | '*='                                            { SubstringMatch }
IdKwd
    : Ident                                           { $1 }
    | MediaKeywordAsIdent                             { $1 }
MediaKeywordAsIdent
    : 'not'                                           { R.Ident "not" }
    | 'or'                                            { R.Ident "or" }
    | 'and'                                           { R.Ident "and" }
    | 'only'                                          { R.Ident "only" }
Ident
    : ident                                           { R.Ident (pack $1) }
IdTxt
    : ident                                           { pack $1 }
Str :: { Text }
    : string                                          { pack $1 }
Var :: { R.Ident }
    : var                                             { R.Ident (pack $1) }

{
happyError :: [TokenLoc] -> P a
happyError (~(TokenLoc t s ~(Just (AlexPn _ l c))):_) =
  failP $ "Can not parse CSS: unpexected token \"" <>
    s <> "\" at (" <> show l <> ", " <> show c <> ")"
happyError _ =
  failP "Unexpected end of a CSS string"
}
