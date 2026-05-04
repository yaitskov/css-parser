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
    , Comma, Plus, Tilde, Dot, Asterisk, Space, BOpen, BClose, PseudoFunction
    , PseudoElementT, TN, TNth, TPM, TInt, TNot, TLang, String, THash
    , COpen, CClose, Colon, Semicolon, Var, Pipe, AtomicPseudoClassT, Ampersand
    , CharsetT, ImportT, MediaT, LayerT, LayerAtT, NamespaceT, CounterStyleT, PropertyT
    , NotT, OrT, AndT, OnlyT
    , TOpen, TClose
    , Greater, Less, LessEqual, GreaterEqual
    , RatioT, ImportantT, MediaTypeT
    , UrlT, UnquotedUrlT, TWhere, THas, TIs, PageT, PageMarginT
    , KeyframesT, ColorProfileT, FontFaceT, SrcPropT, UnicodeRangeT, UnicodeRangeVal
    , FontFeatureValuesT, AtT, FontPaletteValuesT, ContainerT, DivT, PositionTryT
    , StartingStyleT, ViewTransitionT, ScopeT, ToT, SupportsT, SelectorFunT
    , TActiveViewTransitionType, TDir, THeading, THost, TState
    , THighlight, TPart, TPicker, TScrollButton, TSlotted, TViewTransitionGroup
    , TViewTransitionImagePair, TViewTransitionNew, TViewTransitionOld
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
    mediaType   { TokenLoc (MediaTypeT $$) _ _ }
    'charset'   { TokenLoc CharsetT _ _ }
    unRange     { TokenLoc UnicodeRangeT _ _ }
    '@'         { TokenLoc AtT _ _ }
    important   { TokenLoc ImportantT _ _ }
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
    layer       { TokenLoc LayerT _ _ }
    '@layer'    { TokenLoc LayerAtT _ _ }
    'page'      { TokenLoc PageT _ _ }

    pageMargin  { TokenLoc (PageMarginT $$) _ _ }

    'media'     { TokenLoc MediaT _ _ }

    'only'      { TokenLoc OnlyT _ _ }
    'not'       { TokenLoc NotT _ _ }
    'or'        { TokenLoc OrT _ _ }
    'and'       { TokenLoc AndT _ _ }

    'url('      { TokenLoc UrlT _ _ }
    'uqUrl'     { TokenLoc (UnquotedUrlT $$) _ _ }
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

    pseudc      { TokenLoc (AtomicPseudoClassT $$) _ _ }
    pseudf      { TokenLoc (PseudoFunction $$) _ _ }
    pm          { TokenLoc (TPM $$) _ _ }
    'n'         { TokenLoc TN _ _ }
    int         { TokenLoc (TInt $$) _ _ }

    'ratio'     { TokenLoc (RatioT $$) _ _ }
    cap         { TokenLoc (L.Cap $$) _ _ }
    ch          { TokenLoc (L.Ch $$) _ _ }
    cm          { TokenLoc (L.Cm $$) _ _ }
    cqb         { TokenLoc (L.Cqb $$) _ _ }
    cqh         { TokenLoc (L.Cqh $$) _ _ }
    cqi         { TokenLoc (L.Cqi $$) _ _ }
    cqmax       { TokenLoc (L.Cqmax $$) _ _ }
    cqmin       { TokenLoc (L.Cqmin $$) _ _ }
    cqw         { TokenLoc (L.Cqw $$) _ _ }
    deg         { TokenLoc (L.Deg $$) _ _ }
    dpi         { TokenLoc (L.Dpi $$) _ _ }
    dvb         { TokenLoc (L.Dvb $$) _ _ }
    dvh         { TokenLoc (L.Dvh $$) _ _ }
    dvi         { TokenLoc (L.Dvi $$) _ _ }
    dvmax       { TokenLoc (L.Dvmax $$) _ _ }
    dvmin       { TokenLoc (L.Dvmin $$) _ _ }
    em          { TokenLoc (L.Em $$) _ _ }
    ex          { TokenLoc (L.Ex $$) _ _ }
    grad        { TokenLoc (L.Grad $$) _ _ }
    ic          { TokenLoc (L.Ic $$) _ _ }
    in          { TokenLoc (L.In $$) _ _ }
    lh          { TokenLoc (L.Lh $$) _ _ }
    lvb         { TokenLoc (L.Lvb $$) _ _ }
    lvh         { TokenLoc (L.Lvh $$) _ _ }
    lvi         { TokenLoc (L.Lvi $$) _ _ }
    lvmax       { TokenLoc (L.Lvmax $$) _ _ }
    lvmin       { TokenLoc (L.Lvmin $$) _ _ }
    mm          { TokenLoc (L.Mm $$) _ _ }
    ms          { TokenLoc (L.Ms $$) _ _ }
    pc          { TokenLoc (L.Pc $$) _ _ }
    pt          { TokenLoc (L.Pt $$) _ _ }
    percent     { TokenLoc (L.Percents $$) _ _ }
    px          { TokenLoc (L.Px $$) _ _ }
    q           { TokenLoc (L.Q $$) _ _ }
    rad         { TokenLoc (L.Rad $$) _ _ }
    rcap        { TokenLoc (L.Rcap $$) _ _ }
    rch         { TokenLoc (L.Rch $$) _ _ }
    rem         { TokenLoc (L.Rem $$) _ _ }
    rex         { TokenLoc (L.Rex $$) _ _ }
    ric         { TokenLoc (L.Ric $$) _ _ }
    rlh         { TokenLoc (L.Rlh $$) _ _ }
    second      { TokenLoc (L.Second $$) _ _ }
    svb         { TokenLoc (L.Svb $$) _ _ }
    svh         { TokenLoc (L.Svh $$) _ _ }
    svi         { TokenLoc (L.Svi $$) _ _ }
    svmax       { TokenLoc (L.Svmax $$) _ _ }
    svmin       { TokenLoc (L.Svmin $$) _ _ }
    turn        { TokenLoc (L.Turn $$) _ _ }
    vb          { TokenLoc (L.Vb $$) _ _ }
    vh          { TokenLoc (L.Vh $$) _ _ }
    vi          { TokenLoc (L.Vi $$) _ _ }
    vmax        { TokenLoc (L.Vmax $$) _ _ }
    vmin        { TokenLoc (L.Vmin $$) _ _ }
    vw          { TokenLoc (L.Vw $$) _ _ }
    unitLessNum { TokenLoc (L.UnitLessNum $$) _ _ }

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
    : List(Header)                                { $1 }
Header :: { Either CssRule FileHeader }
    : 'import' Import ';'                         { Right (HeaderImport $2) }
    | '@layer' LayerNames ';'                     { Right (HeaderLayers (LayerStmt $2)) }
    | '@layer' IdKwd '{' CssRuleBody '}'          { Left (LayerBlock (Just (LayerName $2)) $4) }
    | '@layer' '{' CssRuleBody '}'                { Left (LayerBlock Nothing $3) }
Import -- :: { Import SelectorList }
    : Source Os                                   { ImportUrlSupports $1 Nothing [] }
    | Source Os layer Os                          { ImportDefaultLayer $1 }
    | Source Os layer LayerNameMb Os              { ImportUrlLayer $1 $4 Nothing [] }
    | Source Os layer LayerNameMb Os Supports Os
                                                  { ImportUrlLayer $1 $4 (Just $6) [] }
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
Namespaces
    :                                             { [] }
    | Namespace ';' Namespaces                    { $1 : $3 }
Namespace
    : namespace IdKwdMb Os Source                 { Namespace $2 $4 }
Source
    : 'url(' Str ')'                              { UrlSource (Url $2) }
    | 'uqUrl'                                     { UrlSource (UnquotedUrl (pack $1)) }
    | Str                                         { StrSource $1 }
LayerNames :: { NonEmpty LayerName }
    : NonEmpty(',', LayerName)                    { $1 }
LayerName :: { LayerName }
    : IdKwd                                       { LayerName $1 }
CssFileBody
    : List(CssRule)                               { $1 }
CssRule :: { CssRule }
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }
    | 'media' '{' CssRuleBody '}'                 { MediaRule (MediaQueryList []) $3 }
    | 'media' MediaQueryList '{' CssRuleBody '}'  { MediaRule (MediaQueryList $2) $4 }
    | '@layer' IdKwdMb '{' CssRuleBody '}'        { LayerBlock (fmap LayerName $2) $4 }
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
    | '@' supports Os FeatureQuery ERB            { Supports (normalize $4) $5 }
    | '@' IdKwd Os CommaSeparatedList Os ERB      { UnknownGramma $2 (Just (CommaSeparatedList $4)) $6 }
    | '@' IdKwd Os ERB                            { UnknownGramma $2 Nothing $4 }
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
    : '@' IdKwd Os Ocb PropEntries '}'            { FontFeatureValuesSubBlock $2 $5 }
StrEitherIds :: { Either LiteralString IdentList }
    : Str                                         { Left (LiteralString $1) }
    | NonEmpty(' ', IdKwd)                        { Right (IdentList $1) }
ColorPropEntries
    : srcProp ':' PropVals ';' ColorPropEntries   { PropEntry (PropertyName "src") $3 : $5 }
    | PropEntry ColorPropEntries                  { $1 : $2 }
    |                                             { [] }
CommaSeparatedList :: { NonEmpty PropVals }
    : CssPropertyVals Important                   { (PropVals $1 $2) :| [] }
    | CssPropertyVals Important ',' CommaSeparatedList
                                                  { (PropVals $1 $2) <| $4 }
FontFacePropEntries :: { NonEmpty (Either SrcVal FontFacePropEntry) }
    : FontFaceProp                                { $1 :| [] }
    | FontFaceProp FontFacePropEntries            { $1 <| $2 }
FontFaceProp
    : srcProp ':' CommaSeparatedList ';'          { Left (CommaSeparatedList $3) }
    | unRange ':' NonEmpty(',', UnicodeRange) ';' { Right (UnicodeRangePropEntry $3) }
    | PropEntry                                   { Right (FontFaceCommonEntry $1) }
UnicodeRange :: { UnicodeRange }
    : unRangeVal                                  { UnicodeRange (pack $1) }
KeyframeList
    : List(Keyframe)                              { $1 }
Keyframe
    : KeyframeAdr Ocb PropEntries '}'             { Keyframe $1 $3 }
KeyframeAdr
    : IdKwd                                       { KeyframeLabel $1 }
    | percent                                     { KeyframePercentAdr (mkRawNum $1) }
PropEntries :: { [PropEntry] }
    : List(PropEntry)                             { $1 }
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
    | MtModifier MediaType Os                     { MediaQueryWithMt $1 $2 Nothing }
    | MediaCondition                              { MediaQueryConditionOnly $1 }
MediaType :: { MediaType }
    : mediaType                                   { $1 }
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
    : cap                                         { IntVal (mkRawNum $1) Vl.Cap }
    | ch                                          { IntVal (mkRawNum $1) Vl.Ch }
    | cm                                          { IntVal (mkRawNum $1) Vl.Cm }
    | cqb                                         { IntVal (mkRawNum $1) Vl.Cqb }
    | cqh                                         { IntVal (mkRawNum $1) Vl.Cqh }
    | cqi                                         { IntVal (mkRawNum $1) Vl.Cqi }
    | cqmax                                       { IntVal (mkRawNum $1) Vl.Cqmax }
    | cqmin                                       { IntVal (mkRawNum $1) Vl.Cqmin }
    | cqw                                         { IntVal (mkRawNum $1) Vl.Cqw }
    | deg                                         { IntVal (mkRawNum $1) Vl.Deg }
    | dpi                                         { IntVal (mkRawNum $1) Vl.Dpi }
    | dvb                                         { IntVal (mkRawNum $1) Vl.Dvb }
    | dvh                                         { IntVal (mkRawNum $1) Vl.Dvh }
    | dvi                                         { IntVal (mkRawNum $1) Vl.Dvi }
    | dvmax                                       { IntVal (mkRawNum $1) Vl.Dvmax }
    | dvmin                                       { IntVal (mkRawNum $1) Vl.Dvmin }
    | em                                          { IntVal (mkRawNum $1) Vl.Em }
    | ex                                          { IntVal (mkRawNum $1) Vl.Ex }
    | grad                                        { IntVal (mkRawNum $1) Vl.Grad }
    | ic                                          { IntVal (mkRawNum $1) Vl.Ic }
    | in                                          { IntVal (mkRawNum $1) Vl.In }
    | lh                                          { IntVal (mkRawNum $1) Vl.Lh }
    | lvb                                         { IntVal (mkRawNum $1) Vl.Lvb }
    | lvh                                         { IntVal (mkRawNum $1) Vl.Lvh }
    | lvi                                         { IntVal (mkRawNum $1) Vl.Lvi }
    | lvmax                                       { IntVal (mkRawNum $1) Vl.Lvmax }
    | lvmin                                       { IntVal (mkRawNum $1) Vl.Lvmin }
    | mm                                          { IntVal (mkRawNum $1) Vl.Mm }
    | ms                                          { IntVal (mkRawNum $1) Vl.Ms }
    | pc                                          { IntVal (mkRawNum $1) Vl.Pc }
    | pt                                          { IntVal (mkRawNum $1) Vl.Pt }
    | percent                                     { IntVal (mkRawNum $1) Vl.Percent }
    | px                                          { IntVal (mkRawNum $1) Vl.Px }
    | q                                           { IntVal (mkRawNum $1) Vl.Q }
    | rad                                         { IntVal (mkRawNum $1) Vl.Rad }
    | rcap                                        { IntVal (mkRawNum $1) Vl.Rcap }
    | rch                                         { IntVal (mkRawNum $1) Vl.Rch }
    | rem                                         { IntVal (mkRawNum $1) Vl.Rem }
    | rex                                         { IntVal (mkRawNum $1) Vl.Rex }
    | ric                                         { IntVal (mkRawNum $1) Vl.Ric }
    | rlh                                         { IntVal (mkRawNum $1) Vl.Rlh }
    | second                                      { IntVal (mkRawNum $1) Vl.Second }
    | svb                                         { IntVal (mkRawNum $1) Vl.Svb }
    | svh                                         { IntVal (mkRawNum $1) Vl.Svh }
    | svi                                         { IntVal (mkRawNum $1) Vl.Svi }
    | svmax                                       { IntVal (mkRawNum $1) Vl.Svmax }
    | svmin                                       { IntVal (mkRawNum $1) Vl.Svmin }
    | turn                                        { IntVal (mkRawNum $1) Vl.Turn }
    | vb                                          { IntVal (mkRawNum $1) Vl.Vb }
    | vh                                          { IntVal (mkRawNum $1) Vl.Vh }
    | vi                                          { IntVal (mkRawNum $1) Vl.Vi }
    | vmax                                        { IntVal (mkRawNum $1) Vl.Vmax }
    | vmin                                        { IntVal (mkRawNum $1) Vl.Vmin }
    | vw                                          { IntVal (mkRawNum $1) Vl.Vw }

    | unitLessNum                                 { IntVal (mkRawNum $1) Vl.K }
    | 'ratio'                                     { RatioVal $1 }
    | PropertyName                                { propRef $1 }
    | PropertyName '/' Os PropertyName            { Div $1 $4 }
    | PropertyName Op PropVals ')'                { AppFun $1 $3 }
    | Str                                         { StrVal $1 }
    | 'url(' Str ')'                              { UrlVal (Url $2) }
    | 'uqUrl'                                     { UrlVal (UnquotedUrl (pack $1)) }
    | hash                                        { HexColor (HC (pack $1)) }
Unsigned :: { Unsigned }
    : unitLessNum                                 {% fmap Unsigned (fromEitherM failP (readEither $1)) }
ContinueRule :: { CssRule }
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }
CssRuleBody :: { [ CssRuleBodyItem ] }
    :                                             { [] }
    | PropertyName ':' PropValsList ';' CssRuleBody
                                                  { mkLeaf $1 $3 : $5 }
    | PropertyName ':' PropValsList               { [ mkLeaf $1 $3 ] }
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
    | IdKwd dir Op IdKwd Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (Dir (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd dir Op IdKwd Os ')' ERB CssRuleBody   { newRule
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

    | IdKwd state Op IdKwd Os ')' ContinueRule CssRuleBody
                                                  { upsertHeadTagSelector
                                                      (setTag $1 . addClass (State (Embraced $4)))
                                                      $7 $8
                                                  }
    | IdKwd state Op IdKwd Os ')' ERB CssRuleBody { newRule
                                                      ( setTag $1
                                                      . addClass (State (Embraced $4)))
                                                      $7 $8
                                                  }

    | IdKwd PsTgSel ERB CssRuleBody               { newPseude (setTag $1) $2 $3 $4 }
    | IdKwd PsTgSel ',' ContinueRule CssRuleBody  { CssNestedRule (pushPeSelector (setTag $1) $2 $4) : $5 }
    | CssRule CssRuleBody                         { CssNestedRule $1 : $2 }
PropValsList :: { NonEmpty PropVals }
    : NonEmpty(',', PropVals)                     { $1 }
PropVals :: { PropVals }
    : CssPropertyVals Important                   { PropVals $1 $2 }
Important :: { Maybe Important }
    : Os Maybe(important)                         { fmap (const Important) $2 }
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
    : CompositePe TagAttrs TagClasses             { PseudeTagSelector $1 $2 $3 }
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
    : Ident '|' TagName TagAttrs TagId TagClasses { TagSelector (R.Namespace $1) $3 $4 $5 $6 }
    | Ident TagAttrs TagId TagClasses             { TagSelector NoBar (TagName $1) $2 $3 $4 }
    | '&' TagAttrs TagId TagClasses               { TagSelector NoBar AmpersandTag $2 $3 $4 }
    | '*' '|' TagName TagAttrs TagId TagClasses   { TagSelector AsteriskNs $3 $4 $5 $6 }
    | '*' TagAttrs TagId TagClasses               { TagSelector NoBar AsteriskTag $2 $3 $4 }
    | '|' TagName TagAttrs TagId TagClasses       { TagSelector NoNs $2 $3 $4 $5 }
    | TagAttrs TagId TagClasses                   { TagSelector NoBar NoTag $1 $2 $3 }
TagName :: { TagName }
    :                                             { NoTag }
    | '&'                                         { AmpersandTag }
    | '*'                                         { AsteriskTag }
    | Ident                                       { TagName $1 }
TagAttrs :: { [Attr] }
    : List(AttrBox)                               { $1 }
TagId :: { Maybe Hash }
    : Maybe(Hash)                                 { $1 }
Hash :: { Hash }
    : hash                                        { Hash (pack $1) }
TagClasses :: { [ Class ] }
    : List(TagClass)                              { $1 }
TagClass :: { Class }
    : '.' IdKwd                                   { AtomicClass $2 }
    | pseudc                                      { AtomicPseudoClass $1 }
    | ':not' ESL                                  { NotClass $2 }
    | 'lang(' Str ')'                             { Lang (Language $2) }
    | activeViewTransitionType Op CslOfIdents Os ')'  { ActiveViewTransitionType (Embraced $3) }
    | dir Op IdKwd Os ')'                         { Dir (Embraced $3) }
    | heading Op CslOfInts Os ')'                 { Heading (Embraced $3) }
    | heading                                     { AtomicPseudoClass P.Heading }
    | host ESL                                    { Host (Embraced $2) }
    | host                                        { AtomicPseudoClass P.Host }
    | state Op IdKwd Os ')'                       { State (Embraced $3) }
    | where ESL                                   { Where $2 }
    | is ESL                                      { Is $2 }
    | has ESL                                     { Has $2 }
    | pseudf Os Nth                               { call $1 $3 }
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
Nth
    : nth Os ')'                                  { $1 }
    | PMOpt IntOpt 'n' Os ')'                     { Nth (call $1 $2) 0 }
    | PMOpt IntOpt 'n' Os pm Os int Os ')'        { Nth (call $1 $2) (call $5 $7) }
    | PMOpt int Os ')'                            { Nth 0 (call $1 $2) }
PMOpt
    :                                             { TpmIdF }
    | pm                                          { $1 }
IntOpt
    :                                             { 1 }
    | int                                         { $1 }
Ocb : '{'                                         { () }
Op  : '(' Os                                      { () }
Os  :                                             { () }
    | ' '                                         { () }
AttrBox
    : '[' Attr                                    { $2 }
Attr
    : IdKwd ']'                                   { HasAttr (AttrName NoBar $1) }
    | IdKwd '|' IdKwd ']'                         { HasAttr (AttrName (R.Namespace $1) $3) }
    | IdKwd '|' IdKwd AttrOp IdTxt ']'
                                                  { Attr (AttrName (R.Namespace $1) $3) $4 $5 }
    | IdKwd '|' IdKwd AttrOp Str ']'              { Attr (AttrName (R.Namespace $1) $3) $4 $5 }
    | IdKwd AttrOp IdTxt ']'                      { Attr (AttrName NoBar $1) $2 $3 }
    | IdKwd AttrOp Str ']'                        { Attr (AttrName NoBar $1) $2 $3 }
    | '|' IdKwd ']'                               { HasAttr (AttrName NoNs $2) }
    | '|' IdKwd AttrOp IdTxt ']'                  { Attr (AttrName NoNs $2) $3 $4 }
    | '|' IdKwd AttrOp Str ']'                    { Attr (AttrName NoNs $2) $3 $4 }
    | '*' '|' IdKwd ']'                           { HasAttr (AttrName AsteriskNs $3) }
    | '*' '|' IdKwd AttrOp IdTxt ']'              { Attr (AttrName AsteriskNs $3) $4 $5 }
    | '*' '|' IdKwd AttrOp Str ']'                { Attr (AttrName AsteriskNs $3) $4 $5 }
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
    | layer                                       { R.Ident "layer" }
    | mediaType                                   { R.Ident (toStrict (toCssText $1)) }
MediaKeywordAsIdent
    : 'not'                                       { R.Ident "not" }
    | 'or'                                        { R.Ident "or" }
    | 'and'                                       { R.Ident "and" }
    | 'only'                                      { R.Ident "only" }
Ident :: { R.Ident }
    : ident                                       { R.Ident (pack $1) }
IdTxt :: { Text }
    : ident                                       { pack $1 }
Str :: { Text }
    : string                                      { pack $1 }
Var :: { R.Ident }
    : var                                         { R.Ident (pack $1) }
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
happyError _ =
  failP "Unexpected end of a CSS string"
}
