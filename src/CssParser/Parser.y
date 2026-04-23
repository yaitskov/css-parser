-- vim:ft=haskell
{
module CssParser.Parser where

import CssParser.At
import CssParser.At.Import
import CssParser.At.MediaQuery hiding (Mm, Cm, Dpi, Em)
import CssParser.At.MediaQuery qualified as MQ
import CssParser.Rule.Pseudo
import CssParser.Rule.Value
import CssParser.Fun
import CssParser.File
import CssParser.FixRule
import CssParser.Ident hiding (Ident)
import CssParser.Ident qualified as R
import CssParser.Lexer qualified as L
import CssParser.Lexer
  ( AlexPosn(AlexPn), TokenLoc(TokenLoc)
  , Token
    ( TIncludes, TEqual, TDashMatch, TPrefixMatch, TSuffixMatch, TSubstringMatch, Ident
    , Integer, Comma, Plus, Tilde, Dot, Asterisk, Space, BOpen, BClose, PseudoFunction
    , PseudoElementT, TN, TNth, TPM, TInt, TNot, TLang, Decimal, String, THash
    , COpen, CClose, Colon, Semicolon, Var, Pipe, AtomicPseudoClassT, Ampersand
    , CharsetT, UrlT, ImportT, MediaT, NotT, OrT, AndT, OnlyT
    , TOpen, TClose
    , Greater, Less, LessEqual, GreaterEqual
    , RatioT, Percents, Pixels
    )
  )
import CssParser.Parser.Monad
import CssParser.Rule
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Text (pack)

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
    'import'    { TokenLoc ImportT _ _ }
    'media'     { TokenLoc MediaT _ _ }
-- media start
    'only'      { TokenLoc OnlyT _ _ }
    'not'       { TokenLoc NotT _ _ }
    'or'        { TokenLoc OrT _ _ }
    'and'       { TokenLoc AndT _ _ }
-- media end
    'url('      { TokenLoc UrlT _ _ }
    '^='        { TokenLoc TPrefixMatch _ _ }
    '$='        { TokenLoc TSuffixMatch _ _ }
    '*='        { TokenLoc TSubstringMatch _ _ }
    '|='        { TokenLoc TDashMatch _ _ }
    '~='        { TokenLoc TIncludes _ _ }
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
    'mm'        { TokenLoc (L.Mm $$) _ _ }
    'em'        { TokenLoc (L.Em $$) _ _ }
    'cm'        { TokenLoc (L.Cm $$) _ _ }
    'vw'        { TokenLoc (L.Vw $$) _ _ }
    'vh'        { TokenLoc (L.Vh $$) _ _ }
    'dpi'       { TokenLoc (L.Dpi $$) _ _ }
    'percents'  { TokenLoc (Percents $$) _ _ }
    'px'        { TokenLoc (Pixels $$) _ _ }
    var         { TokenLoc (Var $$) _ _ }
    nth         { TokenLoc (TNth $$) _ _ }
    'not('      { TokenLoc TNot _ _ }
    'lang('     { TokenLoc TLang _ _ }
    '('         { TokenLoc TOpen _ _ }
    ')'         { TokenLoc TClose _ _ }

%left 'or' 'and'
%right 'not'
%%

CssFile
    : 'charset' Str ';' Imports CssFileBody       { CssFile (Just (Charset $2)) $4 $5 }
    | Imports CssFileBody                         { CssFile Nothing $1 $2 }
Imports
    :                                             { [] }
    | Import Imports                              { $1 : $2 }
Import
    : 'import' Str ';'                            { Import (ImportSourceStr $2) }
    | 'import' 'url(' Str ')' ';'                 { Import (ImportSourceUrl (Url $3)) }
CssFileBody
    : CssRule                                     { $1 :| [] }
    | CssRule CssFileBody                         { $1 <| $2 }

CssRule :: { CssRule }
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }
    | 'media' '{' CssRuleBody '}'                 { MediaRule (MediaQueryList []) $3 }
    | 'media' MediaQueryList '{' CssRuleBody '}'  { MediaRule (MediaQueryList $2) $4 }

MediaQueryList :: { [ MediaQuery ] }
    : MediaQuery                                  { [ $1 ] }
    | MediaQuery ',' MediaQueryList               { $1 : $3 }

MediaQuery :: { MediaQuery }
    : 'not' Os '(' MediaFeature ')'               { MediaQueryConditionOnly (MediaNot $4) }
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
    : 'not' '(' MediaFeature ')' Os 'and' MediaCondition
                                                  { MediaBin And $3 $7 }
    | 'not' '(' MediaFeature ')' Os 'or' MediaCondition
                                                  { MediaBin Or $3 $7 }
    | 'not' '(' MediaFeature ')'                  { MediaNot $3 }
    | '(' MediaFeature ')' Os 'and' MediaCondition
                                                  { MediaBin And $2 $6 }
    | '(' MediaFeature ')' Os 'or' MediaCondition
                                                  { MediaBin Or $2 $6 }
    | '(' MediaFeature ')'                        { MediaFeature $2 }

MediaFeature :: { MediaFeature }
    : Ident ':' PropVal                           { PlainMf $1 $3 }
    | Ident '=' PropVal                           { OpenRangeFeature $1 MfEq $3 }
    | Ident '<' Ident MfRel PropVal               { MfClosedRange (IdentRef $1) MfLt $3 $4 $5 }
    | Ident '<' PropVal                           { OpenRangeFeature $1 MfLt $3 }
    | Ident '>' Ident MfRel PropVal               { MfClosedRange (IdentRef $1) MfGt $3 $4 $5 }
    | Ident '>' PropVal                           { OpenRangeFeature $1 MfGt $3 }
    | Ident '<=' Ident MfRel PropVal              { MfClosedRange (IdentRef $1) MfLe $3 $4 $5 }
    | Ident '<=' PropVal                          { OpenRangeFeature $1 MfLe $3 }
    | Ident '>=' Ident MfRel PropVal              { MfClosedRange (IdentRef $1) MfGe $3 $4 $5 }
    | Ident '>=' PropVal                          { OpenRangeFeature $1 MfGe $3 }
    | Ident                                       { BooleanMf $1 }
    | PropVal MfRel Ident                         { OpenRangeFeatureFlipped $1 $2 $3 }
    | PropVal MfRel Ident MfRel PropVal           { MfClosedRange $1 $2 $3 $4 $5 }

MfRel :: { MfRelation }
    : '<'                                         { MfLt }
    | '>'                                         { MfGt }
    | '<='                                        { MfLe }
    | '>='                                        { MfGe }
    | '='                                         { MfEq }

PropVal :: { PropVal }
    : 'mm'                                        { IntVal (Unsigned $1) MQ.Mm }
    | 'em'                                        { IntVal (Unsigned $1) MQ.Em }
    | 'cm'                                        { IntVal (Unsigned $1) MQ.Cm }
    | 'vw'                                        { IntVal (Unsigned $1) MQ.Vw }
    | 'vh'                                        { IntVal (Unsigned $1) MQ.Vh }
    | 'dpi'                                       { IntVal (Unsigned $1) MQ.Dpi }
    | 'percents'                                  { IntVal (Unsigned $1) MQ.Percent }
    | 'px'                                        { IntVal (Unsigned $1) MQ.Px }
    | Unsigned                                    { IntVal $1 MQ.K }
    | 'ratio'                                     { RatioVal $1 }
    | Ident                                       { IdentRef $1 }

Unsigned
    : integer                                     { Unsigned $1 }

CssRuleBody :: { [ CssRuleBodyItem ] }
    :                                             { [] }
    | Var   ':' CssPropertyVals ';' CssRuleBody   { CssLeafRule (PropertyName $1) $3 : $5 }
    | IdKwd ':' CssPropertyVals ';' CssRuleBody   { CssLeafRule (PropertyName $1) $3 : $5 }
    | Var   ':' CssPropertyVals                   { [ CssLeafRule (PropertyName $1) $3 ] }
    | IdKwd ':' CssPropertyVals                   { [ CssLeafRule (PropertyName $1) $3 ] }
    | IdKwd '{' CssRuleBody '}' CssRuleBody       { CssNestedRule (tagNameRule $1 $3) : $5 }
    | IdKwd '>' OptSpace CssRule CssRuleBody      { CssNestedRule (prependIdentToRule $1 Child $4) : $5 }
    | IdKwd ' ' CssRule CssRuleBody               { CssNestedRule (prependIdentToRule $1 Descendant $3) : $4 }
    | IdKwd '|' CssRule CssRuleBody               { CssNestedRule
                                                     (updateTopTagSelector (setTsNs $1) $3) : $4
                                                  }
    | IdKwd '+' OptSpace CssRule CssRuleBody      { CssNestedRule (prependIdentToRule $1 NextSibling $4) : $5 }
    | IdKwd '~' OptSpace CssRule CssRuleBody      { CssNestedRule (prependIdentToRule $1 GeneralSibling $4) : $5 }
    | IdKwd '[' Attr '{' CssRuleBody '}' CssRuleBody
                                                  { CssNestedRule (tagAndAttrRule $1 $3 $5) : $7 }
    | IdKwd '[' Attr TagRelation CssRule CssRuleBody
                                                  { CssNestedRule (prependIdentAttrSelector $1 $3 $4 $5) : $6 }
    | IdKwd '[' Attr CssRule CssRuleBody          { CssNestedRule (setTopTagName $1 (prependAttr $3 $4)) : $5 }
    | IdKwd ',' CssRule CssRuleBody               { CssNestedRule (prependSelectorToRule $1 $3) : $4 }
    | IdKwd '.' CssRule CssRuleBody               { CssNestedRule (tagNameIsClass $1 $3) : $4 }
    | CssRule CssRuleBody                         { CssNestedRule $1 : $2 }

CssPropertyVals :: { NonEmpty () }
    : CssPropertyVal                              { $1 :| [] }
    | CssPropertyVal ' ' CssPropertyVals          { $1 <| $3 }
    | CssPropertyVal CssPropertyVals              { $1 <| $2 }

CssPropertyVal :: { () }
    : integer                                     { () }
    | ident                                       { () }
    | string                                      { () }

SelectorList :: { NonEmpty Selector }
    : Selector                                    { $1 :| [] }
    | Selector ',' SelectorList                   { $1 <| $3 }
    ;

Selector :: { Selector }
    : TagSelector ZipTagRelationAndTagSelector        { Selector $1 $2 }
    | TagSelector ZipTagRelationAndTagSelector pseude { PeSelector $1 $2 $3 }
    | pseude                                          { PeSelectorOnly $1 }
    ;

TagSelector :: { TagSelector }
    : Ident '|' TagName TagAttrs TagId TagClasses     { TagSelector (Namespace $1) $3 $4 $5 $6 }
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
    | hash                                            { Just (Hash (pack $1)) }

TagClasses :: { [ Class ] }
    :                                                 { [] }
    | TagClass TagClasses                             { $1 : $2 }

TagClass :: { Class }
    : '.' Ident                                       { AtomicClass $2 }
    | pseudc                                          { AtomicPseudoClass $1 }
    | 'not(' SelectorList ')'                         { NotClass $2 }
    | 'lang(' string ')'                              { Lang (Language (pack $2)) }
    | pseudf OptSpace Nth                             { call $1 $3 }

ZipTagRelationAndTagSelector :: { [ (TagRelation, TagSelector) ] }
    :                                                 { [] }
    | TagRelation TagSelector ZipTagRelationAndTagSelector
                                                      { ($1, $2) : $3 }

TagRelation :: { TagRelation }
    : '+' OptSpace { NextSibling }
    | '>' OptSpace { Child }
    | '~' OptSpace { GeneralSibling }
    | ' ' OptSpace { Descendant }
    ;

Nth
    : nth OptSpace ')'                                       { $1 }
    | PMOpt IntOpt 'n' OptSpace ')'                          { Nth (call $1 $2) 0 }
    | PMOpt IntOpt 'n' OptSpace pm OptSpace int OptSpace ')' { Nth (call $1 $2) (call $5 $7) }
    | PMOpt int OptSpace ')'                                 { Nth 0 (call $1 $2) }
    ;

PMOpt
    :                             { TpmIdF }
    | pm                          { $1 }
    ;

IntOpt
    :                             { 1 }
    | int                         { $1 }
    ;

OptSpace
    :                             { () }
    | ' '                         { () }
    ;

Os
    :                             { () }
    | ' '                         { () }


AttrBox
    : '[' Attr                        { $2 }
    ;

Attr
    : Ident ']'                       { HasAttr (AttrName NoBar $1) }
    | Ident '|' Ident ']'             { HasAttr (AttrName (Namespace $1) $3) }
    | Ident '|' Ident AttrOp IdTxt ']'
                                      { Attr (AttrName (Namespace $1) $3) $4 $5 }
    | Ident '|' Ident AttrOp Str ']'  { Attr (AttrName (Namespace $1) $3) $4 $5 }
    | Ident AttrOp IdTxt ']'          { Attr (AttrName NoBar $1) $2 $3 }
    | Ident AttrOp Str ']'            { Attr (AttrName NoBar $1) $2 $3 }
    | '|' Ident ']'                   { HasAttr (AttrName NoNs $2) }
    | '|' Ident AttrOp IdTxt ']'      { Attr (AttrName NoNs $2) $3 $4 }
    | '|' Ident AttrOp Str ']'        { Attr (AttrName NoNs $2) $3 $4 }
    | '*' '|' Ident ']'               { HasAttr (AttrName AsteriskNs $3) }
    | '*' '|' Ident AttrOp IdTxt ']'  { Attr (AttrName AsteriskNs $3) $4 $5 }
    | '*' '|' Ident AttrOp Str ']'    { Attr (AttrName AsteriskNs $3) $4 $5 }

AttrOp ::  { AttrOp }
    : '='                         { Exact }
    | '~='                        { Include }
    | '|='                        { DashMatch }
    | '^='                        { PrefixMatch }
    | '$='                        { SuffixMatch }
    | '*='                        { SubstringMatch }
    ;

IdKwd
    : Ident        { $1 }
    | MediaKeywordAsIdent { $1 }

MediaKeywordAsIdent
    : 'not'      { R.Ident "not" }
    | 'or'       { R.Ident "or" }
    | 'and'      { R.Ident "and" }
    | 'only'     { R.Ident "only" }

Ident
    : ident        { R.Ident (pack $1) }

IdTxt
    : ident        { pack $1 }

Str
    : string        { pack $1 }

Var
    : var        { R.Ident (pack $1) }
    ;

{
happyError :: [TokenLoc] -> P a
happyError (~(TokenLoc t s ~(Just (AlexPn _ l c))):_) =
  failP $ "Can not parse CSS: unpexected token \"" <>
    s <> "\" at (" <> show l <> ", " <> show c <> ")"
happyError _ =
  failP "Unexpected end of a CSS string"
}
