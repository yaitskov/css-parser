-- vim:ft=haskell
{
module CssParser.Parser where

import CssParser.At
import CssParser.At.Import
import CssParser.Pseudo
import CssParser.File
import CssParser.FixRule
import CssParser.Ident hiding (Ident)
import CssParser.Ident qualified as R
import CssParser.Lexer
  ( AlexPosn(AlexPn), TokenLoc(TokenLoc)
  , Token
    ( TIncludes, TEqual, TDashMatch, TPrefixMatch, TSuffixMatch, TSubstringMatch, Ident
    , Integer, Comma, Plus, Greater, Tilde, Dot, Asterisk, Space, BOpen, BClose, PseudoFunction
    , PseudoElementT, TN, TNth, TPM, TInt, TClose, TNot, TLang, Decimal, String, THash
    , COpen, CClose, Colon, Semicolon, Var, Pipe, AtomicPseudoClassT, Ampersand
    , CharsetT, UrlT, ImportT
    )
  )
import CssParser.Rule
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Text (pack)

import Prelude
}

%name cssParser
%tokentype { TokenLoc }
%error { happyError }

%token
    ','         { TokenLoc Comma _ _ }
    ':'         { TokenLoc Colon _ _ }
    ';'         { TokenLoc Semicolon _ _ }
    '>'         { TokenLoc Greater _ _ }
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
    var         { TokenLoc (Var $$) _ _ }
    nth         { TokenLoc (TNth $$) _ _ }
    'not('      { TokenLoc TNot _ _ }
    'lang('     { TokenLoc TLang _ _ }
    ')'         { TokenLoc TClose _ _ }

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

CssRule
    : SelectorList '{' CssRuleBody '}'            { CssRule $1 $3 }

CssRuleBody :: { [ CssRuleBodyItem ] }
    :                                             { [] }
    | Var   ':' CssPropertyVals ';' CssRuleBody   { CssLeafRule (PropertyName $1) $3 : $5 }
    | Ident ':' CssPropertyVals ';' CssRuleBody   { CssLeafRule (PropertyName $1) $3 : $5 }
    | Var   ':' CssPropertyVals                   { [ CssLeafRule (PropertyName $1) $3 ] }
    | Ident ':' CssPropertyVals                   { [ CssLeafRule (PropertyName $1) $3 ] }
    | Ident '{' CssRuleBody '}' CssRuleBody       { CssNestedRule (tagNameRule $1 $3) : $5 }
    | Ident '>' CssRule CssRuleBody               { CssNestedRule (prependIdentToRule $1 Child $3) : $4 }
    | Ident ' ' CssRule CssRuleBody               { CssNestedRule (prependIdentToRule $1 Descendant $3) : $4 }
    | Ident '|' CssRule CssRuleBody               { CssNestedRule
                                                     (updateTopTagSelector (setTsNs $1) $3) : $4
                                                  }
    | Ident '+' CssRule CssRuleBody               { CssNestedRule (prependIdentToRule $1 NextSibling $3) : $4 }
    | Ident '~' CssRule CssRuleBody               { CssNestedRule (prependIdentToRule $1 GeneralSibling $3) : $4 }
    | Ident '[' Attr '{' CssRuleBody '}' CssRuleBody
                                                  { CssNestedRule (tagAndAttrRule $1 $3 $5) : $7 }
    | Ident '[' Attr TagRelation CssRule CssRuleBody
                                                  { CssNestedRule (prependIdentAttrSelector $1 $3 $4 $5) : $6 }
    | Ident '[' Attr CssRule CssRuleBody          { CssNestedRule (setTopTagName $1 (prependAttr $3 $4)) : $5 }
    | Ident ',' CssRule CssRuleBody               { CssNestedRule (prependSelectorToRule $1 $3) : $4 }
    | Ident '.' CssRule CssRuleBody               { CssNestedRule (tagNameIsClass $1 $3) : $4 }
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
    : '+'          { NextSibling }
    | '>'          { Child }
    | '~'          { GeneralSibling }
    | ' '          { Descendant }
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
happyError :: [TokenLoc] -> a
happyError (~(TokenLoc t s ~(Just (AlexPn _ l c))):_) =
  error $ "Can not parse CSS: unpexected token \"" <>
    s <> "\" at (" <> show l <> ", " <> show c <> ")"
happyError _ =
  error "Unexpected end of a CSS string"
}
