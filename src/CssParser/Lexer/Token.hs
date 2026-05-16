module CssParser.Lexer.Token where

import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page ( PageMargin(..) )
import CssParser.Descriptor (Descriptor (BrowserSpecificDescriptor, CustomDescriptor))
import CssParser.Ident ( BrowserPrefix, Ident(Ident), bpLength )
import CssParser.Prelude
import CssParser.Rule.Pseudo
    ( AtomicPseudoClass(UnknownPc),
      BrowserSpecificIdent(BrowserSpecificIdent),
      PseudoElement(UnknownPe, After, Before) )
import CssParser.Rule.Type ( AtomicCssType )
import CssParser.Rule.TypedNum ( NumberStr )
import CssParser.Rule.Value
    ( CalcFns(CalcFn, MinFn, MaxFn, SinFn, CosFn, ClampFn), Ratio )
import CssParser.Show ( mkDecodingMap', smartLookup )
import CssParser.Utils ( readIdentifier )
import Data.Text (pack)
import Data.HashMap.Strict qualified as HM

data Token
  = AlphaT
  | Ampersand
  | AndT
  | Asterisk
  | AtomicPseudoClassT AtomicPseudoClass
  | AtT BrowserPrefix
  | AttrFunT
  | AttrPatT Text
  | BClose
  | BOpen
  | CalcFunT CalcFns
  | CClose
  | CharsetT
  | ClassT String
  | Colon
  | ColorProfileT
  | Comma
  | ContainerT
  | CustomMediaT
  | CustomSelectorT
  | COpen
  | CounterStyleT
  | DescriptorT Descriptor
  | DivT
  | Dot
  | EvenT
  | FalseT
  | FontFaceT
  | FontFeatureValuesT
  | FontPaletteValuesT
  | FromT
  | FunctionT
  | GlobalT
  | Greater
  | GreaterEqual
  | IdentT Text
  | ImportantT
  | ImportT
  | KeyframesT
  | LayerT
  | Less
  | LessEqual
  | MediaT
  | MediaTypeT MediaType
  | MixinT
  | DefineMixinT
  | Minus
  | NamespaceT
  | NotT
  | OddT
  | OfT
  | OnlyT
  | OrT
  | PageMarginT PageMargin
  | PageT
  | PercentT
  | Pipe
  | Plus
  | PositionTryT
  | PropertyT
  | PseudoElementT PseudoElement
  | RatioT Ratio
  | RawStringT
  | ReturnsT
  | ScopeT
  | SelectorFunT
  | Semicolon
  | SharpT
  | Space
  | StartingStyleT
  | String String
  | SupportsT
  | SyntaxTypeT AtomicCssType
  | TActiveViewTransitionType
  | TClose
  | TDashMatch
  | TDir
  | TEqual
  | THas
  | THash String
  | THeading
  | THighlight
  | THost
  | Tilde
  | TIncludes
  | TInt Int
  | TIs
  | TLang
  | TNot
  | TOpen
  | ToT
  | TPart
  | TPicker
  | TPrefixMatch
  | TrueT
  | TScrollButton
  | TSlotted
  | TState
  | TSubstringMatch
  | TSuffixMatch
  | TViewTransitionGroup
  | TViewTransitionImagePair
  | TViewTransitionNew
  | TViewTransitionOld
  | TWhere
  | TypedNum NumberStr
  | TypeFunT
  | UnicodeRangeVal String
  | UnquotedUrlT String
  | UrlT
  | Var String
  | ViewTransitionT

  | NthChildT
  | NthOfTypeT
  | NthLastChildT
  | NthLastOfTypeT

  deriving (Show, Eq)

pseudoClassMap :: HashMap Text Token
pseudoClassMap = auto <> hand
  where
    auto = fmap AtomicPseudoClassT . mkDecodingMap' $ drop 1 genum
    hand =
      HM.fromList
      [ (":after", PseudoElementT After)
      , (":before", PseudoElementT Before)
      , (":global", GlobalT)
      , (":not", TNot)
      , (":where", TWhere)
      , (":has", THas)
      , (":is", TIs)
      , (":active-view-transition-type", TActiveViewTransitionType)
      , (":dir", TDir)
      , (":state", TState)
      , (":nth-child",         NthChildT     )
      , (":nth-of-type",       NthOfTypeT    )
      , (":nth-last-child",    NthLastChildT )
      , (":nth-last-of-type",  NthLastOfTypeT)
      ]

tokenizePseudoClass :: String -> Token
tokenizePseudoClass s =
  case smartLookup (pack s) pseudoClassMap of
    Just pc -> pc
    Nothing ->
      AtomicPseudoClassT . UnknownPc . BrowserSpecificIdent . Ident . pack $ drop 1 s

pseudoElementMap :: HashMap Text Token
pseudoElementMap = auto <> hand
  where
    auto = fmap PseudoElementT . mkDecodingMap' $ drop 1 genum
    hand =
      HM.fromList
      [ ("::highlight", THighlight)
      , ("::part", TPart)
      , ("::picker", TPicker)
      , ("::scroll-button", TScrollButton)
      , ("::slotted", TSlotted)
      , ("::view-transition-group", TViewTransitionGroup)
      , ("::view-transition-image-pair", TViewTransitionImagePair)
      , ("::view-transition-new", TViewTransitionNew)
      , ("::view-transition-old", TViewTransitionOld)
      ]

tokenizePseudoElement :: String -> Token
tokenizePseudoElement s =
  case smartLookup (pack s) pseudoElementMap of
    Just pe -> pe
    Nothing ->
      PseudoElementT . UnknownPe . Ident . pack $ drop 2 s

readCustomDescriptor :: String -> Token
readCustomDescriptor =
  DescriptorT . CustomDescriptor . Ident . pack . readIdentifier . dropEnd 1 . drop 2

readBpDescriptor :: BrowserPrefix -> String -> Token
readBpDescriptor bp =
  DescriptorT . BrowserSpecificDescriptor bp . Ident . pack . readIdentifier . dropEnd 1 . drop (bpLength bp)

descriptorKeywords :: HashMap Text Token
descriptorKeywords =
  HM.fromList
  [ ("all"                                            , MediaTypeT AllMt)
  , ("alpha"                                          , AlphaT)
  , ("aural"                                          , MediaTypeT Aural)
  , ("bottom-center"                                  , PageMarginT BottomCenter)
  , ("bottom-left"                                    , PageMarginT BottomLeft)
  , ("bottom-left-corner"                             , PageMarginT BottomLeftCorner)
  , ("bottom-right"                                   , PageMarginT BottomRight)
  , ("bottom-right-corner"                            , PageMarginT BottomRightCorner)
  , ("braille"                                        , MediaTypeT Braille)
  , ("color-profile"                                  , ColorProfileT)
  , ("container"                                      , ContainerT)
  , ("custom-media"                                   , CustomMediaT)
  , ("custom-selector"                                , CustomSelectorT)
  , ("embossed"                                       , MediaTypeT Embossed)
  , ("false"                                          , FalseT)
  , ("font-face"                                      , FontFaceT)
  , ("font-feature-values"                            , FontFeatureValuesT)
  , ("font-palette-values"                            , FontPaletteValuesT)
  , ("from"                                           , FromT)
  , ("function"                                       , FunctionT)
  , ("handheld"                                       , MediaTypeT Handheld)
  , ("layer"                                          , LayerT)
  , ("left-bottom"                                    , PageMarginT LeftBottom)
  , ("left-middle"                                    , PageMarginT LeftMiddle)
  , ("left-top"                                       , PageMarginT LeftTop)
  , ("media"                                          , MediaT)
  , ("mixin"                                          , MixinT)
  , ("define-mixin"                                   , DefineMixinT)
  , ("of"                                             , OfT)
  , ("even"                                           , EvenT)
  , ("odd"                                            , OddT)
  , ("page"                                           , PageT)
  , ("position-try"                                   , PositionTryT)
  , ("print"                                          , MediaTypeT Print)
  , ("projection"                                     , MediaTypeT Projection)
  , ("property"                                       , PropertyT)
  , ("charset"                                        , CharsetT)
  , ("namespace"                                      , NamespaceT)
  , ("counter-style"                                  , CounterStyleT)
  , ("import"                                         , ImportT)
  , ("only"                                           , OnlyT)
  , ("not"                                            , NotT)
  , ("and"                                            , AndT)
  , ("or"                                             , OrT)
  , ("keyframes"                                      , KeyframesT)
  , ("returns"                                        , ReturnsT)
  , ("right-bottom"                                   , PageMarginT RightBottom)
  , ("right-middle"                                   , PageMarginT RightMiddle)
  , ("right-top"                                      , PageMarginT RightTop)
  , ("scope"                                          , ScopeT)
  , ("screen"                                         , MediaTypeT Screen)
  , ("speech"                                         , MediaTypeT Speech)
  , ("starting-style"                                 , StartingStyleT)
  , ("supports"                                       , SupportsT)
  , ("min"                                            , CalcFunT MinFn)
  , ("max"                                            , CalcFunT MaxFn)
  , ("max"                                            , CalcFunT MaxFn)
  , ("sin"                                            , CalcFunT SinFn)
  , ("cos"                                            , CalcFunT CosFn)
  , ("clamp"                                          , CalcFunT ClampFn)
  , ("calc"                                           , CalcFunT CalcFn)
  , ("to"                                             , ToT)
  , ("top-center"                                     , PageMarginT TopCenter)
  , ("top-left"                                       , PageMarginT TopLeft)
  , ("top-left-corner"                                , PageMarginT TopLeftCorner)
  , ("top-right"                                      , PageMarginT TopRight)
  , ("top-right-corner"                               , PageMarginT TopRightCorner)
  , ("true"                                           , TrueT)
  , ("tty"                                            , MediaTypeT Tty)
  , ("tv"                                             , MediaTypeT Tv)
  , ("view-transition"                                , ViewTransitionT)
  ]
