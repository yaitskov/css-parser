module CssParser.Lexer.Token where

import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page ( PageMargin(..) )
import CssParser.Descriptor (Descriptor (BrowserSpecificDescriptor, CustomDescriptor))
import CssParser.Fun ( TpmF, NthF )
import CssParser.Ident ( BrowserPrefix, Ident(Ident), bpLength )
import CssParser.Prelude
import CssParser.Rule.Pseudo
import CssParser.Rule.Type ( AtomicCssType )
import CssParser.Rule.TypedNum ( NumberStr )
import CssParser.Rule.Value (Ratio)
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
  | CalcFunT
  | CClose
  | CharsetT
  | ClassT String
  | Colon
  | ColorProfileT
  | Comma
  | ContainerT
  | CustomMediaT
  | COpen
  | CounterStyleT
  | DescriptorT Descriptor
  | DivT
  | Dot
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
  | Minus
  | NamespaceT
  | NotT
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
  | PseudoFunction NthF
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
  | TN
  | TNot
  | TNth Nth
  | TOpen
  | ToT
  | TPart
  | TPicker
  | TPM TpmF
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
  , ("aural"                                          , MediaTypeT Aural     )
  , ("bottom-center"                                  , PageMarginT BottomCenter)
  , ("bottom-left"                                    , PageMarginT BottomLeft)
  , ("bottom-left-corner"                             , PageMarginT BottomLeftCorner)
  , ("bottom-right"                                   , PageMarginT BottomRight)
  , ("bottom-right-corner"                            , PageMarginT BottomRightCorner)
  , ("braille"                                        , MediaTypeT Braille   )
  , ("color-profile"                                  , ColorProfileT)
  , ("container"                                      , ContainerT)
  , ("custom-media"                                   , CustomMediaT)
  , ("embossed"                                       , MediaTypeT Embossed  )
  , ("false"                                          , FalseT)
  , ("font-face"                                      , FontFaceT)
  , ("font-feature-values"                            , FontFeatureValuesT)
  , ("font-palette-values"                            , FontPaletteValuesT)
  , ("from"                                           , FromT)
  , ("function"                                       , FunctionT)
  , ("handheld"                                       , MediaTypeT Handheld  )
  , ("layer"                                          , LayerT)
  , ("left-bottom"                                    , PageMarginT LeftBottom)
  , ("left-middle"                                    , PageMarginT LeftMiddle)
  , ("left-top"                                       , PageMarginT LeftTop)
  , ("media"                                          , MediaT)
  , ("page"                                           , PageT)
  , ("position-try"                                   , PositionTryT)
  , ("print"                                          , MediaTypeT Print)
  , ("projection"                                     , MediaTypeT Projection)
  , ("returns"                                        , ReturnsT)
  , ("right-bottom"                                   , PageMarginT RightBottom)
  , ("right-middle"                                   , PageMarginT RightMiddle)
  , ("right-top"                                      , PageMarginT RightTop)
  , ("scope"                                          , ScopeT)
  , ("screen"                                         , MediaTypeT Screen)
  , ("speech"                                         , MediaTypeT Speech    )
  , ("starting-style"                                 , StartingStyleT)
  , ("supports"                                       , SupportsT)
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
