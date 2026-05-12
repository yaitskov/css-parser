module CssParser.Lexer.Token where

import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page ( PageMargin(..) )
import CssParser.Descriptor (Descriptor (BrowserSpecificDescriptor, CustomDescriptor))
import CssParser.Fun ( TpmF, NthF )
import CssParser.Ident ( BrowserPrefix, Ident(Ident), bpLength )
import CssParser.Prelude
import CssParser.Rule.Pseudo
    ( AtomicPseudoClass(UnknownPc),
      BrowserSpecificIdent(BrowserSpecificIdent),
      PseudoElement(UnknownPe),
      Nth,
      pseudoClassMap )
import CssParser.Rule.Type ( AtomicCssType )
import CssParser.Rule.TypedNum ( NumberStr )
import CssParser.Rule.Value (Ratio)
import CssParser.Show ( mkDecodingMap', smartLookup )
import CssParser.Utils ( readIdentifier )
import Data.Text (pack)
import Data.HashMap.Strict qualified as HM

data Token
  = TIncludes
  | TEqual
  | TDashMatch
  | TPrefixMatch
  | TSuffixMatch
  | TSubstringMatch
  | IdentT Text
  | AttrPatT Text
  | String String
  | UnicodeRangeVal String
  | FontFeatureValuesT
  | FontPaletteValuesT
  | Var String
  | THash String
  | TypedNum NumberStr
  | RatioT Ratio
  | Comma
  | Ampersand
  | Colon
  | Semicolon
  | Pipe
  | Plus
  | PercentT
  | SharpT
  | Minus
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Tilde
  | Dot
  | GlobalT
  | ClassT String
  | PageT
  | PageMarginT PageMargin
  | ReturnsT
  | SelectorFunT
  | CalcFunT
  | AttrFunT
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
  | AtT BrowserPrefix
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
  | RawStringT
  | DescriptorT Descriptor
  deriving (Show, Eq)


tokenizePseudoClass :: String -> Token
tokenizePseudoClass s =
  case smartLookup (pack s) pseudoClassMap of
    Just pc ->
      AtomicPseudoClassT pc
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
  [ ("container"                                      , ContainerT)
  , ("position-try"                                   , PositionTryT)
  , ("page"                                           , PageT)
  , ("all"                                            , MediaTypeT AllMt)
  , ("function"                                       , FunctionT)
  , ("font-face"                                      , FontFaceT)
  , ("top-left-corner"                                , PageMarginT TopLeftCorner)
  , ("bottom-right-corner"                            , PageMarginT BottomRightCorner)
  , ("top-right-corner"                               , PageMarginT TopRightCorner)
  , ("bottom-left-corner"                             , PageMarginT BottomLeftCorner)
  , ("top-left"                                       , PageMarginT TopLeft)
  , ("top-center"                                     , PageMarginT TopCenter)
  , ("top-right"                                      , PageMarginT TopRight)
  , ("bottom-left"                                    , PageMarginT BottomLeft)
  , ("bottom-center"                                  , PageMarginT BottomCenter)
  , ("bottom-right"                                   , PageMarginT BottomRight)
  , ("left-top"                                       , PageMarginT LeftTop)
  , ("left-middle"                                    , PageMarginT LeftMiddle)
  , ("left-bottom"                                    , PageMarginT LeftBottom)
  , ("right-top"                                      , PageMarginT RightTop)
  , ("right-middle"                                   , PageMarginT RightMiddle)
  , ("right-bottom"                                   , PageMarginT RightBottom)
  , ("scope"                                          , ScopeT)
  , ("view-transition"                                , ViewTransitionT)
  , ("starting-style"                                 , StartingStyleT)
  , ("font-palette-values"                            , FontPaletteValuesT)
  , ("font-feature-values"                            , FontFeatureValuesT)
  , ("color-profile"                                  , ColorProfileT)
  , ("media"                                          , MediaT)
  , ("layer"                                          , LayerT)
  , ("returns"                                        , ReturnsT)
  , ("from"                                           , FromT)
  , ("to"                                             , ToT)
  , ("print"                                          , MediaTypeT Print)
  , ("screen"                                         , MediaTypeT Screen)
  , ("tty"                                            , MediaTypeT Tty)
  , ("tv"                                             , MediaTypeT Tv)
  , ("projection"                                     , MediaTypeT Projection)
  , ("handheld"                                       , MediaTypeT Handheld  )
  , ("braille"                                        , MediaTypeT Braille   )
  , ("embossed"                                       , MediaTypeT Embossed  )
  , ("aural"                                          , MediaTypeT Aural     )
  , ("speech"                                         , MediaTypeT Speech    )
  , ("supports"                                       , SupportsT)
  ]
