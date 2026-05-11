module CssParser.Lexer.Token where

import CssParser.At.Function (AtomicCssType)
import CssParser.At.MediaQuery (MediaType(..))
import CssParser.At.Page ( PageMargin(..) )
import CssParser.Descriptor (Descriptor (BrowserSpecificDescriptor, CustomDescriptor))
import CssParser.Fun ( TpmF, NthF )
import CssParser.Ident ( BrowserPrefix, Ident(Ident), bpLength )
import CssParser.Prelude
import CssParser.Rule.Pseudo ( AtomicPseudoClass, Nth, PseudoElement )
import CssParser.Rule.Value (Ratio)
import CssParser.Utils ( readIdentifier )
import Data.Text (pack)
import Data.HashMap.Strict qualified as HM

type NumberStr = String

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
  | UnitLessNum String

  | Deg NumberStr
  | Rad NumberStr
  | Grad NumberStr
  | Fr NumberStr
  | Hz NumberStr
  | KHz NumberStr
  | Turn NumberStr

  | Px NumberStr
  | Mm NumberStr
  | Ms NumberStr
  | Cm NumberStr
  | Em NumberStr
  | Vh NumberStr
  | Vw NumberStr
  | Rem NumberStr
  | Dpi NumberStr
  | Rcap NumberStr
  | Cap NumberStr
  | Ch NumberStr
  | Rch NumberStr
  | Ex NumberStr
  | Rex NumberStr
  | Lh NumberStr
  | Rlh NumberStr
  | Ic NumberStr
  | Ric NumberStr
  | Pc NumberStr
  | In NumberStr
  | Pt NumberStr
  | Q NumberStr
  | Second NumberStr
  | Svh NumberStr
  | Dvh NumberStr
  | Lvh NumberStr
  | Vb NumberStr
  | Lvb NumberStr
  | Dvb NumberStr
  | Svb NumberStr
  | Vi NumberStr
  | Lvi NumberStr
  | Dvi NumberStr
  | Svi NumberStr
  | Vmax NumberStr
  | Lvmax NumberStr
  | Dvmax NumberStr
  | Svmax NumberStr
  | Vmin NumberStr
  | Lvmin NumberStr
  | Dvmin NumberStr
  | Svmin NumberStr
  | Cqw NumberStr
  | Cqh NumberStr
  | Cqi NumberStr
  | Cqb NumberStr
  | Cqmax NumberStr
  | Cqmin NumberStr
  | Percents NumberStr

  | RatioT Ratio
  | Comma
  | Ampersand
  | Colon
  | Semicolon
  | Pipe
  | Plus
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

  | DescriptorT Descriptor
  deriving (Show, Eq)

readCustomDescriptor :: String -> Token
readCustomDescriptor =
  DescriptorT . CustomDescriptor . Ident . pack . readIdentifier . dropEnd 1 . drop 2

readBpDescriptor :: BrowserPrefix -> String -> Token
readBpDescriptor bp =
  DescriptorT . BrowserSpecificDescriptor bp . Ident . pack . readIdentifier . dropEnd 1 . drop (bpLength bp)

descriptorKeywords :: HM.HashMap Text Token
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
