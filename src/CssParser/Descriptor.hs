module CssParser.Descriptor where

import CssParser.Ident ( BrowserPrefix, Ident (..), PropertyName(..), Var (..) )
import CssParser.Prelude
    ( ($),
      Bounded(..),
      Enum,
      Eq,
      Ord,
      Show,
      Generic,
      Semigroup((<>)),
      (.),
      Text )
import CssParser.Show ( CssShow(..), mkDecodingMap)
import Data.HashMap.Strict qualified as HM
import Data.Text.Lazy (dropEnd, toStrict)

data Descriptor
  = CustomDescriptor Ident
  | BrowserSpecificDescriptor BrowserPrefix Ident
  | KnownDescriptor KnownDescriptor
  deriving (Eq, Show, Ord, Generic)

data KnownDescriptor
  = AccentColorT
  | AlignContentT
  | AlignItemsT
  | AlignmentBaselineT
  | AlignSelfT
  | AllT
  | AnchorNameT
  | AnchorScopeT
  | AnimationCompositionT
  | AnimationDelayT
  | AnimationDirectionT
  | AnimationDurationT
  | AnimationFillModeT
  | AnimationIterationCountT
  | AnimationNameT
  | AnimationPlayStateT
  | AnimationRangeEndT
  | AnimationRangeStartT
  | AnimationRangeT
  | AnimationT
  | AnimationTimelineT
  | AnimationTimingFunctionT
  | AnyHoverT
  | AnyPointerT
  | AppearanceT
  | AspectRatioT
  | BackdropFilterT
  | BackfaceVisibilityT
  | BackgroundAttachmentT
  | BackgroundBlendModeT
  | BackgroundClipT
  | BackgroundColorT
  | BackgroundImageT
  | BackgroundOriginT
  | BackgroundPositionT
  | BackgroundPositionXT
  | BackgroundPositionYT
  | BackgroundRepeatT
  | BackgroundRepeatXT
  | BackgroundRepeatYT
  | BackgroundSizeT
  | BackgroundT
  | BaselineShiftT
  | BaselineSourceT
  | BasePaletteT
  | BlockSizeT
  | BorderBlockColorT
  | BorderBlockEndColorT
  | BorderBlockEndStyleT
  | BorderBlockEndT
  | BorderBlockEndWidthT
  | BorderBlockStartColorT
  | BorderBlockStartStyleT
  | BorderBlockStartT
  | BorderBlockStartWidthT
  | BorderBlockStyleT
  | BorderBlockT
  | BorderBlockWidthT
  | BorderBottomColorT
  | BorderBottomLeftRadiusT
  | BorderBottomRightRadiusT
  | BorderBottomStyleT
  | BorderBottomT
  | BorderBottomWidthT
  | BorderCollapseT
  | BorderColorT
  | BorderEndEndRadiusT
  | BorderEndStartRadiusT
  | BorderImageOutsetT
  | BorderImageRepeatT
  | BorderImageSliceT
  | BorderImageSourceT
  | BorderImageT
  | BorderImageWidthT
  | BorderInlineColorT
  | BorderInlineEndColorT
  | BorderInlineEndStyleT
  | BorderInlineEndT
  | BorderInlineEndWidthT
  | BorderInlineStartColorT
  | BorderInlineStartStyleT
  | BorderInlineStartT
  | BorderInlineStartWidthT
  | BorderInlineStyleT
  | BorderInlineT
  | BorderInlineWidthT
  | BorderLeftColorT
  | BorderLeftStyleT
  | BorderLeftT
  | BorderLeftWidthT
  | BorderRadiusT
  | BorderRightColorT
  | BorderRightStyleT
  | BorderRightT
  | BorderRightWidthT
  | BorderSpacingT
  | BorderStartEndRadiusT
  | BorderStartStartRadiusT
  | BorderStyleT
  | BorderT
  | BorderTopColorT
  | BorderTopLeftRadiusT
  | BorderTopRightRadiusT
  | BorderTopStyleT
  | BorderTopT
  | BorderTopWidthT
  | BorderWidthT
  | BottomT
  | BoxAlignT
  | BoxDecorationBreakT
  | BoxDirectionT
  | BoxFlexGroupT
  | BoxFlexT
  | BoxLinesT
  | BoxOrdinalGroupT
  | BoxOrientT
  | BoxPackT
  | BoxShadowT
  | BoxSizingT
  | BreakAfterT
  | BreakBeforeT
  | BreakInsideT
  | CaptionSideT
  | CaretAnimationT
  | CaretColorT
  | CaretShapeT
  | CaretT
  | ClearT
  | ClipPathT
  | ClipRuleT
  | ClipT
  | ColorAdjustT
  | ColorGamutT
  | ColorIndexT
  | ColorInterpolationFiltersT
  | ColorInterpolationT
  | ColorSchemeT
  | ColorT
  | ColumnCountT
  | ColumnFillT
  | ColumnGapT
  | ColumnHeightT
  | ColumnRuleColorT
  | ColumnRuleStyleT
  | ColumnRuleT
  | ColumnRuleWidthT
  | ColumnSpanT
  | ColumnsT
  | ColumnWidthT
  | ColumnWrapT
  | ContainerD
  | ContainerNameT
  | ContainerTypeT
  | ContainIntrinsicBlockSizeT
  | ContainIntrinsicHeightT
  | ContainIntrinsicInlineSizeT
  | ContainIntrinsicSizeT
  | ContainIntrinsicWidthT
  | ContainT
  | ContentT
  | ContentVisibilityT
  | CornerBlockEndShapeT
  | CornerBlockStartShapeT
  | CornerBottomLeftShapeT
  | CornerBottomRightShapeT
  | CornerBottomShapeT
  | CornerEndEndShapeT
  | CornerEndStartShapeT
  | CornerInlineEndShapeT
  | CornerInlineStartShapeT
  | CornerLeftShapeT
  | CornerRightShapeT
  | CornerShapeT
  | CornerStartEndShapeT
  | CornerStartStartShapeT
  | CornerTopLeftShapeT
  | CornerTopRightShapeT
  | CornerTopShapeT
  | CounterIncrementT
  | CounterResetT
  | CounterSetT
  | CursorT
  | CxT
  | CyT
  | DeviceAspectRatioT
  | DeviceHeightT
  | DevicePostureT
  | DeviceWidthT
  | DirectionT
  | DisplayModeT
  | DisplayT
  | DominantBaselineT
  | DT
  | DynamicRangeLimitT
  | DynamicRangeT
  | EmptyCellsT
  | FieldSizingT
  | FillOpacityT
  | FillRuleT
  | FillT
  | FilterT
  | FlexBasisT
  | FlexDirectionT
  | FlexFlowT
  | FlexGrowT
  | FlexShrinkT
  | FlexT
  | FlexWrapT
  | FloatT
  | FloodColorT
  | FloodOpacityT
  | FontDisplayT
  | FontFamilyT
  | FontFeatureSettingsT
  | FontKerningT
  | FontLanguageOverrideT
  | FontOpticalSizingT
  | FontPaletteT
  | FontSizeAdjustT
  | FontSizeT
  | FontSmoothT
  | FontStretchT
  | FontStyleT
  | FontSynthesisPositionT
  | FontSynthesisSmallCapsT
  | FontSynthesisStyleT
  | FontSynthesisT
  | FontSynthesisWeightT
  | FontT
  | FontVariantAlternatesT
  | FontVariantCapsT
  | FontVariantEastAsianT
  | FontVariantEmojiT
  | FontVariantLigaturesT
  | FontVariantNumericT
  | FontVariantPositionT
  | FontVariantT
  | FontVariationSettingsT
  | FontWeightT
  | FontWidthT
  | ForcedColorAdjustT
  | ForcedColorsT
  | GapT
  | GridAreaT
  | GridAutoColumnsT
  | GridAutoFlowT
  | GridAutoRowsT
  | GridColumnEndT
  | GridColumnStartT
  | GridColumnT
  | GridGapT
  | GridRowEndT
  | GridRowStartT
  | GridRowT
  | GridT
  | GridTemplateAreasT
  | GridTemplateColumnsT
  | GridTemplateRowsT
  | GridTemplateT
  | HangingPunctuationT
  | HeightT
  | HorizontalViewportSegmentsT
  | HoverT
  | HyphenateCharacterT
  | HyphenateLimitCharsT
  | HyphensT
  | ImageOrientationT
  | ImageRenderingT
  | ImageResolutionT
  | InheritsT
  | InitialLetterT
  | InitialValueT
  | InlineSizeT
  | InsetBlockEndT
  | InsetBlockStartT
  | InsetBlockT
  | InsetInlineEndT
  | InsetInlineStartT
  | InsetInlineT
  | InsetT
  | InteractivityT
  | InterestDelayEndT
  | InterestDelayStartT
  | InterestDelayT
  | InterpolateSizeT
  | InvertedColorsT
  | IsolationT
  | JustifyContentT
  | JustifyItemsT
  | JustifySelfT
  | LeftT
  | LetterSpacingT
  | LightingColorT
  | LineBreakT
  | LineClampT
  | LineHeightStepT
  | LineHeightT
  | ListStyleImageT
  | ListStylePositionT
  | ListStyleT
  | ListStyleTypeT
  | MarginBlockEndT
  | MarginBlockStartT
  | MarginBlockT
  | MarginBottomT
  | MarginInlineEndT
  | MarginInlineStartT
  | MarginInlineT
  | MarginLeftT
  | MarginRightT
  | MarginT
  | MarginTopT
  | MarginTrimT
  | MarkerEndT
  | MarkerMidT
  | MarkerStartT
  | MarkerT
  | MaskBorderModeT
  | MaskBorderOutsetT
  | MaskBorderRepeatT
  | MaskBorderSliceT
  | MaskBorderSourceT
  | MaskBorderT
  | MaskBorderWidthT
  | MaskClipT
  | MaskCompositeT
  | MaskImageT
  | MaskModeT
  | MaskOriginT
  | MaskPositionT
  | MaskRepeatT
  | MaskSizeT
  | MaskT
  | MaskTypeT
  | MathDepthT
  | MathShiftT
  | MathStyleT
  | MaxBlockSizeT
  | MaxHeightT
  | MaxInlineSizeT
  | MaxWidthT
  | MinBlockSizeT
  | MinHeightT
  | MinInlineSizeT
  | MinWidthT
  | MixBlendModeT
  | MonochromeT
  | ObjectFitT
  | ObjectPositionT
  | ObjectViewBoxT
  | OffsetAnchorT
  | OffsetDistanceT
  | OffsetPathT
  | OffsetPositionT
  | OffsetRotateT
  | OffsetT
  | OpacityT
  | OrderT
  | OrientationT
  | OrphansT
  | OutlineColorT
  | OutlineOffsetT
  | OutlineStyleT
  | OutlineT
  | OutlineWidthT
  | OverflowAnchorT
  | OverflowBlockT
  | OverflowClipMarginT
  | OverflowInlineT
  | OverflowT
  | OverflowWrapT
  | OverflowXT
  | OverflowYT
  | OverlayT
  | OverrideColorsT
  | OverscrollBehaviorBlockT
  | OverscrollBehaviorInlineT
  | OverscrollBehaviorT
  | OverscrollBehaviorXT
  | OverscrollBehaviorYT
  | PaddingBlockEndT
  | PaddingBlockStartT
  | PaddingBlockT
  | PaddingBottomT
  | PaddingInlineEndT
  | PaddingInlineStartT
  | PaddingInlineT
  | PaddingLeftT
  | PaddingRightT
  | PaddingT
  | PaddingTopT
  | PageBreakAfterT
  | PageBreakBeforeT
  | PageBreakInsideT
  | PageD
  | PaintOrderT
  | PerspectiveOriginT
  | PerspectiveT
  | PlaceContentT
  | PlaceItemsT
  | PlaceSelfT
  | PointerEventsT
  | PointerT
  | PositionAnchorT
  | PositionAreaT
  | PositionT
  | PositionTryD
  | PositionTryFallbacksT
  | PositionTryOrderT
  | PositionVisibilityT
  | PrefersColorSchemeT
  | PrefersContrastT
  | PrefersReducedDataT
  | PrefersReducedMotionT
  | PrefersReducedTransparencyT
  | PrintColorAdjustT
  | QuotesT
  | ReadingFlowT
  | ReadingOrderT
  | ResizeT
  | ResolutionT
  | ResultT
  | RightT
  | RotateT
  | RowGapT
  | RT
  | RubyAlignT
  | RubyOverhangT
  | RubyPositionT
  | RxT
  | RyT
  | ScaleT
  | ScanT
  | ScriptingT
  | ScrollbarColorT
  | ScrollbarGutterT
  | ScrollbarWidthT
  | ScrollBehaviorT
  | ScrollInitialTargetT
  | ScrollMarginBlockEndT
  | ScrollMarginBlockStartT
  | ScrollMarginBlockT
  | ScrollMarginBottomT
  | ScrollMarginInlineEndT
  | ScrollMarginInlineStartT
  | ScrollMarginInlineT
  | ScrollMarginLeftT
  | ScrollMarginRightT
  | ScrollMarginT
  | ScrollMarginTopT
  | ScrollMarkerGroupT
  | ScrollPaddingBlockEndT
  | ScrollPaddingBlockStartT
  | ScrollPaddingBlockT
  | ScrollPaddingBottomT
  | ScrollPaddingInlineEndT
  | ScrollPaddingInlineStartT
  | ScrollPaddingInlineT
  | ScrollPaddingLeftT
  | ScrollPaddingRightT
  | ScrollPaddingT
  | ScrollPaddingTopT
  | ScrollSnapAlignT
  | ScrollSnapStopT
  | ScrollSnapTypeT
  | ScrollTargetGroupT
  | ScrollTimelineAxisT
  | ScrollTimelineNameT
  | ScrollTimelineT
  | ShapeImageThresholdT
  | ShapeMarginT
  | ShapeOutsideT
  | ShapeRenderingT
  | ShapeT
  | SpeakAsT
  | SrcT
  | StopColorT
  | StopOpacityT
  | StrokeDasharrayT
  | StrokeDashoffsetT
  | StrokeLinecapT
  | StrokeLinejoinT
  | StrokeMiterlimitT
  | StrokeOpacityT
  | StrokeT
  | StrokeWidthT
  | SyntaxT
  | TableLayoutT
  | TabSizeT
  | TextAlignLastT
  | TextAlignT
  | TextAnchorT
  | TextAutospaceT
  | TextBoxEdgeT
  | TextBoxT
  | TextBoxTrimT
  | TextCombineUprightT
  | TextDecorationColorT
  | TextDecorationInsetT
  | TextDecorationLineT
  | TextDecorationSkipInkT
  | TextDecorationSkipT
  | TextDecorationStyleT
  | TextDecorationT
  | TextDecorationThicknessT
  | TextEmphasisColorT
  | TextEmphasisPositionT
  | TextEmphasisStyleT
  | TextEmphasisT
  | TextIndentT
  | TextJustifyT
  | TextOrientationT
  | TextOverflowT
  | TextRenderingT
  | TextShadowT
  | TextSizeAdjustT
  | TextSpacingTrimT
  | TextTransformT
  | TextUnderlineOffsetT
  | TextUnderlinePositionT
  | TextWrapModeT
  | TextWrapStyleT
  | TextWrapT
  | TimelineScopeT
  | TopT
  | TouchActionT
  | TransformBoxT
  | TransformOriginT
  | TransformStyleT
  | TransformT
  | TransitionBehaviorT
  | TransitionDelayT
  | TransitionDurationT
  | TransitionPropertyT
  | TransitionT
  | TransitionTimingFunctionT
  | TranslateT
  | UnicodeBidiT
  | UnicodeRangeT
  | UpdateT
  | UserModifyT
  | UserSelectT
  | VectorEffectT
  | VerticalAlignT
  | VerticalViewportSegmentsT
  | VideoDynamicRangeT
  | ViewTimelineAxisT
  | ViewTimelineInsetT
  | ViewTimelineNameT
  | ViewTimelineT
  | ViewTransitionClassT
  | ViewTransitionNameT
  | VisibilityT
  | WhiteSpaceCollapseT
  | WhiteSpaceT
  | WidowsT
  | WidthT
  | WillChangeT
  | WordBreakT
  | WordSpacingT
  | WordWrapT
  | WritingModeT
  | XT
  | YT
  | ZIndexT
  | ZoomT
  deriving (Eq, Show, Ord, Bounded, Enum, Generic)

instance CssShow Descriptor where
  toCssText = \case
    CustomDescriptor i              -> "--" <> toCssText i <> ":"
    BrowserSpecificDescriptor bp i  -> toCssText bp <> toCssText i <> ":"
    KnownDescriptor kd              -> toCssText kd <> ":"

knownDescriptorMap :: HM.HashMap Text KnownDescriptor
knownDescriptorMap = mkDecodingMap

instance CssShow KnownDescriptor where
  toCssText = \case
    AccentColorT                    -> "accent-color"
    AlignContentT                   -> "align-content"
    AlignItemsT                     -> "align-items"
    AlignmentBaselineT              -> "alignment-baseline"
    AlignSelfT                      -> "align-self"
    AllT                            -> "all"
    AnchorNameT                     -> "anchor-name"
    AnchorScopeT                    -> "anchor-scope"
    AnimationCompositionT           -> "animation-composition"
    AnimationDelayT                 -> "animation-delay"
    AnimationDirectionT             -> "animation-direction"
    AnimationDurationT              -> "animation-duration"
    AnimationFillModeT              -> "animation-fill-mode"
    AnimationIterationCountT        -> "animation-iteration-count"
    AnimationNameT                  -> "animation-name"
    AnimationPlayStateT             -> "animation-play-state"
    AnimationRangeEndT              -> "animation-range-end"
    AnimationRangeStartT            -> "animation-range-start"
    AnimationRangeT                 -> "animation-range"
    AnimationT                      -> "animation"
    AnimationTimelineT              -> "animation-timeline"
    AnimationTimingFunctionT        -> "animation-timing-function"
    AnyHoverT                       -> "any-hover"
    AnyPointerT                     -> "any-pointer"
    AppearanceT                     -> "appearance"
    AspectRatioT                    -> "aspect-ratio"
    BackdropFilterT                 -> "backdrop-filter"
    BackfaceVisibilityT             -> "backface-visibility"
    BackgroundAttachmentT           -> "background-attachment"
    BackgroundBlendModeT            -> "background-blend-mode"
    BackgroundClipT                 -> "background-clip"
    BackgroundColorT                -> "background-color"
    BackgroundImageT                -> "background-image"
    BackgroundOriginT               -> "background-origin"
    BackgroundPositionT             -> "background-position"
    BackgroundPositionXT            -> "background-position-x"
    BackgroundPositionYT            -> "background-position-y"
    BackgroundRepeatT               -> "background-repeat"
    BackgroundRepeatXT              -> "background-repeat-x"
    BackgroundRepeatYT              -> "background-repeat-y"
    BackgroundSizeT                 -> "background-size"
    BackgroundT                     -> "background"
    BaselineShiftT                  -> "baseline-shift"
    BaselineSourceT                 -> "baseline-source"
    BasePaletteT                    -> "base-palette"
    BlockSizeT                      -> "block-size"
    BorderBlockColorT               -> "border-block-color"
    BorderBlockEndColorT            -> "border-block-end-color"
    BorderBlockEndStyleT            -> "border-block-end-style"
    BorderBlockEndT                 -> "border-block-end"
    BorderBlockEndWidthT            -> "border-block-end-width"
    BorderBlockStartColorT          -> "border-block-start-color"
    BorderBlockStartStyleT          -> "border-block-start-style"
    BorderBlockStartT               -> "border-block-start"
    BorderBlockStartWidthT          -> "border-block-start-width"
    BorderBlockStyleT               -> "border-block-style"
    BorderBlockT                    -> "border-block"
    BorderBlockWidthT               -> "border-block-width"
    BorderBottomColorT              -> "border-bottom-color"
    BorderBottomLeftRadiusT         -> "border-bottom-left-radius"
    BorderBottomRightRadiusT        -> "border-bottom-right-radius"
    BorderBottomStyleT              -> "border-bottom-style"
    BorderBottomT                   -> "border-bottom"
    BorderBottomWidthT              -> "border-bottom-width"
    BorderCollapseT                 -> "border-collapse"
    BorderColorT                    -> "border-color"
    BorderEndEndRadiusT             -> "border-end-end-radius"
    BorderEndStartRadiusT           -> "border-end-start-radius"
    BorderImageOutsetT              -> "border-image-outset"
    BorderImageRepeatT              -> "border-image-repeat"
    BorderImageSliceT               -> "border-image-slice"
    BorderImageSourceT              -> "border-image-source"
    BorderImageT                    -> "border-image"
    BorderImageWidthT               -> "border-image-width"
    BorderInlineColorT              -> "border-inline-color"
    BorderInlineEndColorT           -> "border-inline-end-color"
    BorderInlineEndStyleT           -> "border-inline-end-style"
    BorderInlineEndT                -> "border-inline-end"
    BorderInlineEndWidthT           -> "border-inline-end-width"
    BorderInlineStartColorT         -> "border-inline-start-color"
    BorderInlineStartStyleT         -> "border-inline-start-style"
    BorderInlineStartT              -> "border-inline-start"
    BorderInlineStartWidthT         -> "border-inline-start-width"
    BorderInlineStyleT              -> "border-inline-style"
    BorderInlineT                   -> "border-inline"
    BorderInlineWidthT              -> "border-inline-width"
    BorderLeftColorT                -> "border-left-color"
    BorderLeftStyleT                -> "border-left-style"
    BorderLeftT                     -> "border-left"
    BorderLeftWidthT                -> "border-left-width"
    BorderRadiusT                   -> "border-radius"
    BorderRightColorT               -> "border-right-color"
    BorderRightStyleT               -> "border-right-style"
    BorderRightT                    -> "border-right"
    BorderRightWidthT               -> "border-right-width"
    BorderSpacingT                  -> "border-spacing"
    BorderStartEndRadiusT           -> "border-start-end-radius"
    BorderStartStartRadiusT         -> "border-start-start-radius"
    BorderStyleT                    -> "border-style"
    BorderT                         -> "border"
    BorderTopColorT                 -> "border-top-color"
    BorderTopLeftRadiusT            -> "border-top-left-radius"
    BorderTopRightRadiusT           -> "border-top-right-radius"
    BorderTopStyleT                 -> "border-top-style"
    BorderTopT                      -> "border-top"
    BorderTopWidthT                 -> "border-top-width"
    BorderWidthT                    -> "border-width"
    BottomT                         -> "bottom"
    BoxAlignT                       -> "box-align"
    BoxDecorationBreakT             -> "box-decoration-break"
    BoxDirectionT                   -> "box-direction"
    BoxFlexGroupT                   -> "box-flex-group"
    BoxFlexT                        -> "box-flex"
    BoxLinesT                       -> "box-lines"
    BoxOrdinalGroupT                -> "box-ordinal-group"
    BoxOrientT                      -> "box-orient"
    BoxPackT                        -> "box-pack"
    BoxShadowT                      -> "box-shadow"
    BoxSizingT                      -> "box-sizing"
    BreakAfterT                     -> "break-after"
    BreakBeforeT                    -> "break-before"
    BreakInsideT                    -> "break-inside"
    CaptionSideT                    -> "caption-side"
    CaretAnimationT                 -> "caret-animation"
    CaretColorT                     -> "caret-color"
    CaretShapeT                     -> "caret-shape"
    CaretT                          -> "caret"
    ClearT                          -> "clear"
    ClipPathT                       -> "clip-path"
    ClipRuleT                       -> "clip-rule"
    ClipT                           -> "clip"
    ColorAdjustT                    -> "color-adjust"
    ColorGamutT                     -> "color-gamut"
    ColorIndexT                     -> "color-index"
    ColorInterpolationFiltersT      -> "color-interpolation-filters"
    ColorInterpolationT             -> "color-interpolation"
    ColorSchemeT                    -> "color-scheme"
    ColorT                          -> "color"
    ColumnCountT                    -> "column-count"
    ColumnFillT                     -> "column-fill"
    ColumnGapT                      -> "column-gap"
    ColumnHeightT                   -> "column-height"
    ColumnRuleColorT                -> "column-rule-color"
    ColumnRuleStyleT                -> "column-rule-style"
    ColumnRuleT                     -> "column-rule"
    ColumnRuleWidthT                -> "column-rule-width"
    ColumnSpanT                     -> "column-span"
    ColumnsT                        -> "columns"
    ColumnWidthT                    -> "column-width"
    ColumnWrapT                     -> "column-wrap"
    ContainerD                      -> "container"
    ContainerNameT                  -> "container-name"
    ContainerTypeT                  -> "container-type"
    ContainIntrinsicBlockSizeT      -> "contain-intrinsic-block-size"
    ContainIntrinsicHeightT         -> "contain-intrinsic-height"
    ContainIntrinsicInlineSizeT     -> "contain-intrinsic-inline-size"
    ContainIntrinsicSizeT           -> "contain-intrinsic-size"
    ContainIntrinsicWidthT          -> "contain-intrinsic-width"
    ContainT                        -> "contain"
    ContentT                        -> "content"
    ContentVisibilityT              -> "content-visibility"
    CornerBlockEndShapeT            -> "corner-block-end-shape"
    CornerBlockStartShapeT          -> "corner-block-start-shape"
    CornerBottomLeftShapeT          -> "corner-bottom-left-shape"
    CornerBottomRightShapeT         -> "corner-bottom-right-shape"
    CornerBottomShapeT              -> "corner-bottom-shape"
    CornerEndEndShapeT              -> "corner-end-end-shape"
    CornerEndStartShapeT            -> "corner-end-start-shape"
    CornerInlineEndShapeT           -> "corner-inline-end-shape"
    CornerInlineStartShapeT         -> "corner-inline-start-shape"
    CornerLeftShapeT                -> "corner-left-shape"
    CornerRightShapeT               -> "corner-right-shape"
    CornerShapeT                    -> "corner-shape"
    CornerStartEndShapeT            -> "corner-start-end-shape"
    CornerStartStartShapeT          -> "corner-start-start-shape"
    CornerTopLeftShapeT             -> "corner-top-left-shape"
    CornerTopRightShapeT            -> "corner-top-right-shape"
    CornerTopShapeT                 -> "corner-top-shape"
    CounterIncrementT               -> "counter-increment"
    CounterResetT                   -> "counter-reset"
    CounterSetT                     -> "counter-set"
    CursorT                         -> "cursor"
    CxT                             -> "cx"
    CyT                             -> "cy"
    DeviceAspectRatioT              -> "device-aspect-ratio"
    DeviceHeightT                   -> "device-height"
    DevicePostureT                  -> "device-posture"
    DeviceWidthT                    -> "device-width"
    DirectionT                      -> "direction"
    DisplayModeT                    -> "display-mode"
    DisplayT                        -> "display"
    DominantBaselineT               -> "dominant-baseline"
    DT                              -> "d"
    DynamicRangeLimitT              -> "dynamic-range-limit"
    DynamicRangeT                   -> "dynamic-range"
    EmptyCellsT                     -> "empty-cells"
    FieldSizingT                    -> "field-sizing"
    FillOpacityT                    -> "fill-opacity"
    FillRuleT                       -> "fill-rule"
    FillT                           -> "fill"
    FilterT                         -> "filter"
    FlexBasisT                      -> "flex-basis"
    FlexDirectionT                  -> "flex-direction"
    FlexFlowT                       -> "flex-flow"
    FlexGrowT                       -> "flex-grow"
    FlexShrinkT                     -> "flex-shrink"
    FlexT                           -> "flex"
    FlexWrapT                       -> "flex-wrap"
    FloatT                          -> "float"
    FloodColorT                     -> "flood-color"
    FloodOpacityT                   -> "flood-opacity"
    FontDisplayT                    -> "font-display"
    FontFamilyT                     -> "font-family"
    FontFeatureSettingsT            -> "font-feature-settings"
    FontKerningT                    -> "font-kerning"
    FontLanguageOverrideT           -> "font-language-override"
    FontOpticalSizingT              -> "font-optical-sizing"
    FontPaletteT                    -> "font-palette"
    FontSizeAdjustT                 -> "font-size-adjust"
    FontSizeT                       -> "font-size"
    FontSmoothT                     -> "font-smooth"
    FontStretchT                    -> "font-stretch"
    FontStyleT                      -> "font-style"
    FontSynthesisPositionT          -> "font-synthesis-position"
    FontSynthesisSmallCapsT         -> "font-synthesis-small-caps"
    FontSynthesisStyleT             -> "font-synthesis-style"
    FontSynthesisT                  -> "font-synthesis"
    FontSynthesisWeightT            -> "font-synthesis-weight"
    FontT                           -> "font"
    FontVariantAlternatesT          -> "font-variant-alternates"
    FontVariantCapsT                -> "font-variant-caps"
    FontVariantEastAsianT           -> "font-variant-east-asian"
    FontVariantEmojiT               -> "font-variant-emoji"
    FontVariantLigaturesT           -> "font-variant-ligatures"
    FontVariantNumericT             -> "font-variant-numeric"
    FontVariantPositionT            -> "font-variant-position"
    FontVariantT                    -> "font-variant"
    FontVariationSettingsT          -> "font-variation-settings"
    FontWeightT                     -> "font-weight"
    FontWidthT                      -> "font-width"
    ForcedColorAdjustT              -> "forced-color-adjust"
    ForcedColorsT                   -> "forced-colors"
    GapT                            -> "gap"
    GridAreaT                       -> "grid-area"
    GridAutoColumnsT                -> "grid-auto-columns"
    GridAutoFlowT                   -> "grid-auto-flow"
    GridAutoRowsT                   -> "grid-auto-rows"
    GridColumnEndT                  -> "grid-column-end"
    GridColumnStartT                -> "grid-column-start"
    GridColumnT                     -> "grid-column"
    GridGapT                        -> "grid-gap"
    GridRowEndT                     -> "grid-row-end"
    GridRowStartT                   -> "grid-row-start"
    GridRowT                        -> "grid-row"
    GridT                           -> "grid"
    GridTemplateAreasT              -> "grid-template-areas"
    GridTemplateColumnsT            -> "grid-template-columns"
    GridTemplateRowsT               -> "grid-template-rows"
    GridTemplateT                   -> "grid-template"
    HangingPunctuationT             -> "hanging-punctuation"
    HeightT                         -> "height"
    HorizontalViewportSegmentsT     -> "horizontal-viewport-segments"
    HoverT                          -> "hover"
    HyphenateCharacterT             -> "hyphenate-character"
    HyphenateLimitCharsT            -> "hyphenate-limit-chars"
    HyphensT                        -> "hyphens"
    ImageOrientationT               -> "image-orientation"
    ImageRenderingT                 -> "image-rendering"
    ImageResolutionT                -> "image-resolution"
    InheritsT                       -> "inherits"
    InitialLetterT                  -> "initial-letter"
    InitialValueT                   -> "initial-value"
    InlineSizeT                     -> "inline-size"
    InsetBlockEndT                  -> "inset-block-end"
    InsetBlockStartT                -> "inset-block-start"
    InsetBlockT                     -> "inset-block"
    InsetInlineEndT                 -> "inset-inline-end"
    InsetInlineStartT               -> "inset-inline-start"
    InsetInlineT                    -> "inset-inline"
    InsetT                          -> "inset"
    InteractivityT                  -> "interactivity"
    InterestDelayEndT               -> "interest-delay-end"
    InterestDelayStartT             -> "interest-delay-start"
    InterestDelayT                  -> "interest-delay"
    InterpolateSizeT                -> "interpolate-size"
    InvertedColorsT                 -> "inverted-colors"
    IsolationT                      -> "isolation"
    JustifyContentT                 -> "justify-content"
    JustifyItemsT                   -> "justify-items"
    JustifySelfT                    -> "justify-self"
    LeftT                           -> "left"
    LetterSpacingT                  -> "letter-spacing"
    LightingColorT                  -> "lighting-color"
    LineBreakT                      -> "line-break"
    LineClampT                      -> "line-clamp"
    LineHeightStepT                 -> "line-height-step"
    LineHeightT                     -> "line-height"
    ListStyleImageT                 -> "list-style-image"
    ListStylePositionT              -> "list-style-position"
    ListStyleT                      -> "list-style"
    ListStyleTypeT                  -> "list-style-type"
    MarginBlockEndT                 -> "margin-block-end"
    MarginBlockStartT               -> "margin-block-start"
    MarginBlockT                    -> "margin-block"
    MarginBottomT                   -> "margin-bottom"
    MarginInlineEndT                -> "margin-inline-end"
    MarginInlineStartT              -> "margin-inline-start"
    MarginInlineT                   -> "margin-inline"
    MarginLeftT                     -> "margin-left"
    MarginRightT                    -> "margin-right"
    MarginT                         -> "margin"
    MarginTopT                      -> "margin-top"
    MarginTrimT                     -> "margin-trim"
    MarkerEndT                      -> "marker-end"
    MarkerMidT                      -> "marker-mid"
    MarkerStartT                    -> "marker-start"
    MarkerT                         -> "marker"
    MaskBorderModeT                 -> "mask-border-mode"
    MaskBorderOutsetT               -> "mask-border-outset"
    MaskBorderRepeatT               -> "mask-border-repeat"
    MaskBorderSliceT                -> "mask-border-slice"
    MaskBorderSourceT               -> "mask-border-source"
    MaskBorderT                     -> "mask-border"
    MaskBorderWidthT                -> "mask-border-width"
    MaskClipT                       -> "mask-clip"
    MaskCompositeT                  -> "mask-composite"
    MaskImageT                      -> "mask-image"
    MaskModeT                       -> "mask-mode"
    MaskOriginT                     -> "mask-origin"
    MaskPositionT                   -> "mask-position"
    MaskRepeatT                     -> "mask-repeat"
    MaskSizeT                       -> "mask-size"
    MaskT                           -> "mask"
    MaskTypeT                       -> "mask-type"
    MathDepthT                      -> "math-depth"
    MathShiftT                      -> "math-shift"
    MathStyleT                      -> "math-style"
    MaxBlockSizeT                   -> "max-block-size"
    MaxHeightT                      -> "max-height"
    MaxInlineSizeT                  -> "max-inline-size"
    MaxWidthT                       -> "max-width"
    MinBlockSizeT                   -> "min-block-size"
    MinHeightT                      -> "min-height"
    MinInlineSizeT                  -> "min-inline-size"
    MinWidthT                       -> "min-width"
    MixBlendModeT                   -> "mix-blend-mode"
    MonochromeT                     -> "monochrome"
    ObjectFitT                      -> "object-fit"
    ObjectPositionT                 -> "object-position"
    ObjectViewBoxT                  -> "object-view-box"
    OffsetAnchorT                   -> "offset-anchor"
    OffsetDistanceT                 -> "offset-distance"
    OffsetPathT                     -> "offset-path"
    OffsetPositionT                 -> "offset-position"
    OffsetRotateT                   -> "offset-rotate"
    OffsetT                         -> "offset"
    OpacityT                        -> "opacity"
    OrderT                          -> "order"
    OrientationT                    -> "orientation"
    OrphansT                        -> "orphans"
    OutlineColorT                   -> "outline-color"
    OutlineOffsetT                  -> "outline-offset"
    OutlineStyleT                   -> "outline-style"
    OutlineT                        -> "outline"
    OutlineWidthT                   -> "outline-width"
    OverflowAnchorT                 -> "overflow-anchor"
    OverflowBlockT                  -> "overflow-block"
    OverflowClipMarginT             -> "overflow-clip-margin"
    OverflowInlineT                 -> "overflow-inline"
    OverflowT                       -> "overflow"
    OverflowWrapT                   -> "overflow-wrap"
    OverflowXT                      -> "overflow-x"
    OverflowYT                      -> "overflow-y"
    OverlayT                        -> "overlay"
    OverrideColorsT                 -> "override-colors"
    OverscrollBehaviorBlockT        -> "overscroll-behavior-block"
    OverscrollBehaviorInlineT       -> "overscroll-behavior-inline"
    OverscrollBehaviorT             -> "overscroll-behavior"
    OverscrollBehaviorXT            -> "overscroll-behavior-x"
    OverscrollBehaviorYT            -> "overscroll-behavior-y"
    PaddingBlockEndT                -> "padding-block-end"
    PaddingBlockStartT              -> "padding-block-start"
    PaddingBlockT                   -> "padding-block"
    PaddingBottomT                  -> "padding-bottom"
    PaddingInlineEndT               -> "padding-inline-end"
    PaddingInlineStartT             -> "padding-inline-start"
    PaddingInlineT                  -> "padding-inline"
    PaddingLeftT                    -> "padding-left"
    PaddingRightT                   -> "padding-right"
    PaddingT                        -> "padding"
    PaddingTopT                     -> "padding-top"
    PageBreakAfterT                 -> "page-break-after"
    PageBreakBeforeT                -> "page-break-before"
    PageBreakInsideT                -> "page-break-inside"
    PageD                           -> "page"
    PaintOrderT                     -> "paint-order"
    PerspectiveOriginT              -> "perspective-origin"
    PerspectiveT                    -> "perspective"
    PlaceContentT                   -> "place-content"
    PlaceItemsT                     -> "place-items"
    PlaceSelfT                      -> "place-self"
    PointerEventsT                  -> "pointer-events"
    PointerT                        -> "pointer"
    PositionAnchorT                 -> "position-anchor"
    PositionAreaT                   -> "position-area"
    PositionT                       -> "position"
    PositionTryD                    -> "position-try"
    PositionTryFallbacksT           -> "position-try-fallbacks"
    PositionTryOrderT               -> "position-try-order"
    PositionVisibilityT             -> "position-visibility"
    PrefersColorSchemeT             -> "prefers-color-scheme"
    PrefersContrastT                -> "prefers-contrast"
    PrefersReducedDataT             -> "prefers-reduced-data"
    PrefersReducedMotionT           -> "prefers-reduced-motion"
    PrefersReducedTransparencyT     -> "prefers-reduced-transparency"
    PrintColorAdjustT               -> "print-color-adjust"
    QuotesT                         -> "quotes"
    ReadingFlowT                    -> "reading-flow"
    ReadingOrderT                   -> "reading-order"
    ResizeT                         -> "resize"
    ResolutionT                     -> "resolution"
    ResultT                         -> "result"
    RightT                          -> "right"
    RotateT                         -> "rotate"
    RowGapT                         -> "row-gap"
    RT                              -> "r"
    RubyAlignT                      -> "ruby-align"
    RubyOverhangT                   -> "ruby-overhang"
    RubyPositionT                   -> "ruby-position"
    RxT                             -> "rx"
    RyT                             -> "ry"
    ScaleT                          -> "scale"
    ScanT                           -> "scan"
    ScriptingT                      -> "scripting"
    ScrollbarColorT                 -> "scrollbar-color"
    ScrollbarGutterT                -> "scrollbar-gutter"
    ScrollbarWidthT                 -> "scrollbar-width"
    ScrollBehaviorT                 -> "scroll-behavior"
    ScrollInitialTargetT            -> "scroll-initial-target"
    ScrollMarginBlockEndT           -> "scroll-margin-block-end"
    ScrollMarginBlockStartT         -> "scroll-margin-block-start"
    ScrollMarginBlockT              -> "scroll-margin-block"
    ScrollMarginBottomT             -> "scroll-margin-bottom"
    ScrollMarginInlineEndT          -> "scroll-margin-inline-end"
    ScrollMarginInlineStartT        -> "scroll-margin-inline-start"
    ScrollMarginInlineT             -> "scroll-margin-inline"
    ScrollMarginLeftT               -> "scroll-margin-left"
    ScrollMarginRightT              -> "scroll-margin-right"
    ScrollMarginT                   -> "scroll-margin"
    ScrollMarginTopT                -> "scroll-margin-top"
    ScrollMarkerGroupT              -> "scroll-marker-group"
    ScrollPaddingBlockEndT          -> "scroll-padding-block-end"
    ScrollPaddingBlockStartT        -> "scroll-padding-block-start"
    ScrollPaddingBlockT             -> "scroll-padding-block"
    ScrollPaddingBottomT            -> "scroll-padding-bottom"
    ScrollPaddingInlineEndT         -> "scroll-padding-inline-end"
    ScrollPaddingInlineStartT       -> "scroll-padding-inline-start"
    ScrollPaddingInlineT            -> "scroll-padding-inline"
    ScrollPaddingLeftT              -> "scroll-padding-left"
    ScrollPaddingRightT             -> "scroll-padding-right"
    ScrollPaddingT                  -> "scroll-padding"
    ScrollPaddingTopT               -> "scroll-padding-top"
    ScrollSnapAlignT                -> "scroll-snap-align"
    ScrollSnapStopT                 -> "scroll-snap-stop"
    ScrollSnapTypeT                 -> "scroll-snap-type"
    ScrollTargetGroupT              -> "scroll-target-group"
    ScrollTimelineAxisT             -> "scroll-timeline-axis"
    ScrollTimelineNameT             -> "scroll-timeline-name"
    ScrollTimelineT                 -> "scroll-timeline"
    ShapeImageThresholdT            -> "shape-image-threshold"
    ShapeMarginT                    -> "shape-margin"
    ShapeOutsideT                   -> "shape-outside"
    ShapeRenderingT                 -> "shape-rendering"
    ShapeT                          -> "shape"
    SpeakAsT                        -> "speak-as"
    SrcT                            -> "src"
    StopColorT                      -> "stop-color"
    StopOpacityT                    -> "stop-opacity"
    StrokeDasharrayT                -> "stroke-dasharray"
    StrokeDashoffsetT               -> "stroke-dashoffset"
    StrokeLinecapT                  -> "stroke-linecap"
    StrokeLinejoinT                 -> "stroke-linejoin"
    StrokeMiterlimitT               -> "stroke-miterlimit"
    StrokeOpacityT                  -> "stroke-opacity"
    StrokeT                         -> "stroke"
    StrokeWidthT                    -> "stroke-width"
    SyntaxT                         -> "syntax"
    TableLayoutT                    -> "table-layout"
    TabSizeT                        -> "tab-size"
    TextAlignLastT                  -> "text-align-last"
    TextAlignT                      -> "text-align"
    TextAnchorT                     -> "text-anchor"
    TextAutospaceT                  -> "text-autospace"
    TextBoxEdgeT                    -> "text-box-edge"
    TextBoxT                        -> "text-box"
    TextBoxTrimT                    -> "text-box-trim"
    TextCombineUprightT             -> "text-combine-upright"
    TextDecorationColorT            -> "text-decoration-color"
    TextDecorationInsetT            -> "text-decoration-inset"
    TextDecorationLineT             -> "text-decoration-line"
    TextDecorationSkipInkT          -> "text-decoration-skip-ink"
    TextDecorationSkipT             -> "text-decoration-skip"
    TextDecorationStyleT            -> "text-decoration-style"
    TextDecorationT                 -> "text-decoration"
    TextDecorationThicknessT        -> "text-decoration-thickness"
    TextEmphasisColorT              -> "text-emphasis-color"
    TextEmphasisPositionT           -> "text-emphasis-position"
    TextEmphasisStyleT              -> "text-emphasis-style"
    TextEmphasisT                   -> "text-emphasis"
    TextIndentT                     -> "text-indent"
    TextJustifyT                    -> "text-justify"
    TextOrientationT                -> "text-orientation"
    TextOverflowT                   -> "text-overflow"
    TextRenderingT                  -> "text-rendering"
    TextShadowT                     -> "text-shadow"
    TextSizeAdjustT                 -> "text-size-adjust"
    TextSpacingTrimT                -> "text-spacing-trim"
    TextTransformT                  -> "text-transform"
    TextUnderlineOffsetT            -> "text-underline-offset"
    TextUnderlinePositionT          -> "text-underline-position"
    TextWrapModeT                   -> "text-wrap-mode"
    TextWrapStyleT                  -> "text-wrap-style"
    TextWrapT                       -> "text-wrap"
    TimelineScopeT                  -> "timeline-scope"
    TopT                            -> "top"
    TouchActionT                    -> "touch-action"
    TransformBoxT                   -> "transform-box"
    TransformOriginT                -> "transform-origin"
    TransformStyleT                 -> "transform-style"
    TransformT                      -> "transform"
    TransitionBehaviorT             -> "transition-behavior"
    TransitionDelayT                -> "transition-delay"
    TransitionDurationT             -> "transition-duration"
    TransitionPropertyT             -> "transition-property"
    TransitionT                     -> "transition"
    TransitionTimingFunctionT       -> "transition-timing-function"
    TranslateT                      -> "translate"
    UnicodeBidiT                    -> "unicode-bidi"
    UnicodeRangeT                   -> "unicode-range"
    UpdateT                         -> "update"
    UserModifyT                     -> "user-modify"
    UserSelectT                     -> "user-select"
    VectorEffectT                   -> "vector-effect"
    VerticalAlignT                  -> "vertical-align"
    VerticalViewportSegmentsT       -> "vertical-viewport-segments"
    VideoDynamicRangeT              -> "video-dynamic-range"
    ViewTimelineAxisT               -> "view-timeline-axis"
    ViewTimelineInsetT              -> "view-timeline-inset"
    ViewTimelineNameT               -> "view-timeline-name"
    ViewTimelineT                   -> "view-timeline"
    ViewTransitionClassT            -> "view-transition-class"
    ViewTransitionNameT             -> "view-transition-name"
    VisibilityT                     -> "visibility"
    WhiteSpaceCollapseT             -> "white-space-collapse"
    WhiteSpaceT                     -> "white-space"
    WidowsT                         -> "widows"
    WidthT                          -> "width"
    WillChangeT                     -> "will-change"
    WordBreakT                      -> "word-break"
    WordSpacingT                    -> "word-spacing"
    WordWrapT                       -> "word-wrap"
    WritingModeT                    -> "writing-mode"
    XT                              -> "x"
    YT                              -> "y"
    ZIndexT                         -> "z-index"
    ZoomT                           -> "zoom"

toPropertyName :: Descriptor -> PropertyName
toPropertyName = \case
  CustomDescriptor i -> VarProp $ Var i
  bsd@BrowserSpecificDescriptor{} ->
    PropertyName . Ident . toStrict . dropEnd 1 $ toCssText bsd
  o -> PropertyName . Ident . toStrict . dropEnd 1 $ toCssText o
