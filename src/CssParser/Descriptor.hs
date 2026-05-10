module CssParser.Descriptor where

import CssParser.Ident ( BrowserPrefix, Ident (..), PropertyName(..), Var (..) )
import CssParser.Prelude
    ( ($), Eq, Ord, Show, Generic, Semigroup((<>)), (.) )
import CssParser.Show ( CssShow(..) )
import Data.Text.Lazy (dropEnd, toStrict)

data Descriptor
  = CustomDescriptor Ident
  | BrowserSpecificDescriptor BrowserPrefix Ident
  | AccentColorT
  | AlignContentT
  | AlignItemsT
  | AlignSelfT
  | AlignmentBaselineT
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
  | AnimationTimelineT
  | AnimationTimingFunctionT
  | AnimationT
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
  | BackgroundPositionXT
  | BackgroundPositionYT
  | BackgroundPositionT
  | BackgroundRepeatT
  | BackgroundRepeatXT
  | BackgroundRepeatYT
  | BackgroundSizeT
  | BackgroundT
  | BasePaletteT
  | BaselineShiftT
  | BaselineSourceT
  | BlockSizeT
  | BorderBlockColorT
  | BorderBlockEndColorT
  | BorderBlockEndStyleT
  | BorderBlockEndWidthT
  | BorderBlockEndT
  | BorderBlockStartColorT
  | BorderBlockStartStyleT
  | BorderBlockStartWidthT
  | BorderBlockStartT
  | BorderBlockStyleT
  | BorderBlockWidthT
  | BorderBlockT
  | BorderBottomColorT
  | BorderBottomLeftRadiusT
  | BorderBottomRightRadiusT
  | BorderBottomStyleT
  | BorderBottomWidthT
  | BorderBottomT
  | BorderCollapseT
  | BorderColorT
  | BorderEndEndRadiusT
  | BorderEndStartRadiusT
  | BorderImageOutsetT
  | BorderImageRepeatT
  | BorderImageSliceT
  | BorderImageSourceT
  | BorderImageWidthT
  | BorderImageT
  | BorderInlineColorT
  | BorderInlineEndColorT
  | BorderInlineEndStyleT
  | BorderInlineEndWidthT
  | BorderInlineEndT
  | BorderInlineStartColorT
  | BorderInlineStartStyleT
  | BorderInlineStartWidthT
  | BorderInlineStartT
  | BorderInlineStyleT
  | BorderInlineWidthT
  | BorderInlineT
  | BorderLeftColorT
  | BorderLeftStyleT
  | BorderLeftWidthT
  | BorderLeftT
  | BorderRadiusT
  | BorderRightColorT
  | BorderRightStyleT
  | BorderRightWidthT
  | BorderRightT
  | BorderSpacingT
  | BorderStartEndRadiusT
  | BorderStartStartRadiusT
  | BorderStyleT
  | BorderTopColorT
  | BorderTopLeftRadiusT
  | BorderTopRightRadiusT
  | BorderTopStyleT
  | BorderTopWidthT
  | BorderTopT
  | BorderWidthT
  | BorderT
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
  | ColumnRuleWidthT
  | ColumnRuleT
  | ColumnSpanT
  | ColumnWidthT
  | ColumnWrapT
  | ColumnsT
  | ContainIntrinsicBlockSizeT
  | ContainIntrinsicHeightT
  | ContainIntrinsicInlineSizeT
  | ContainIntrinsicSizeT
  | ContainIntrinsicWidthT
  | ContainT
  | ContainerNameT
  | ContainerTypeT
  | ContainerD
  | ContentVisibilityT
  | ContentT
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
  | DT
  | DirectionT
  | DisplayT
  | DominantBaselineT
  | DynamicRangeLimitT
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
  | FlexWrapT
  | FlexT
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
  | FontSynthesisWeightT
  | FontSynthesisT
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
  | FontT
  | ForcedColorAdjustT
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
  | GridTemplateAreasT
  | GridTemplateColumnsT
  | GridTemplateRowsT
  | GridTemplateT
  | GridT
  | HangingPunctuationT
  | HeightT
  | HyphenateCharacterT
  | HyphenateLimitCharsT
  | HyphensT
  | ImageOrientationT
  | ImageRenderingT
  | ImageResolutionT
  | InitialLetterT
  | InitialValueT
  | InheritsT
  | InlineSizeT
  | InsetBlockEndT
  | InsetBlockStartT
  | InsetBlockT
  | InsetInlineEndT
  | InsetInlineStartT
  | InsetInlineT
  | InsetT
  | InteractivityT
  | InterestDelayT
  | InterestDelayEndT
  | InterestDelayStartT
  | InterpolateSizeT
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
  | ListStyleTypeT
  | ListStyleT
  | MarginBlockEndT
  | MarginBlockStartT
  | MarginBlockT
  | MarginBottomT
  | MarginInlineEndT
  | MarginInlineStartT
  | MarginInlineT
  | MarginLeftT
  | MarginRightT
  | MarginTopT
  | MarginTrimT
  | MarginT
  | MarkerEndT
  | MarkerMidT
  | MarkerStartT
  | MarkerT
  | MaskBorderModeT
  | MaskBorderOutsetT
  | MaskBorderRepeatT
  | MaskBorderSliceT
  | MaskBorderSourceT
  | MaskBorderWidthT
  | MaskBorderT
  | MaskClipT
  | MaskCompositeT
  | MaskImageT
  | MaskModeT
  | MaskOriginT
  | MaskPositionT
  | MaskRepeatT
  | MaskSizeT
  | MaskTypeT
  | MaskT
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
  | OrphansT
  | OutlineColorT
  | OutlineOffsetT
  | OutlineStyleT
  | OutlineWidthT
  | OutlineT
  | OverflowAnchorT
  | OverflowBlockT
  | OverflowClipMarginT
  | OverflowInlineT
  | OverflowWrapT
  | OverflowXT
  | OverflowYT
  | OverflowT
  | OverlayT
  | OverrideColorsT
  | OverscrollBehaviorBlockT
  | OverscrollBehaviorInlineT
  | OverscrollBehaviorXT
  | OverscrollBehaviorYT
  | OverscrollBehaviorT
  | PaddingBlockEndT
  | PaddingBlockStartT
  | PaddingBlockT
  | PaddingBottomT
  | PaddingInlineEndT
  | PaddingInlineStartT
  | PaddingInlineT
  | PaddingLeftT
  | PaddingRightT
  | PaddingTopT
  | PaddingT
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
  | PositionAnchorT
  | PositionAreaT
  | PositionTryFallbacksT
  | PositionTryOrderT
  | PositionTryD
  | PositionVisibilityT
  | PositionT
  | PrintColorAdjustT
  | QuotesT
  | RT
  | ReadingFlowT
  | ReadingOrderT
  | ResizeT
  | ResultT
  | RightT
  | RotateT
  | RowGapT
  | RubyAlignT
  | RubyOverhangT
  | RubyPositionT
  | RxT
  | RyT
  | ScaleT
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
  | ScrollMarginTopT
  | ScrollMarginT
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
  | ScrollPaddingTopT
  | ScrollPaddingT
  | ScrollSnapAlignT
  | ScrollSnapStopT
  | ScrollSnapTypeT
  | ScrollTargetGroupT
  | ScrollTimelineAxisT
  | ScrollTimelineNameT
  | ScrollTimelineT
  | ScrollbarColorT
  | ScrollbarGutterT
  | ScrollbarWidthT
  | ShapeImageThresholdT
  | ShapeMarginT
  | ShapeOutsideT
  | ShapeRenderingT
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
  | StrokeWidthT
  | StrokeT
  | SyntaxT
  | TabSizeT
  | TableLayoutT
  | TextAlignLastT
  | TextAlignT
  | TextAnchorT
  | TextAutospaceT
  | TextBoxEdgeT
  | TextBoxTrimT
  | TextBoxT
  | TextCombineUprightT
  | TextDecorationColorT
  | TextDecorationInsetT
  | TextDecorationLineT
  | TextDecorationSkipInkT
  | TextDecorationSkipT
  | TextDecorationStyleT
  | TextDecorationThicknessT
  | TextDecorationT
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
  | TransitionTimingFunctionT
  | TransitionT
  | TranslateT
  | UnicodeBidiT
  | UnicodeRangeT
  | UserModifyT
  | UserSelectT
  | VectorEffectT
  | VerticalAlignT
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
  deriving (Eq, Show, Ord, Generic)

instance CssShow Descriptor where
  toCssText = \case
    CustomDescriptor i              -> "--" <> toCssText i <> ":"
    BrowserSpecificDescriptor bp i  -> toCssText bp <> toCssText i <> ":"
    AccentColorT                    -> "accent-color:"
    AlignContentT                   -> "align-content:"
    AlignItemsT                     -> "align-items:"
    AlignSelfT                      -> "align-self:"
    AlignmentBaselineT              -> "alignment-baseline:"
    AllT                            -> "all:"
    AnchorNameT                     -> "anchor-name:"
    AnchorScopeT                    -> "anchor-scope:"
    AnimationCompositionT           -> "animation-composition:"
    AnimationDelayT                 -> "animation-delay:"
    AnimationDirectionT             -> "animation-direction:"
    AnimationDurationT              -> "animation-duration:"
    AnimationFillModeT              -> "animation-fill-mode:"
    AnimationIterationCountT        -> "animation-iteration-count:"
    AnimationNameT                  -> "animation-name:"
    AnimationPlayStateT             -> "animation-play-state:"
    AnimationRangeEndT              -> "animation-range-end:"
    AnimationRangeStartT            -> "animation-range-start:"
    AnimationRangeT                 -> "animation-range:"
    AnimationTimelineT              -> "animation-timeline:"
    AnimationTimingFunctionT        -> "animation-timing-function:"
    AnimationT                      -> "animation:"
    AppearanceT                     -> "appearance:"
    AspectRatioT                    -> "aspect-ratio:"
    BackdropFilterT                 -> "backdrop-filter:"
    BackfaceVisibilityT             -> "backface-visibility:"
    BackgroundAttachmentT           -> "background-attachment:"
    BackgroundBlendModeT            -> "background-blend-mode:"
    BackgroundClipT                 -> "background-clip:"
    BackgroundColorT                -> "background-color:"
    BackgroundImageT                -> "background-image:"
    BackgroundOriginT               -> "background-origin:"
    BackgroundPositionXT            -> "background-position-x:"
    BackgroundPositionYT            -> "background-position-y:"
    BackgroundPositionT             -> "background-position:"
    BackgroundRepeatT               -> "background-repeat:"
    BackgroundRepeatXT              -> "background-repeat-x:"
    BackgroundRepeatYT              -> "background-repeat-y:"
    BackgroundSizeT                 -> "background-size:"
    BackgroundT                     -> "background:"
    BasePaletteT                    -> "base-palette:"
    BaselineShiftT                  -> "baseline-shift:"
    BaselineSourceT                 -> "baseline-source:"
    BlockSizeT                      -> "block-size:"
    BorderBlockColorT               -> "border-block-color:"
    BorderBlockEndColorT            -> "border-block-end-color:"
    BorderBlockEndStyleT            -> "border-block-end-style:"
    BorderBlockEndWidthT            -> "border-block-end-width:"
    BorderBlockEndT                 -> "border-block-end:"
    BorderBlockStartColorT          -> "border-block-start-color:"
    BorderBlockStartStyleT          -> "border-block-start-style:"
    BorderBlockStartWidthT          -> "border-block-start-width:"
    BorderBlockStartT               -> "border-block-start:"
    BorderBlockStyleT               -> "border-block-style:"
    BorderBlockWidthT               -> "border-block-width:"
    BorderBlockT                    -> "border-block:"
    BorderBottomColorT              -> "border-bottom-color:"
    BorderBottomLeftRadiusT         -> "border-bottom-left-radius:"
    BorderBottomRightRadiusT        -> "border-bottom-right-radius:"
    BorderBottomStyleT              -> "border-bottom-style:"
    BorderBottomWidthT              -> "border-bottom-width:"
    BorderBottomT                   -> "border-bottom:"
    BorderCollapseT                 -> "border-collapse:"
    BorderColorT                    -> "border-color:"
    BorderEndEndRadiusT             -> "border-end-end-radius:"
    BorderEndStartRadiusT           -> "border-end-start-radius:"
    BorderImageOutsetT              -> "border-image-outset:"
    BorderImageRepeatT              -> "border-image-repeat:"
    BorderImageSliceT               -> "border-image-slice:"
    BorderImageSourceT              -> "border-image-source:"
    BorderImageWidthT               -> "border-image-width:"
    BorderImageT                    -> "border-image:"
    BorderInlineColorT              -> "border-inline-color:"
    BorderInlineEndColorT           -> "border-inline-end-color:"
    BorderInlineEndStyleT           -> "border-inline-end-style:"
    BorderInlineEndWidthT           -> "border-inline-end-width:"
    BorderInlineEndT                -> "border-inline-end:"
    BorderInlineStartColorT         -> "border-inline-start-color:"
    BorderInlineStartStyleT         -> "border-inline-start-style:"
    BorderInlineStartWidthT         -> "border-inline-start-width:"
    BorderInlineStartT              -> "border-inline-start:"
    BorderInlineStyleT              -> "border-inline-style:"
    BorderInlineWidthT              -> "border-inline-width:"
    BorderInlineT                   -> "border-inline:"
    BorderLeftColorT                -> "border-left-color:"
    BorderLeftStyleT                -> "border-left-style:"
    BorderLeftWidthT                -> "border-left-width:"
    BorderLeftT                     -> "border-left:"
    BorderRadiusT                   -> "border-radius:"
    BorderRightColorT               -> "border-right-color:"
    BorderRightStyleT               -> "border-right-style:"
    BorderRightWidthT               -> "border-right-width:"
    BorderRightT                    -> "border-right:"
    BorderSpacingT                  -> "border-spacing:"
    BorderStartEndRadiusT           -> "border-start-end-radius:"
    BorderStartStartRadiusT         -> "border-start-start-radius:"
    BorderStyleT                    -> "border-style:"
    BorderTopColorT                 -> "border-top-color:"
    BorderTopLeftRadiusT            -> "border-top-left-radius:"
    BorderTopRightRadiusT           -> "border-top-right-radius:"
    BorderTopStyleT                 -> "border-top-style:"
    BorderTopWidthT                 -> "border-top-width:"
    BorderTopT                      -> "border-top:"
    BorderWidthT                    -> "border-width:"
    BorderT                         -> "border:"
    BottomT                         -> "bottom:"
    BoxAlignT                       -> "box-align:"
    BoxDecorationBreakT             -> "box-decoration-break:"
    BoxDirectionT                   -> "box-direction:"
    BoxFlexGroupT                   -> "box-flex-group:"
    BoxFlexT                        -> "box-flex:"
    BoxLinesT                       -> "box-lines:"
    BoxOrdinalGroupT                -> "box-ordinal-group:"
    BoxOrientT                      -> "box-orient:"
    BoxPackT                        -> "box-pack:"
    BoxShadowT                      -> "box-shadow:"
    BoxSizingT                      -> "box-sizing:"
    BreakAfterT                     -> "break-after:"
    BreakBeforeT                    -> "break-before:"
    BreakInsideT                    -> "break-inside:"
    CaptionSideT                    -> "caption-side:"
    CaretAnimationT                 -> "caret-animation:"
    CaretColorT                     -> "caret-color:"
    CaretShapeT                     -> "caret-shape:"
    CaretT                          -> "caret:"
    ClearT                          -> "clear:"
    ClipPathT                       -> "clip-path:"
    ClipRuleT                       -> "clip-rule:"
    ClipT                           -> "clip:"
    ColorAdjustT                    -> "color-adjust:"
    ColorInterpolationFiltersT      -> "color-interpolation-filters:"
    ColorInterpolationT             -> "color-interpolation:"
    ColorSchemeT                    -> "color-scheme:"
    ColorT                          -> "color:"
    ColumnCountT                    -> "column-count:"
    ColumnFillT                     -> "column-fill:"
    ColumnGapT                      -> "column-gap:"
    ColumnHeightT                   -> "column-height:"
    ColumnRuleColorT                -> "column-rule-color:"
    ColumnRuleStyleT                -> "column-rule-style:"
    ColumnRuleWidthT                -> "column-rule-width:"
    ColumnRuleT                     -> "column-rule:"
    ColumnSpanT                     -> "column-span:"
    ColumnWidthT                    -> "column-width:"
    ColumnWrapT                     -> "column-wrap:"
    ColumnsT                        -> "columns:"
    ContainIntrinsicBlockSizeT      -> "contain-intrinsic-block-size:"
    ContainIntrinsicHeightT         -> "contain-intrinsic-height:"
    ContainIntrinsicInlineSizeT     -> "contain-intrinsic-inline-size:"
    ContainIntrinsicSizeT           -> "contain-intrinsic-size:"
    ContainIntrinsicWidthT          -> "contain-intrinsic-width:"
    ContainT                        -> "contain:"
    ContainerNameT                  -> "container-name:"
    ContainerTypeT                  -> "container-type:"
    ContainerD                      -> "container:"
    ContentVisibilityT              -> "content-visibility:"
    ContentT                        -> "content:"
    CornerBlockEndShapeT            -> "corner-block-end-shape:"
    CornerBlockStartShapeT          -> "corner-block-start-shape:"
    CornerBottomLeftShapeT          -> "corner-bottom-left-shape:"
    CornerBottomRightShapeT         -> "corner-bottom-right-shape:"
    CornerBottomShapeT              -> "corner-bottom-shape:"
    CornerEndEndShapeT              -> "corner-end-end-shape:"
    CornerEndStartShapeT            -> "corner-end-start-shape:"
    CornerInlineEndShapeT           -> "corner-inline-end-shape:"
    CornerInlineStartShapeT         -> "corner-inline-start-shape:"
    CornerLeftShapeT                -> "corner-left-shape:"
    CornerRightShapeT               -> "corner-right-shape:"
    CornerShapeT                    -> "corner-shape:"
    CornerStartEndShapeT            -> "corner-start-end-shape:"
    CornerStartStartShapeT          -> "corner-start-start-shape:"
    CornerTopLeftShapeT             -> "corner-top-left-shape:"
    CornerTopRightShapeT            -> "corner-top-right-shape:"
    CornerTopShapeT                 -> "corner-top-shape:"
    CounterIncrementT               -> "counter-increment:"
    CounterResetT                   -> "counter-reset:"
    CounterSetT                     -> "counter-set:"
    CursorT                         -> "cursor:"
    CxT                             -> "cx:"
    CyT                             -> "cy:"
    DT                              -> "d:"
    DirectionT                      -> "direction:"
    DisplayT                        -> "display:"
    DominantBaselineT               -> "dominant-baseline:"
    DynamicRangeLimitT              -> "dynamic-range-limit:"
    EmptyCellsT                     -> "empty-cells:"
    FieldSizingT                    -> "field-sizing:"
    FillOpacityT                    -> "fill-opacity:"
    FillRuleT                       -> "fill-rule:"
    FillT                           -> "fill:"
    FilterT                         -> "filter:"
    FlexBasisT                      -> "flex-basis:"
    FlexDirectionT                  -> "flex-direction:"
    FlexFlowT                       -> "flex-flow:"
    FlexGrowT                       -> "flex-grow:"
    FlexShrinkT                     -> "flex-shrink:"
    FlexWrapT                       -> "flex-wrap:"
    FlexT                           -> "flex:"
    FloatT                          -> "float:"
    FloodColorT                     -> "flood-color:"
    FloodOpacityT                   -> "flood-opacity:"
    FontDisplayT                    -> "font-display:"
    FontFamilyT                     -> "font-family:"
    FontFeatureSettingsT            -> "font-feature-settings:"
    FontKerningT                    -> "font-kerning:"
    FontLanguageOverrideT           -> "font-language-override:"
    FontOpticalSizingT              -> "font-optical-sizing:"
    FontPaletteT                    -> "font-palette:"
    FontSizeAdjustT                 -> "font-size-adjust:"
    FontSizeT                       -> "font-size:"
    FontSmoothT                     -> "font-smooth:"
    FontStretchT                    -> "font-stretch:"
    FontStyleT                      -> "font-style:"
    FontSynthesisPositionT          -> "font-synthesis-position:"
    FontSynthesisSmallCapsT         -> "font-synthesis-small-caps:"
    FontSynthesisStyleT             -> "font-synthesis-style:"
    FontSynthesisWeightT            -> "font-synthesis-weight:"
    FontSynthesisT                  -> "font-synthesis:"
    FontVariantAlternatesT          -> "font-variant-alternates:"
    FontVariantCapsT                -> "font-variant-caps:"
    FontVariantEastAsianT           -> "font-variant-east-asian:"
    FontVariantEmojiT               -> "font-variant-emoji:"
    FontVariantLigaturesT           -> "font-variant-ligatures:"
    FontVariantNumericT             -> "font-variant-numeric:"
    FontVariantPositionT            -> "font-variant-position:"
    FontVariantT                    -> "font-variant:"
    FontVariationSettingsT          -> "font-variation-settings:"
    FontWeightT                     -> "font-weight:"
    FontWidthT                      -> "font-width:"
    FontT                           -> "font:"
    ForcedColorAdjustT              -> "forced-color-adjust:"
    GapT                            -> "gap:"
    GridAreaT                       -> "grid-area:"
    GridAutoColumnsT                -> "grid-auto-columns:"
    GridAutoFlowT                   -> "grid-auto-flow:"
    GridAutoRowsT                   -> "grid-auto-rows:"
    GridColumnEndT                  -> "grid-column-end:"
    GridColumnStartT                -> "grid-column-start:"
    GridColumnT                     -> "grid-column:"
    GridGapT                        -> "grid-gap:"
    GridRowEndT                     -> "grid-row-end:"
    GridRowStartT                   -> "grid-row-start:"
    GridRowT                        -> "grid-row:"
    GridTemplateAreasT              -> "grid-template-areas:"
    GridTemplateColumnsT            -> "grid-template-columns:"
    GridTemplateRowsT               -> "grid-template-rows:"
    GridTemplateT                   -> "grid-template:"
    GridT                           -> "grid:"
    HangingPunctuationT             -> "hanging-punctuation:"
    HeightT                         -> "height:"
    HyphenateCharacterT             -> "hyphenate-character:"
    HyphenateLimitCharsT            -> "hyphenate-limit-chars:"
    HyphensT                        -> "hyphens:"
    ImageOrientationT               -> "image-orientation:"
    ImageRenderingT                 -> "image-rendering:"
    ImageResolutionT                -> "image-resolution:"
    InheritsT                       -> "inherits:"
    InitialLetterT                  -> "initial-letter:"
    InitialValueT                   -> "initial-value:"
    InlineSizeT                     -> "inline-size:"
    InsetBlockEndT                  -> "inset-block-end:"
    InsetBlockStartT                -> "inset-block-start:"
    InsetBlockT                     -> "inset-block:"
    InsetInlineEndT                 -> "inset-inline-end:"
    InsetInlineStartT               -> "inset-inline-start:"
    InsetInlineT                    -> "inset-inline:"
    InsetT                          -> "inset:"
    InteractivityT                  -> "interactivity:"
    InterestDelayT                  -> "interest-delay:"
    InterestDelayEndT               -> "interest-delay-end:"
    InterestDelayStartT             -> "interest-delay-start:"
    InterpolateSizeT                -> "interpolate-size:"
    IsolationT                      -> "isolation:"
    JustifyContentT                 -> "justify-content:"
    JustifyItemsT                   -> "justify-items:"
    JustifySelfT                    -> "justify-self:"
    LeftT                           -> "left:"
    LetterSpacingT                  -> "letter-spacing:"
    LightingColorT                  -> "lighting-color:"
    LineBreakT                      -> "line-break:"
    LineClampT                      -> "line-clamp:"
    LineHeightStepT                 -> "line-height-step:"
    LineHeightT                     -> "line-height:"
    ListStyleImageT                 -> "list-style-image:"
    ListStylePositionT              -> "list-style-position:"
    ListStyleTypeT                  -> "list-style-type:"
    ListStyleT                      -> "list-style:"
    MarginBlockEndT                 -> "margin-block-end:"
    MarginBlockStartT               -> "margin-block-start:"
    MarginBlockT                    -> "margin-block:"
    MarginBottomT                   -> "margin-bottom:"
    MarginInlineEndT                -> "margin-inline-end:"
    MarginInlineStartT              -> "margin-inline-start:"
    MarginInlineT                   -> "margin-inline:"
    MarginLeftT                     -> "margin-left:"
    MarginRightT                    -> "margin-right:"
    MarginTopT                      -> "margin-top:"
    MarginTrimT                     -> "margin-trim:"
    MarginT                         -> "margin:"
    MarkerEndT                      -> "marker-end:"
    MarkerMidT                      -> "marker-mid:"
    MarkerStartT                    -> "marker-start:"
    MarkerT                         -> "marker:"
    MaskBorderModeT                 -> "mask-border-mode:"
    MaskBorderOutsetT               -> "mask-border-outset:"
    MaskBorderRepeatT               -> "mask-border-repeat:"
    MaskBorderSliceT                -> "mask-border-slice:"
    MaskBorderSourceT               -> "mask-border-source:"
    MaskBorderWidthT                -> "mask-border-width:"
    MaskBorderT                     -> "mask-border:"
    MaskClipT                       -> "mask-clip:"
    MaskCompositeT                  -> "mask-composite:"
    MaskImageT                      -> "mask-image:"
    MaskModeT                       -> "mask-mode:"
    MaskOriginT                     -> "mask-origin:"
    MaskPositionT                   -> "mask-position:"
    MaskRepeatT                     -> "mask-repeat:"
    MaskSizeT                       -> "mask-size:"
    MaskTypeT                       -> "mask-type:"
    MaskT                           -> "mask:"
    MathDepthT                      -> "math-depth:"
    MathShiftT                      -> "math-shift:"
    MathStyleT                      -> "math-style:"
    MaxBlockSizeT                   -> "max-block-size:"
    MaxHeightT                      -> "max-height:"
    MaxInlineSizeT                  -> "max-inline-size:"
    MaxWidthT                       -> "max-width:"
    MinBlockSizeT                   -> "min-block-size:"
    MinHeightT                      -> "min-height:"
    MinInlineSizeT                  -> "min-inline-size:"
    MinWidthT                       -> "min-width:"
    MixBlendModeT                   -> "mix-blend-mode:"
    ObjectFitT                      -> "object-fit:"
    ObjectPositionT                 -> "object-position:"
    ObjectViewBoxT                  -> "object-view-box:"
    OffsetAnchorT                   -> "offset-anchor:"
    OffsetDistanceT                 -> "offset-distance:"
    OffsetPathT                     -> "offset-path:"
    OffsetPositionT                 -> "offset-position:"
    OffsetRotateT                   -> "offset-rotate:"
    OffsetT                         -> "offset:"
    OpacityT                        -> "opacity:"
    OrderT                          -> "order:"
    OrphansT                        -> "orphans:"
    OutlineColorT                   -> "outline-color:"
    OutlineOffsetT                  -> "outline-offset:"
    OutlineStyleT                   -> "outline-style:"
    OutlineWidthT                   -> "outline-width:"
    OutlineT                        -> "outline:"
    OverflowAnchorT                 -> "overflow-anchor:"
    OverflowBlockT                  -> "overflow-block:"
    OverflowClipMarginT             -> "overflow-clip-margin:"
    OverflowInlineT                 -> "overflow-inline:"
    OverflowWrapT                   -> "overflow-wrap:"
    OverflowXT                      -> "overflow-x:"
    OverflowYT                      -> "overflow-y:"
    OverflowT                       -> "overflow:"
    OverlayT                        -> "overlay:"
    OverrideColorsT                 -> "override-colors:"
    OverscrollBehaviorBlockT        -> "overscroll-behavior-block:"
    OverscrollBehaviorInlineT       -> "overscroll-behavior-inline:"
    OverscrollBehaviorXT            -> "overscroll-behavior-x:"
    OverscrollBehaviorYT            -> "overscroll-behavior-y:"
    OverscrollBehaviorT             -> "overscroll-behavior:"
    PaddingBlockEndT                -> "padding-block-end:"
    PaddingBlockStartT              -> "padding-block-start:"
    PaddingBlockT                   -> "padding-block:"
    PaddingBottomT                  -> "padding-bottom:"
    PaddingInlineEndT               -> "padding-inline-end:"
    PaddingInlineStartT             -> "padding-inline-start:"
    PaddingInlineT                  -> "padding-inline:"
    PaddingLeftT                    -> "padding-left:"
    PaddingRightT                   -> "padding-right:"
    PaddingTopT                     -> "padding-top:"
    PaddingT                        -> "padding:"
    PageBreakAfterT                 -> "page-break-after:"
    PageBreakBeforeT                -> "page-break-before:"
    PageBreakInsideT                -> "page-break-inside:"
    PageD                           -> "page:"
    PaintOrderT                     -> "paint-order:"
    PerspectiveOriginT              -> "perspective-origin:"
    PerspectiveT                    -> "perspective:"
    PlaceContentT                   -> "place-content:"
    PlaceItemsT                     -> "place-items:"
    PlaceSelfT                      -> "place-self:"
    PointerEventsT                  -> "pointer-events:"
    PositionAnchorT                 -> "position-anchor:"
    PositionAreaT                   -> "position-area:"
    PositionTryFallbacksT           -> "position-try-fallbacks:"
    PositionTryOrderT               -> "position-try-order:"
    PositionTryD                    -> "position-try:"
    PositionVisibilityT             -> "position-visibility:"
    PositionT                       -> "position:"
    PrintColorAdjustT               -> "print-color-adjust:"
    QuotesT                         -> "quotes:"
    RT                              -> "r:"
    ReadingFlowT                    -> "reading-flow:"
    ReadingOrderT                   -> "reading-order:"
    ResizeT                         -> "resize:"
    ResultT                         -> "result:"
    RightT                          -> "right:"
    RotateT                         -> "rotate:"
    RowGapT                         -> "row-gap:"
    RubyAlignT                      -> "ruby-align:"
    RubyOverhangT                   -> "ruby-overhang:"
    RubyPositionT                   -> "ruby-position:"
    RxT                             -> "rx:"
    RyT                             -> "ry:"
    ScaleT                          -> "scale:"
    ScrollBehaviorT                 -> "scroll-behavior:"
    ScrollInitialTargetT            -> "scroll-initial-target:"
    ScrollMarginBlockEndT           -> "scroll-margin-block-end:"
    ScrollMarginBlockStartT         -> "scroll-margin-block-start:"
    ScrollMarginBlockT              -> "scroll-margin-block:"
    ScrollMarginBottomT             -> "scroll-margin-bottom:"
    ScrollMarginInlineEndT          -> "scroll-margin-inline-end:"
    ScrollMarginInlineStartT        -> "scroll-margin-inline-start:"
    ScrollMarginInlineT             -> "scroll-margin-inline:"
    ScrollMarginLeftT               -> "scroll-margin-left:"
    ScrollMarginRightT              -> "scroll-margin-right:"
    ScrollMarginTopT                -> "scroll-margin-top:"
    ScrollMarginT                   -> "scroll-margin:"
    ScrollMarkerGroupT              -> "scroll-marker-group:"
    ScrollPaddingBlockEndT          -> "scroll-padding-block-end:"
    ScrollPaddingBlockStartT        -> "scroll-padding-block-start:"
    ScrollPaddingBlockT             -> "scroll-padding-block:"
    ScrollPaddingBottomT            -> "scroll-padding-bottom:"
    ScrollPaddingInlineEndT         -> "scroll-padding-inline-end:"
    ScrollPaddingInlineStartT       -> "scroll-padding-inline-start:"
    ScrollPaddingInlineT            -> "scroll-padding-inline:"
    ScrollPaddingLeftT              -> "scroll-padding-left:"
    ScrollPaddingRightT             -> "scroll-padding-right:"
    ScrollPaddingTopT               -> "scroll-padding-top:"
    ScrollPaddingT                  -> "scroll-padding:"
    ScrollSnapAlignT                -> "scroll-snap-align:"
    ScrollSnapStopT                 -> "scroll-snap-stop:"
    ScrollSnapTypeT                 -> "scroll-snap-type:"
    ScrollTargetGroupT              -> "scroll-target-group:"
    ScrollTimelineAxisT             -> "scroll-timeline-axis:"
    ScrollTimelineNameT             -> "scroll-timeline-name:"
    ScrollTimelineT                 -> "scroll-timeline:"
    ScrollbarColorT                 -> "scrollbar-color:"
    ScrollbarGutterT                -> "scrollbar-gutter:"
    ScrollbarWidthT                 -> "scrollbar-width:"
    ShapeImageThresholdT            -> "shape-image-threshold:"
    ShapeMarginT                    -> "shape-margin:"
    ShapeOutsideT                   -> "shape-outside:"
    ShapeRenderingT                 -> "shape-rendering:"
    SpeakAsT                        -> "speak-as:"
    SrcT                            -> "src:"
    StopColorT                      -> "stop-color:"
    StopOpacityT                    -> "stop-opacity:"
    StrokeDasharrayT                -> "stroke-dasharray:"
    StrokeDashoffsetT               -> "stroke-dashoffset:"
    StrokeLinecapT                  -> "stroke-linecap:"
    StrokeLinejoinT                 -> "stroke-linejoin:"
    StrokeMiterlimitT               -> "stroke-miterlimit:"
    StrokeOpacityT                  -> "stroke-opacity:"
    StrokeWidthT                    -> "stroke-width:"
    StrokeT                         -> "stroke:"
    SyntaxT                         -> "syntax:"
    TabSizeT                        -> "tab-size:"
    TableLayoutT                    -> "table-layout:"
    TextAlignLastT                  -> "text-align-last:"
    TextAlignT                      -> "text-align:"
    TextAnchorT                     -> "text-anchor:"
    TextAutospaceT                  -> "text-autospace:"
    TextBoxEdgeT                    -> "text-box-edge:"
    TextBoxTrimT                    -> "text-box-trim:"
    TextBoxT                        -> "text-box:"
    TextCombineUprightT             -> "text-combine-upright:"
    TextDecorationColorT            -> "text-decoration-color:"
    TextDecorationInsetT            -> "text-decoration-inset:"
    TextDecorationLineT             -> "text-decoration-line:"
    TextDecorationSkipInkT          -> "text-decoration-skip-ink:"
    TextDecorationSkipT             -> "text-decoration-skip:"
    TextDecorationStyleT            -> "text-decoration-style:"
    TextDecorationThicknessT        -> "text-decoration-thickness:"
    TextDecorationT                 -> "text-decoration:"
    TextEmphasisColorT              -> "text-emphasis-color:"
    TextEmphasisPositionT           -> "text-emphasis-position:"
    TextEmphasisStyleT              -> "text-emphasis-style:"
    TextEmphasisT                   -> "text-emphasis:"
    TextIndentT                     -> "text-indent:"
    TextJustifyT                    -> "text-justify:"
    TextOrientationT                -> "text-orientation:"
    TextOverflowT                   -> "text-overflow:"
    TextRenderingT                  -> "text-rendering:"
    TextShadowT                     -> "text-shadow:"
    TextSizeAdjustT                 -> "text-size-adjust:"
    TextSpacingTrimT                -> "text-spacing-trim:"
    TextTransformT                  -> "text-transform:"
    TextUnderlineOffsetT            -> "text-underline-offset:"
    TextUnderlinePositionT          -> "text-underline-position:"
    TextWrapModeT                   -> "text-wrap-mode:"
    TextWrapStyleT                  -> "text-wrap-style:"
    TextWrapT                       -> "text-wrap:"
    TimelineScopeT                  -> "timeline-scope:"
    TopT                            -> "top:"
    TouchActionT                    -> "touch-action:"
    TransformBoxT                   -> "transform-box:"
    TransformOriginT                -> "transform-origin:"
    TransformStyleT                 -> "transform-style:"
    TransformT                      -> "transform:"
    TransitionBehaviorT             -> "transition-behavior:"
    TransitionDelayT                -> "transition-delay:"
    TransitionDurationT             -> "transition-duration:"
    TransitionPropertyT             -> "transition-property:"
    TransitionTimingFunctionT       -> "transition-timing-function:"
    TransitionT                     -> "transition:"
    TranslateT                      -> "translate:"
    UnicodeBidiT                    -> "unicode-bidi:"
    UnicodeRangeT                   -> "unicode-range:"
    UserModifyT                     -> "user-modify:"
    UserSelectT                     -> "user-select:"
    VectorEffectT                   -> "vector-effect:"
    VerticalAlignT                  -> "vertical-align:"
    ViewTimelineAxisT               -> "view-timeline-axis:"
    ViewTimelineInsetT              -> "view-timeline-inset:"
    ViewTimelineNameT               -> "view-timeline-name:"
    ViewTimelineT                   -> "view-timeline:"
    ViewTransitionClassT            -> "view-transition-class:"
    ViewTransitionNameT             -> "view-transition-name:"
    VisibilityT                     -> "visibility:"
    WhiteSpaceCollapseT             -> "white-space-collapse:"
    WhiteSpaceT                     -> "white-space:"
    WidowsT                         -> "widows:"
    WidthT                          -> "width:"
    WillChangeT                     -> "will-change:"
    WordBreakT                      -> "word-break:"
    WordSpacingT                    -> "word-spacing:"
    WordWrapT                       -> "word-wrap:"
    WritingModeT                    -> "writing-mode:"
    XT                              -> "x:"
    YT                              -> "y:"
    ZIndexT                         -> "z-index:"
    ZoomT                           -> "zoom:"

toPropertyName :: Descriptor -> PropertyName
toPropertyName = \case
  CustomDescriptor i -> VarProp $ Var i
  bsd@BrowserSpecificDescriptor{} ->
    PropertyName . Ident . toStrict . dropEnd 1 $ toCssText bsd
  o -> PropertyName . Ident . toStrict . dropEnd 1 $ toCssText o
