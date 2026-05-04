module CssParser.At.Import where

import CssParser.At.Layer ( LayerName )
import CssParser.At.Supports ( FeatureQuery, hasOuterParens )
import CssParser.Prelude
import CssParser.Rule.Value ( Source )
import CssParser.Show ( CssShow(..) )
import CssParser.At.MediaQuery ( MediaQuery )

{-
@import url;
@import url layer;
@import url layer(layer-name);
@import url layer(layer-name) supports(supports-condition);
@import url layer(layer-name) supports(supports-condition) list-of-media-queries;
@import url layer(layer-name) list-of-media-queries;
@import url supports(supports-condition);
@import url supports(supports-condition) list-of-media-queries;
@import url list-of-media-queries;
-}

data Import sl
  = ImportDefaultLayer Source
  | ImportUrlLayer     Source (Maybe LayerName) (Maybe (FeatureQuery sl)) [MediaQuery]
  | ImportUrlSupports  Source                   (Maybe (FeatureQuery sl)) [MediaQuery]
  deriving (Show, Eq, Generic)

instance CssShow sl => CssShow (Import sl) where
  toCssText x = "@import " <> go x <> ";"
    where
      showMq = \case
        [] -> ""
        o -> " " <> toCssText o
      embrace v
        | hasOuterParens v = toCssText v
        | otherwise = "(" <> toCssText v <> ")"
      showSup = maybe "" ((" supports" <>) . embrace)
      go = \case
        ImportDefaultLayer u -> toCssText u <> " layer"
        ImportUrlLayer u l sup mq ->
          toCssText u <> " layer(" <> toCssText l <> ")" <> showSup sup <> showMq mq
        ImportUrlSupports u sup mq ->
          toCssText u <> showSup sup <> showMq mq
