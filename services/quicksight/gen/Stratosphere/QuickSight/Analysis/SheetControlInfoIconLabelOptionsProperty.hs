module Stratosphere.QuickSight.Analysis.SheetControlInfoIconLabelOptionsProperty (
        SheetControlInfoIconLabelOptionsProperty(..),
        mkSheetControlInfoIconLabelOptionsProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data SheetControlInfoIconLabelOptionsProperty
  = SheetControlInfoIconLabelOptionsProperty {infoIconText :: (Prelude.Maybe (Value Prelude.Text)),
                                              visibility :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkSheetControlInfoIconLabelOptionsProperty ::
  SheetControlInfoIconLabelOptionsProperty
mkSheetControlInfoIconLabelOptionsProperty
  = SheetControlInfoIconLabelOptionsProperty
      {infoIconText = Prelude.Nothing, visibility = Prelude.Nothing}
instance ToResourceProperties SheetControlInfoIconLabelOptionsProperty where
  toResourceProperties SheetControlInfoIconLabelOptionsProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Analysis.SheetControlInfoIconLabelOptions",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "InfoIconText" Prelude.<$> infoIconText,
                            (JSON..=) "Visibility" Prelude.<$> visibility])}
instance JSON.ToJSON SheetControlInfoIconLabelOptionsProperty where
  toJSON SheetControlInfoIconLabelOptionsProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "InfoIconText" Prelude.<$> infoIconText,
               (JSON..=) "Visibility" Prelude.<$> visibility]))
instance Property "InfoIconText" SheetControlInfoIconLabelOptionsProperty where
  type PropertyType "InfoIconText" SheetControlInfoIconLabelOptionsProperty = Value Prelude.Text
  set newValue SheetControlInfoIconLabelOptionsProperty {..}
    = SheetControlInfoIconLabelOptionsProperty
        {infoIconText = Prelude.pure newValue, ..}
instance Property "Visibility" SheetControlInfoIconLabelOptionsProperty where
  type PropertyType "Visibility" SheetControlInfoIconLabelOptionsProperty = Value Prelude.Text
  set newValue SheetControlInfoIconLabelOptionsProperty {..}
    = SheetControlInfoIconLabelOptionsProperty
        {visibility = Prelude.pure newValue, ..}