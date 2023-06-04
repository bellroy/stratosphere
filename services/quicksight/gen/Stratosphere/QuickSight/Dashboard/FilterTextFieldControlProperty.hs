module Stratosphere.QuickSight.Dashboard.FilterTextFieldControlProperty (
        module Exports, FilterTextFieldControlProperty(..),
        mkFilterTextFieldControlProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.TextFieldControlDisplayOptionsProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data FilterTextFieldControlProperty
  = FilterTextFieldControlProperty {displayOptions :: (Prelude.Maybe TextFieldControlDisplayOptionsProperty),
                                    filterControlId :: (Value Prelude.Text),
                                    sourceFilterId :: (Value Prelude.Text),
                                    title :: (Value Prelude.Text)}
mkFilterTextFieldControlProperty ::
  Value Prelude.Text
  -> Value Prelude.Text
     -> Value Prelude.Text -> FilterTextFieldControlProperty
mkFilterTextFieldControlProperty
  filterControlId
  sourceFilterId
  title
  = FilterTextFieldControlProperty
      {filterControlId = filterControlId,
       sourceFilterId = sourceFilterId, title = title,
       displayOptions = Prelude.Nothing}
instance ToResourceProperties FilterTextFieldControlProperty where
  toResourceProperties FilterTextFieldControlProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.FilterTextFieldControl",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["FilterControlId" JSON..= filterControlId,
                            "SourceFilterId" JSON..= sourceFilterId, "Title" JSON..= title]
                           (Prelude.catMaybes
                              [(JSON..=) "DisplayOptions" Prelude.<$> displayOptions]))}
instance JSON.ToJSON FilterTextFieldControlProperty where
  toJSON FilterTextFieldControlProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["FilterControlId" JSON..= filterControlId,
               "SourceFilterId" JSON..= sourceFilterId, "Title" JSON..= title]
              (Prelude.catMaybes
                 [(JSON..=) "DisplayOptions" Prelude.<$> displayOptions])))
instance Property "DisplayOptions" FilterTextFieldControlProperty where
  type PropertyType "DisplayOptions" FilterTextFieldControlProperty = TextFieldControlDisplayOptionsProperty
  set newValue FilterTextFieldControlProperty {..}
    = FilterTextFieldControlProperty
        {displayOptions = Prelude.pure newValue, ..}
instance Property "FilterControlId" FilterTextFieldControlProperty where
  type PropertyType "FilterControlId" FilterTextFieldControlProperty = Value Prelude.Text
  set newValue FilterTextFieldControlProperty {..}
    = FilterTextFieldControlProperty {filterControlId = newValue, ..}
instance Property "SourceFilterId" FilterTextFieldControlProperty where
  type PropertyType "SourceFilterId" FilterTextFieldControlProperty = Value Prelude.Text
  set newValue FilterTextFieldControlProperty {..}
    = FilterTextFieldControlProperty {sourceFilterId = newValue, ..}
instance Property "Title" FilterTextFieldControlProperty where
  type PropertyType "Title" FilterTextFieldControlProperty = Value Prelude.Text
  set newValue FilterTextFieldControlProperty {..}
    = FilterTextFieldControlProperty {title = newValue, ..}