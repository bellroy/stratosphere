module Stratosphere.QBusiness.DataSource.DocumentAttributeTargetProperty (
        module Exports, DocumentAttributeTargetProperty(..),
        mkDocumentAttributeTargetProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QBusiness.DataSource.DocumentAttributeValueProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data DocumentAttributeTargetProperty
  = DocumentAttributeTargetProperty {attributeValueOperator :: (Prelude.Maybe (Value Prelude.Text)),
                                     key :: (Value Prelude.Text),
                                     value :: (Prelude.Maybe DocumentAttributeValueProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkDocumentAttributeTargetProperty ::
  Value Prelude.Text -> DocumentAttributeTargetProperty
mkDocumentAttributeTargetProperty key
  = DocumentAttributeTargetProperty
      {key = key, attributeValueOperator = Prelude.Nothing,
       value = Prelude.Nothing}
instance ToResourceProperties DocumentAttributeTargetProperty where
  toResourceProperties DocumentAttributeTargetProperty {..}
    = ResourceProperties
        {awsType = "AWS::QBusiness::DataSource.DocumentAttributeTarget",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Key" JSON..= key]
                           (Prelude.catMaybes
                              [(JSON..=) "AttributeValueOperator"
                                 Prelude.<$> attributeValueOperator,
                               (JSON..=) "Value" Prelude.<$> value]))}
instance JSON.ToJSON DocumentAttributeTargetProperty where
  toJSON DocumentAttributeTargetProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Key" JSON..= key]
              (Prelude.catMaybes
                 [(JSON..=) "AttributeValueOperator"
                    Prelude.<$> attributeValueOperator,
                  (JSON..=) "Value" Prelude.<$> value])))
instance Property "AttributeValueOperator" DocumentAttributeTargetProperty where
  type PropertyType "AttributeValueOperator" DocumentAttributeTargetProperty = Value Prelude.Text
  set newValue DocumentAttributeTargetProperty {..}
    = DocumentAttributeTargetProperty
        {attributeValueOperator = Prelude.pure newValue, ..}
instance Property "Key" DocumentAttributeTargetProperty where
  type PropertyType "Key" DocumentAttributeTargetProperty = Value Prelude.Text
  set newValue DocumentAttributeTargetProperty {..}
    = DocumentAttributeTargetProperty {key = newValue, ..}
instance Property "Value" DocumentAttributeTargetProperty where
  type PropertyType "Value" DocumentAttributeTargetProperty = DocumentAttributeValueProperty
  set newValue DocumentAttributeTargetProperty {..}
    = DocumentAttributeTargetProperty
        {value = Prelude.pure newValue, ..}