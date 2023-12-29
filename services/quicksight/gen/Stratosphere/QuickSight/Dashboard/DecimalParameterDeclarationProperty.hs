module Stratosphere.QuickSight.Dashboard.DecimalParameterDeclarationProperty (
        module Exports, DecimalParameterDeclarationProperty(..),
        mkDecimalParameterDeclarationProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.DecimalDefaultValuesProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.DecimalValueWhenUnsetConfigurationProperty as Exports
import {-# SOURCE #-} Stratosphere.QuickSight.Dashboard.MappedDataSetParameterProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data DecimalParameterDeclarationProperty
  = DecimalParameterDeclarationProperty {defaultValues :: (Prelude.Maybe DecimalDefaultValuesProperty),
                                         mappedDataSetParameters :: (Prelude.Maybe [MappedDataSetParameterProperty]),
                                         name :: (Value Prelude.Text),
                                         parameterValueType :: (Value Prelude.Text),
                                         valueWhenUnset :: (Prelude.Maybe DecimalValueWhenUnsetConfigurationProperty)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkDecimalParameterDeclarationProperty ::
  Value Prelude.Text
  -> Value Prelude.Text -> DecimalParameterDeclarationProperty
mkDecimalParameterDeclarationProperty name parameterValueType
  = DecimalParameterDeclarationProperty
      {name = name, parameterValueType = parameterValueType,
       defaultValues = Prelude.Nothing,
       mappedDataSetParameters = Prelude.Nothing,
       valueWhenUnset = Prelude.Nothing}
instance ToResourceProperties DecimalParameterDeclarationProperty where
  toResourceProperties DecimalParameterDeclarationProperty {..}
    = ResourceProperties
        {awsType = "AWS::QuickSight::Dashboard.DecimalParameterDeclaration",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["Name" JSON..= name,
                            "ParameterValueType" JSON..= parameterValueType]
                           (Prelude.catMaybes
                              [(JSON..=) "DefaultValues" Prelude.<$> defaultValues,
                               (JSON..=) "MappedDataSetParameters"
                                 Prelude.<$> mappedDataSetParameters,
                               (JSON..=) "ValueWhenUnset" Prelude.<$> valueWhenUnset]))}
instance JSON.ToJSON DecimalParameterDeclarationProperty where
  toJSON DecimalParameterDeclarationProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["Name" JSON..= name,
               "ParameterValueType" JSON..= parameterValueType]
              (Prelude.catMaybes
                 [(JSON..=) "DefaultValues" Prelude.<$> defaultValues,
                  (JSON..=) "MappedDataSetParameters"
                    Prelude.<$> mappedDataSetParameters,
                  (JSON..=) "ValueWhenUnset" Prelude.<$> valueWhenUnset])))
instance Property "DefaultValues" DecimalParameterDeclarationProperty where
  type PropertyType "DefaultValues" DecimalParameterDeclarationProperty = DecimalDefaultValuesProperty
  set newValue DecimalParameterDeclarationProperty {..}
    = DecimalParameterDeclarationProperty
        {defaultValues = Prelude.pure newValue, ..}
instance Property "MappedDataSetParameters" DecimalParameterDeclarationProperty where
  type PropertyType "MappedDataSetParameters" DecimalParameterDeclarationProperty = [MappedDataSetParameterProperty]
  set newValue DecimalParameterDeclarationProperty {..}
    = DecimalParameterDeclarationProperty
        {mappedDataSetParameters = Prelude.pure newValue, ..}
instance Property "Name" DecimalParameterDeclarationProperty where
  type PropertyType "Name" DecimalParameterDeclarationProperty = Value Prelude.Text
  set newValue DecimalParameterDeclarationProperty {..}
    = DecimalParameterDeclarationProperty {name = newValue, ..}
instance Property "ParameterValueType" DecimalParameterDeclarationProperty where
  type PropertyType "ParameterValueType" DecimalParameterDeclarationProperty = Value Prelude.Text
  set newValue DecimalParameterDeclarationProperty {..}
    = DecimalParameterDeclarationProperty
        {parameterValueType = newValue, ..}
instance Property "ValueWhenUnset" DecimalParameterDeclarationProperty where
  type PropertyType "ValueWhenUnset" DecimalParameterDeclarationProperty = DecimalValueWhenUnsetConfigurationProperty
  set newValue DecimalParameterDeclarationProperty {..}
    = DecimalParameterDeclarationProperty
        {valueWhenUnset = Prelude.pure newValue, ..}