module Stratosphere.Timestream.Table.MagneticStoreWritePropertiesProperty (
        module Exports, MagneticStoreWritePropertiesProperty(..),
        mkMagneticStoreWritePropertiesProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.Timestream.Table.MagneticStoreRejectedDataLocationProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data MagneticStoreWritePropertiesProperty
  = MagneticStoreWritePropertiesProperty {enableMagneticStoreWrites :: (Value Prelude.Bool),
                                          magneticStoreRejectedDataLocation :: (Prelude.Maybe MagneticStoreRejectedDataLocationProperty)}
mkMagneticStoreWritePropertiesProperty ::
  Value Prelude.Bool -> MagneticStoreWritePropertiesProperty
mkMagneticStoreWritePropertiesProperty enableMagneticStoreWrites
  = MagneticStoreWritePropertiesProperty
      {enableMagneticStoreWrites = enableMagneticStoreWrites,
       magneticStoreRejectedDataLocation = Prelude.Nothing}
instance ToResourceProperties MagneticStoreWritePropertiesProperty where
  toResourceProperties MagneticStoreWritePropertiesProperty {..}
    = ResourceProperties
        {awsType = "AWS::Timestream::Table.MagneticStoreWriteProperties",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        ((Prelude.<>)
                           ["EnableMagneticStoreWrites" JSON..= enableMagneticStoreWrites]
                           (Prelude.catMaybes
                              [(JSON..=) "MagneticStoreRejectedDataLocation"
                                 Prelude.<$> magneticStoreRejectedDataLocation]))}
instance JSON.ToJSON MagneticStoreWritePropertiesProperty where
  toJSON MagneticStoreWritePropertiesProperty {..}
    = JSON.object
        (Prelude.fromList
           ((Prelude.<>)
              ["EnableMagneticStoreWrites" JSON..= enableMagneticStoreWrites]
              (Prelude.catMaybes
                 [(JSON..=) "MagneticStoreRejectedDataLocation"
                    Prelude.<$> magneticStoreRejectedDataLocation])))
instance Property "EnableMagneticStoreWrites" MagneticStoreWritePropertiesProperty where
  type PropertyType "EnableMagneticStoreWrites" MagneticStoreWritePropertiesProperty = Value Prelude.Bool
  set newValue MagneticStoreWritePropertiesProperty {..}
    = MagneticStoreWritePropertiesProperty
        {enableMagneticStoreWrites = newValue, ..}
instance Property "MagneticStoreRejectedDataLocation" MagneticStoreWritePropertiesProperty where
  type PropertyType "MagneticStoreRejectedDataLocation" MagneticStoreWritePropertiesProperty = MagneticStoreRejectedDataLocationProperty
  set newValue MagneticStoreWritePropertiesProperty {..}
    = MagneticStoreWritePropertiesProperty
        {magneticStoreRejectedDataLocation = Prelude.pure newValue, ..}