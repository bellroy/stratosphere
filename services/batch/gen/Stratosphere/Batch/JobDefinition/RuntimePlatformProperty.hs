module Stratosphere.Batch.JobDefinition.RuntimePlatformProperty (
        RuntimePlatformProperty(..), mkRuntimePlatformProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data RuntimePlatformProperty
  = RuntimePlatformProperty {cpuArchitecture :: (Prelude.Maybe (Value Prelude.Text)),
                             operatingSystemFamily :: (Prelude.Maybe (Value Prelude.Text))}
  deriving stock (Prelude.Eq, Prelude.Show)
mkRuntimePlatformProperty :: RuntimePlatformProperty
mkRuntimePlatformProperty
  = RuntimePlatformProperty
      {cpuArchitecture = Prelude.Nothing,
       operatingSystemFamily = Prelude.Nothing}
instance ToResourceProperties RuntimePlatformProperty where
  toResourceProperties RuntimePlatformProperty {..}
    = ResourceProperties
        {awsType = "AWS::Batch::JobDefinition.RuntimePlatform",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "CpuArchitecture" Prelude.<$> cpuArchitecture,
                            (JSON..=) "OperatingSystemFamily"
                              Prelude.<$> operatingSystemFamily])}
instance JSON.ToJSON RuntimePlatformProperty where
  toJSON RuntimePlatformProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "CpuArchitecture" Prelude.<$> cpuArchitecture,
               (JSON..=) "OperatingSystemFamily"
                 Prelude.<$> operatingSystemFamily]))
instance Property "CpuArchitecture" RuntimePlatformProperty where
  type PropertyType "CpuArchitecture" RuntimePlatformProperty = Value Prelude.Text
  set newValue RuntimePlatformProperty {..}
    = RuntimePlatformProperty
        {cpuArchitecture = Prelude.pure newValue, ..}
instance Property "OperatingSystemFamily" RuntimePlatformProperty where
  type PropertyType "OperatingSystemFamily" RuntimePlatformProperty = Value Prelude.Text
  set newValue RuntimePlatformProperty {..}
    = RuntimePlatformProperty
        {operatingSystemFamily = Prelude.pure newValue, ..}