module Stratosphere.IoT.JobTemplate.TimeoutConfigProperty where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.ResourceProperties
data TimeoutConfigProperty :: Prelude.Type
instance ToResourceProperties TimeoutConfigProperty
instance Prelude.Eq TimeoutConfigProperty
instance Prelude.Show TimeoutConfigProperty
instance JSON.ToJSON TimeoutConfigProperty