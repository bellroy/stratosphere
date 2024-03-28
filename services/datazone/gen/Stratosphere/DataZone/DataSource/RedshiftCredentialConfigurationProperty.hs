module Stratosphere.DataZone.DataSource.RedshiftCredentialConfigurationProperty (
        RedshiftCredentialConfigurationProperty(..),
        mkRedshiftCredentialConfigurationProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import Stratosphere.ResourceProperties
import Stratosphere.Value
data RedshiftCredentialConfigurationProperty
  = RedshiftCredentialConfigurationProperty {secretManagerArn :: (Value Prelude.Text)}
  deriving stock (Prelude.Eq, Prelude.Show)
mkRedshiftCredentialConfigurationProperty ::
  Value Prelude.Text -> RedshiftCredentialConfigurationProperty
mkRedshiftCredentialConfigurationProperty secretManagerArn
  = RedshiftCredentialConfigurationProperty
      {secretManagerArn = secretManagerArn}
instance ToResourceProperties RedshiftCredentialConfigurationProperty where
  toResourceProperties RedshiftCredentialConfigurationProperty {..}
    = ResourceProperties
        {awsType = "AWS::DataZone::DataSource.RedshiftCredentialConfiguration",
         supportsTags = Prelude.False,
         properties = ["SecretManagerArn" JSON..= secretManagerArn]}
instance JSON.ToJSON RedshiftCredentialConfigurationProperty where
  toJSON RedshiftCredentialConfigurationProperty {..}
    = JSON.object ["SecretManagerArn" JSON..= secretManagerArn]
instance Property "SecretManagerArn" RedshiftCredentialConfigurationProperty where
  type PropertyType "SecretManagerArn" RedshiftCredentialConfigurationProperty = Value Prelude.Text
  set newValue RedshiftCredentialConfigurationProperty {}
    = RedshiftCredentialConfigurationProperty
        {secretManagerArn = newValue, ..}