module Stratosphere.NetworkFirewall.TLSInspectionConfiguration.ServerCertificateScopeProperty (
        module Exports, ServerCertificateScopeProperty(..),
        mkServerCertificateScopeProperty
    ) where
import qualified Data.Aeson as JSON
import qualified Stratosphere.Prelude as Prelude
import Stratosphere.Property
import {-# SOURCE #-} Stratosphere.NetworkFirewall.TLSInspectionConfiguration.AddressProperty as Exports
import {-# SOURCE #-} Stratosphere.NetworkFirewall.TLSInspectionConfiguration.PortRangeProperty as Exports
import Stratosphere.ResourceProperties
import Stratosphere.Value
data ServerCertificateScopeProperty
  = ServerCertificateScopeProperty {destinationPorts :: (Prelude.Maybe [PortRangeProperty]),
                                    destinations :: (Prelude.Maybe [AddressProperty]),
                                    protocols :: (Prelude.Maybe (ValueList Prelude.Integer)),
                                    sourcePorts :: (Prelude.Maybe [PortRangeProperty]),
                                    sources :: (Prelude.Maybe [AddressProperty])}
  deriving stock (Prelude.Eq, Prelude.Show)
mkServerCertificateScopeProperty :: ServerCertificateScopeProperty
mkServerCertificateScopeProperty
  = ServerCertificateScopeProperty
      {destinationPorts = Prelude.Nothing,
       destinations = Prelude.Nothing, protocols = Prelude.Nothing,
       sourcePorts = Prelude.Nothing, sources = Prelude.Nothing}
instance ToResourceProperties ServerCertificateScopeProperty where
  toResourceProperties ServerCertificateScopeProperty {..}
    = ResourceProperties
        {awsType = "AWS::NetworkFirewall::TLSInspectionConfiguration.ServerCertificateScope",
         supportsTags = Prelude.False,
         properties = Prelude.fromList
                        (Prelude.catMaybes
                           [(JSON..=) "DestinationPorts" Prelude.<$> destinationPorts,
                            (JSON..=) "Destinations" Prelude.<$> destinations,
                            (JSON..=) "Protocols" Prelude.<$> protocols,
                            (JSON..=) "SourcePorts" Prelude.<$> sourcePorts,
                            (JSON..=) "Sources" Prelude.<$> sources])}
instance JSON.ToJSON ServerCertificateScopeProperty where
  toJSON ServerCertificateScopeProperty {..}
    = JSON.object
        (Prelude.fromList
           (Prelude.catMaybes
              [(JSON..=) "DestinationPorts" Prelude.<$> destinationPorts,
               (JSON..=) "Destinations" Prelude.<$> destinations,
               (JSON..=) "Protocols" Prelude.<$> protocols,
               (JSON..=) "SourcePorts" Prelude.<$> sourcePorts,
               (JSON..=) "Sources" Prelude.<$> sources]))
instance Property "DestinationPorts" ServerCertificateScopeProperty where
  type PropertyType "DestinationPorts" ServerCertificateScopeProperty = [PortRangeProperty]
  set newValue ServerCertificateScopeProperty {..}
    = ServerCertificateScopeProperty
        {destinationPorts = Prelude.pure newValue, ..}
instance Property "Destinations" ServerCertificateScopeProperty where
  type PropertyType "Destinations" ServerCertificateScopeProperty = [AddressProperty]
  set newValue ServerCertificateScopeProperty {..}
    = ServerCertificateScopeProperty
        {destinations = Prelude.pure newValue, ..}
instance Property "Protocols" ServerCertificateScopeProperty where
  type PropertyType "Protocols" ServerCertificateScopeProperty = ValueList Prelude.Integer
  set newValue ServerCertificateScopeProperty {..}
    = ServerCertificateScopeProperty
        {protocols = Prelude.pure newValue, ..}
instance Property "SourcePorts" ServerCertificateScopeProperty where
  type PropertyType "SourcePorts" ServerCertificateScopeProperty = [PortRangeProperty]
  set newValue ServerCertificateScopeProperty {..}
    = ServerCertificateScopeProperty
        {sourcePorts = Prelude.pure newValue, ..}
instance Property "Sources" ServerCertificateScopeProperty where
  type PropertyType "Sources" ServerCertificateScopeProperty = [AddressProperty]
  set newValue ServerCertificateScopeProperty {..}
    = ServerCertificateScopeProperty
        {sources = Prelude.pure newValue, ..}