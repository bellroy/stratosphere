
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-layer-lifecycleeventconfiguration-shutdowneventconfiguration.html

module Stratosphere.ResourceProperties.OpsWorksLayerShutdownEventConfiguration where

import Stratosphere.ResourceImports


-- | Full data type definition for OpsWorksLayerShutdownEventConfiguration.
-- See 'opsWorksLayerShutdownEventConfiguration' for a more convenient
-- constructor.
data OpsWorksLayerShutdownEventConfiguration =
  OpsWorksLayerShutdownEventConfiguration
  { _opsWorksLayerShutdownEventConfigurationDelayUntilElbConnectionsDrained :: Maybe (Val Bool)
  , _opsWorksLayerShutdownEventConfigurationExecutionTimeout :: Maybe (Val Integer)
  } deriving (Show, Eq)

instance ToJSON OpsWorksLayerShutdownEventConfiguration where
  toJSON OpsWorksLayerShutdownEventConfiguration{..} =
    object $
    catMaybes
    [ fmap (("DelayUntilElbConnectionsDrained",) . toJSON) _opsWorksLayerShutdownEventConfigurationDelayUntilElbConnectionsDrained
    , fmap (("ExecutionTimeout",) . toJSON) _opsWorksLayerShutdownEventConfigurationExecutionTimeout
    ]

-- | Constructor for 'OpsWorksLayerShutdownEventConfiguration' containing
-- required fields as arguments.
opsWorksLayerShutdownEventConfiguration
  :: OpsWorksLayerShutdownEventConfiguration
opsWorksLayerShutdownEventConfiguration  =
  OpsWorksLayerShutdownEventConfiguration
  { _opsWorksLayerShutdownEventConfigurationDelayUntilElbConnectionsDrained = Nothing
  , _opsWorksLayerShutdownEventConfigurationExecutionTimeout = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-layer-lifecycleeventconfiguration-shutdowneventconfiguration.html#cfn-opsworks-layer-lifecycleconfiguration-shutdowneventconfiguration-delayuntilelbconnectionsdrained
owlsecDelayUntilElbConnectionsDrained :: Lens' OpsWorksLayerShutdownEventConfiguration (Maybe (Val Bool))
owlsecDelayUntilElbConnectionsDrained = lens _opsWorksLayerShutdownEventConfigurationDelayUntilElbConnectionsDrained (\s a -> s { _opsWorksLayerShutdownEventConfigurationDelayUntilElbConnectionsDrained = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-layer-lifecycleeventconfiguration-shutdowneventconfiguration.html#cfn-opsworks-layer-lifecycleconfiguration-shutdowneventconfiguration-executiontimeout
owlsecExecutionTimeout :: Lens' OpsWorksLayerShutdownEventConfiguration (Maybe (Val Integer))
owlsecExecutionTimeout = lens _opsWorksLayerShutdownEventConfigurationExecutionTimeout (\s a -> s { _opsWorksLayerShutdownEventConfigurationExecutionTimeout = a })
