
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-codedeploy-deploymentgroup-autorollbackconfiguration.html

module Stratosphere.ResourceProperties.CodeDeployDeploymentGroupAutoRollbackConfiguration where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for
-- CodeDeployDeploymentGroupAutoRollbackConfiguration. See
-- 'codeDeployDeploymentGroupAutoRollbackConfiguration' for a more
-- convenient constructor.
data CodeDeployDeploymentGroupAutoRollbackConfiguration =
  CodeDeployDeploymentGroupAutoRollbackConfiguration
  { _codeDeployDeploymentGroupAutoRollbackConfigurationEnabled :: Maybe (Val Bool)
  , _codeDeployDeploymentGroupAutoRollbackConfigurationEvents :: Maybe (ValList Text)
  } deriving (Show, Eq)

instance ToJSON CodeDeployDeploymentGroupAutoRollbackConfiguration where
  toJSON CodeDeployDeploymentGroupAutoRollbackConfiguration{..} =
    object $
    catMaybes
    [ fmap (("Enabled",) . toJSON) _codeDeployDeploymentGroupAutoRollbackConfigurationEnabled
    , fmap (("Events",) . toJSON) _codeDeployDeploymentGroupAutoRollbackConfigurationEvents
    ]

-- | Constructor for 'CodeDeployDeploymentGroupAutoRollbackConfiguration'
-- containing required fields as arguments.
codeDeployDeploymentGroupAutoRollbackConfiguration
  :: CodeDeployDeploymentGroupAutoRollbackConfiguration
codeDeployDeploymentGroupAutoRollbackConfiguration  =
  CodeDeployDeploymentGroupAutoRollbackConfiguration
  { _codeDeployDeploymentGroupAutoRollbackConfigurationEnabled = Nothing
  , _codeDeployDeploymentGroupAutoRollbackConfigurationEvents = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-codedeploy-deploymentgroup-autorollbackconfiguration.html#cfn-codedeploy-deploymentgroup-autorollbackconfiguration-enabled
cddgarcEnabled :: Lens' CodeDeployDeploymentGroupAutoRollbackConfiguration (Maybe (Val Bool))
cddgarcEnabled = lens _codeDeployDeploymentGroupAutoRollbackConfigurationEnabled (\s a -> s { _codeDeployDeploymentGroupAutoRollbackConfigurationEnabled = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-codedeploy-deploymentgroup-autorollbackconfiguration.html#cfn-codedeploy-deploymentgroup-autorollbackconfiguration-events
cddgarcEvents :: Lens' CodeDeployDeploymentGroupAutoRollbackConfiguration (Maybe (ValList Text))
cddgarcEvents = lens _codeDeployDeploymentGroupAutoRollbackConfigurationEvents (\s a -> s { _codeDeployDeploymentGroupAutoRollbackConfigurationEvents = a })
