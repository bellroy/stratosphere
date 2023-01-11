
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-rds-dbcluster-dbclusterrole.html

module Stratosphere.ResourceProperties.RDSDBClusterDBClusterRole where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for RDSDBClusterDBClusterRole. See
-- 'rdsdbClusterDBClusterRole' for a more convenient constructor.
data RDSDBClusterDBClusterRole =
  RDSDBClusterDBClusterRole
  { _rDSDBClusterDBClusterRoleFeatureName :: Maybe (Val Text)
  , _rDSDBClusterDBClusterRoleRoleArn :: Val Text
  } deriving (Show, Eq)

instance ToJSON RDSDBClusterDBClusterRole where
  toJSON RDSDBClusterDBClusterRole{..} =
    object $
    catMaybes
    [ fmap (("FeatureName",) . toJSON) _rDSDBClusterDBClusterRoleFeatureName
    , (Just . ("RoleArn",) . toJSON) _rDSDBClusterDBClusterRoleRoleArn
    ]

-- | Constructor for 'RDSDBClusterDBClusterRole' containing required fields as
-- arguments.
rdsdbClusterDBClusterRole
  :: Val Text -- ^ 'rdsdbcdbcrRoleArn'
  -> RDSDBClusterDBClusterRole
rdsdbClusterDBClusterRole roleArnarg =
  RDSDBClusterDBClusterRole
  { _rDSDBClusterDBClusterRoleFeatureName = Nothing
  , _rDSDBClusterDBClusterRoleRoleArn = roleArnarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-rds-dbcluster-dbclusterrole.html#cfn-rds-dbcluster-dbclusterrole-featurename
rdsdbcdbcrFeatureName :: Lens' RDSDBClusterDBClusterRole (Maybe (Val Text))
rdsdbcdbcrFeatureName = lens _rDSDBClusterDBClusterRoleFeatureName (\s a -> s { _rDSDBClusterDBClusterRoleFeatureName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-rds-dbcluster-dbclusterrole.html#cfn-rds-dbcluster-dbclusterrole-rolearn
rdsdbcdbcrRoleArn :: Lens' RDSDBClusterDBClusterRole (Val Text)
rdsdbcdbcrRoleArn = lens _rDSDBClusterDBClusterRoleRoleArn (\s a -> s { _rDSDBClusterDBClusterRoleRoleArn = a })
