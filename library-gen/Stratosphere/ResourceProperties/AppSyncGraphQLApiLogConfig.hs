
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-appsync-graphqlapi-logconfig.html

module Stratosphere.ResourceProperties.AppSyncGraphQLApiLogConfig where

import Stratosphere.ResourceImports


-- | Full data type definition for AppSyncGraphQLApiLogConfig. See
-- 'appSyncGraphQLApiLogConfig' for a more convenient constructor.
data AppSyncGraphQLApiLogConfig =
  AppSyncGraphQLApiLogConfig
  { _appSyncGraphQLApiLogConfigCloudWatchLogsRoleArn :: Maybe (Val Text)
  , _appSyncGraphQLApiLogConfigExcludeVerboseContent :: Maybe (Val Bool)
  , _appSyncGraphQLApiLogConfigFieldLogLevel :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON AppSyncGraphQLApiLogConfig where
  toJSON AppSyncGraphQLApiLogConfig{..} =
    object $
    catMaybes
    [ fmap (("CloudWatchLogsRoleArn",) . toJSON) _appSyncGraphQLApiLogConfigCloudWatchLogsRoleArn
    , fmap (("ExcludeVerboseContent",) . toJSON) _appSyncGraphQLApiLogConfigExcludeVerboseContent
    , fmap (("FieldLogLevel",) . toJSON) _appSyncGraphQLApiLogConfigFieldLogLevel
    ]

-- | Constructor for 'AppSyncGraphQLApiLogConfig' containing required fields
-- as arguments.
appSyncGraphQLApiLogConfig
  :: AppSyncGraphQLApiLogConfig
appSyncGraphQLApiLogConfig  =
  AppSyncGraphQLApiLogConfig
  { _appSyncGraphQLApiLogConfigCloudWatchLogsRoleArn = Nothing
  , _appSyncGraphQLApiLogConfigExcludeVerboseContent = Nothing
  , _appSyncGraphQLApiLogConfigFieldLogLevel = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-appsync-graphqlapi-logconfig.html#cfn-appsync-graphqlapi-logconfig-cloudwatchlogsrolearn
asgqlalcCloudWatchLogsRoleArn :: Lens' AppSyncGraphQLApiLogConfig (Maybe (Val Text))
asgqlalcCloudWatchLogsRoleArn = lens _appSyncGraphQLApiLogConfigCloudWatchLogsRoleArn (\s a -> s { _appSyncGraphQLApiLogConfigCloudWatchLogsRoleArn = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-appsync-graphqlapi-logconfig.html#cfn-appsync-graphqlapi-logconfig-excludeverbosecontent
asgqlalcExcludeVerboseContent :: Lens' AppSyncGraphQLApiLogConfig (Maybe (Val Bool))
asgqlalcExcludeVerboseContent = lens _appSyncGraphQLApiLogConfigExcludeVerboseContent (\s a -> s { _appSyncGraphQLApiLogConfigExcludeVerboseContent = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-appsync-graphqlapi-logconfig.html#cfn-appsync-graphqlapi-logconfig-fieldloglevel
asgqlalcFieldLogLevel :: Lens' AppSyncGraphQLApiLogConfig (Maybe (Val Text))
asgqlalcFieldLogLevel = lens _appSyncGraphQLApiLogConfigFieldLogLevel (\s a -> s { _appSyncGraphQLApiLogConfigFieldLogLevel = a })
