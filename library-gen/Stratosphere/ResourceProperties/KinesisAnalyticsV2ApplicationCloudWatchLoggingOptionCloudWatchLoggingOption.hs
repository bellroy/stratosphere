
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisanalyticsv2-applicationcloudwatchloggingoption-cloudwatchloggingoption.html

module Stratosphere.ResourceProperties.KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption where

import Stratosphere.ResourceImports


-- | Full data type definition for
-- KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption.
-- See
-- 'kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption'
-- for a more convenient constructor.
data KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption =
  KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption
  { _kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOptionLogStreamARN :: Val Text
  } deriving (Show, Eq)

instance ToJSON KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption where
  toJSON KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption{..} =
    object $
    catMaybes
    [ (Just . ("LogStreamARN",) . toJSON) _kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOptionLogStreamARN
    ]

-- | Constructor for
-- 'KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption'
-- containing required fields as arguments.
kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption
  :: Val Text -- ^ 'kavacwlocwloLogStreamARN'
  -> KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption
kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption logStreamARNarg =
  KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption
  { _kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOptionLogStreamARN = logStreamARNarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisanalyticsv2-applicationcloudwatchloggingoption-cloudwatchloggingoption.html#cfn-kinesisanalyticsv2-applicationcloudwatchloggingoption-cloudwatchloggingoption-logstreamarn
kavacwlocwloLogStreamARN :: Lens' KinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOption (Val Text)
kavacwlocwloLogStreamARN = lens _kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOptionLogStreamARN (\s a -> s { _kinesisAnalyticsV2ApplicationCloudWatchLoggingOptionCloudWatchLoggingOptionLogStreamARN = a })
