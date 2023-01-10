
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisanalyticsv2-application-applicationsnapshotconfiguration.html

module Stratosphere.ResourceProperties.KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for
-- KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration. See
-- 'kinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration' for a
-- more convenient constructor.
data KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration =
  KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration
  { _kinesisAnalyticsV2ApplicationApplicationSnapshotConfigurationSnapshotsEnabled :: Val Bool
  } deriving (Show, Eq)

instance ToJSON KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration where
  toJSON KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration{..} =
    object $
    catMaybes
    [ (Just . ("SnapshotsEnabled",) . toJSON) _kinesisAnalyticsV2ApplicationApplicationSnapshotConfigurationSnapshotsEnabled
    ]

-- | Constructor for
-- 'KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration'
-- containing required fields as arguments.
kinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration
  :: Val Bool -- ^ 'kavaascSnapshotsEnabled'
  -> KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration
kinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration snapshotsEnabledarg =
  KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration
  { _kinesisAnalyticsV2ApplicationApplicationSnapshotConfigurationSnapshotsEnabled = snapshotsEnabledarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisanalyticsv2-application-applicationsnapshotconfiguration.html#cfn-kinesisanalyticsv2-application-applicationsnapshotconfiguration-snapshotsenabled
kavaascSnapshotsEnabled :: Lens' KinesisAnalyticsV2ApplicationApplicationSnapshotConfiguration (Val Bool)
kavaascSnapshotsEnabled = lens _kinesisAnalyticsV2ApplicationApplicationSnapshotConfigurationSnapshotsEnabled (\s a -> s { _kinesisAnalyticsV2ApplicationApplicationSnapshotConfigurationSnapshotsEnabled = a })
