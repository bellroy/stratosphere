
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html

module Stratosphere.ResourceProperties.S3BucketReplicationDestination where

import Prelude
import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.S3BucketAccessControlTranslation
import Stratosphere.ResourceProperties.S3BucketEncryptionConfiguration
import Stratosphere.ResourceProperties.S3BucketMetrics
import Stratosphere.ResourceProperties.S3BucketReplicationTime

-- | Full data type definition for S3BucketReplicationDestination. See
-- 's3BucketReplicationDestination' for a more convenient constructor.
data S3BucketReplicationDestination =
  S3BucketReplicationDestination
  { _s3BucketReplicationDestinationAccessControlTranslation :: Maybe S3BucketAccessControlTranslation
  , _s3BucketReplicationDestinationAccount :: Maybe (Val Text)
  , _s3BucketReplicationDestinationBucket :: Val Text
  , _s3BucketReplicationDestinationEncryptionConfiguration :: Maybe S3BucketEncryptionConfiguration
  , _s3BucketReplicationDestinationMetrics :: Maybe S3BucketMetrics
  , _s3BucketReplicationDestinationReplicationTime :: Maybe S3BucketReplicationTime
  , _s3BucketReplicationDestinationStorageClass :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON S3BucketReplicationDestination where
  toJSON S3BucketReplicationDestination{..} =
    object $
    catMaybes
    [ fmap (("AccessControlTranslation",) . toJSON) _s3BucketReplicationDestinationAccessControlTranslation
    , fmap (("Account",) . toJSON) _s3BucketReplicationDestinationAccount
    , (Just . ("Bucket",) . toJSON) _s3BucketReplicationDestinationBucket
    , fmap (("EncryptionConfiguration",) . toJSON) _s3BucketReplicationDestinationEncryptionConfiguration
    , fmap (("Metrics",) . toJSON) _s3BucketReplicationDestinationMetrics
    , fmap (("ReplicationTime",) . toJSON) _s3BucketReplicationDestinationReplicationTime
    , fmap (("StorageClass",) . toJSON) _s3BucketReplicationDestinationStorageClass
    ]

-- | Constructor for 'S3BucketReplicationDestination' containing required
-- fields as arguments.
s3BucketReplicationDestination
  :: Val Text -- ^ 'sbrdBucket'
  -> S3BucketReplicationDestination
s3BucketReplicationDestination bucketarg =
  S3BucketReplicationDestination
  { _s3BucketReplicationDestinationAccessControlTranslation = Nothing
  , _s3BucketReplicationDestinationAccount = Nothing
  , _s3BucketReplicationDestinationBucket = bucketarg
  , _s3BucketReplicationDestinationEncryptionConfiguration = Nothing
  , _s3BucketReplicationDestinationMetrics = Nothing
  , _s3BucketReplicationDestinationReplicationTime = Nothing
  , _s3BucketReplicationDestinationStorageClass = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationdestination-accesscontroltranslation
sbrdAccessControlTranslation :: Lens' S3BucketReplicationDestination (Maybe S3BucketAccessControlTranslation)
sbrdAccessControlTranslation = lens _s3BucketReplicationDestinationAccessControlTranslation (\s a -> s { _s3BucketReplicationDestinationAccessControlTranslation = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationdestination-account
sbrdAccount :: Lens' S3BucketReplicationDestination (Maybe (Val Text))
sbrdAccount = lens _s3BucketReplicationDestinationAccount (\s a -> s { _s3BucketReplicationDestinationAccount = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationconfiguration-rules-destination-bucket
sbrdBucket :: Lens' S3BucketReplicationDestination (Val Text)
sbrdBucket = lens _s3BucketReplicationDestinationBucket (\s a -> s { _s3BucketReplicationDestinationBucket = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationdestination-encryptionconfiguration
sbrdEncryptionConfiguration :: Lens' S3BucketReplicationDestination (Maybe S3BucketEncryptionConfiguration)
sbrdEncryptionConfiguration = lens _s3BucketReplicationDestinationEncryptionConfiguration (\s a -> s { _s3BucketReplicationDestinationEncryptionConfiguration = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationdestination-metrics
sbrdMetrics :: Lens' S3BucketReplicationDestination (Maybe S3BucketMetrics)
sbrdMetrics = lens _s3BucketReplicationDestinationMetrics (\s a -> s { _s3BucketReplicationDestinationMetrics = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationdestination-replicationtime
sbrdReplicationTime :: Lens' S3BucketReplicationDestination (Maybe S3BucketReplicationTime)
sbrdReplicationTime = lens _s3BucketReplicationDestinationReplicationTime (\s a -> s { _s3BucketReplicationDestinationReplicationTime = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-replicationconfiguration-rules-destination.html#cfn-s3-bucket-replicationconfiguration-rules-destination-storageclass
sbrdStorageClass :: Lens' S3BucketReplicationDestination (Maybe (Val Text))
sbrdStorageClass = lens _s3BucketReplicationDestinationStorageClass (\s a -> s { _s3BucketReplicationDestinationStorageClass = a })
