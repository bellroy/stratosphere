
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-sourceselectioncriteria.html

module Stratosphere.ResourceProperties.S3BucketSourceSelectionCriteria where

import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.S3BucketSseKmsEncryptedObjects

-- | Full data type definition for S3BucketSourceSelectionCriteria. See
-- 's3BucketSourceSelectionCriteria' for a more convenient constructor.
data S3BucketSourceSelectionCriteria =
  S3BucketSourceSelectionCriteria
  { _s3BucketSourceSelectionCriteriaSseKmsEncryptedObjects :: S3BucketSseKmsEncryptedObjects
  } deriving (Show, Eq)

instance ToJSON S3BucketSourceSelectionCriteria where
  toJSON S3BucketSourceSelectionCriteria{..} =
    object $
    catMaybes
    [ (Just . ("SseKmsEncryptedObjects",) . toJSON) _s3BucketSourceSelectionCriteriaSseKmsEncryptedObjects
    ]

-- | Constructor for 'S3BucketSourceSelectionCriteria' containing required
-- fields as arguments.
s3BucketSourceSelectionCriteria
  :: S3BucketSseKmsEncryptedObjects -- ^ 'sbsscSseKmsEncryptedObjects'
  -> S3BucketSourceSelectionCriteria
s3BucketSourceSelectionCriteria sseKmsEncryptedObjectsarg =
  S3BucketSourceSelectionCriteria
  { _s3BucketSourceSelectionCriteriaSseKmsEncryptedObjects = sseKmsEncryptedObjectsarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket-sourceselectioncriteria.html#cfn-s3-bucket-sourceselectioncriteria-ssekmsencryptedobjects
sbsscSseKmsEncryptedObjects :: Lens' S3BucketSourceSelectionCriteria S3BucketSseKmsEncryptedObjects
sbsscSseKmsEncryptedObjects = lens _s3BucketSourceSelectionCriteriaSseKmsEncryptedObjects (\s a -> s { _s3BucketSourceSelectionCriteriaSseKmsEncryptedObjects = a })
