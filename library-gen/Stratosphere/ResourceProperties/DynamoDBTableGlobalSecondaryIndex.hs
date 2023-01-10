
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-gsi.html

module Stratosphere.ResourceProperties.DynamoDBTableGlobalSecondaryIndex where

import Prelude
import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.DynamoDBTableKeySchema
import Stratosphere.ResourceProperties.DynamoDBTableProjection
import Stratosphere.ResourceProperties.DynamoDBTableProvisionedThroughput

-- | Full data type definition for DynamoDBTableGlobalSecondaryIndex. See
-- 'dynamoDBTableGlobalSecondaryIndex' for a more convenient constructor.
data DynamoDBTableGlobalSecondaryIndex =
  DynamoDBTableGlobalSecondaryIndex
  { _dynamoDBTableGlobalSecondaryIndexIndexName :: Val Text
  , _dynamoDBTableGlobalSecondaryIndexKeySchema :: [DynamoDBTableKeySchema]
  , _dynamoDBTableGlobalSecondaryIndexProjection :: DynamoDBTableProjection
  , _dynamoDBTableGlobalSecondaryIndexProvisionedThroughput :: Maybe DynamoDBTableProvisionedThroughput
  } deriving (Show, Eq)

instance ToJSON DynamoDBTableGlobalSecondaryIndex where
  toJSON DynamoDBTableGlobalSecondaryIndex{..} =
    object $
    catMaybes
    [ (Just . ("IndexName",) . toJSON) _dynamoDBTableGlobalSecondaryIndexIndexName
    , (Just . ("KeySchema",) . toJSON) _dynamoDBTableGlobalSecondaryIndexKeySchema
    , (Just . ("Projection",) . toJSON) _dynamoDBTableGlobalSecondaryIndexProjection
    , fmap (("ProvisionedThroughput",) . toJSON) _dynamoDBTableGlobalSecondaryIndexProvisionedThroughput
    ]

-- | Constructor for 'DynamoDBTableGlobalSecondaryIndex' containing required
-- fields as arguments.
dynamoDBTableGlobalSecondaryIndex
  :: Val Text -- ^ 'ddbtgsiIndexName'
  -> [DynamoDBTableKeySchema] -- ^ 'ddbtgsiKeySchema'
  -> DynamoDBTableProjection -- ^ 'ddbtgsiProjection'
  -> DynamoDBTableGlobalSecondaryIndex
dynamoDBTableGlobalSecondaryIndex indexNamearg keySchemaarg projectionarg =
  DynamoDBTableGlobalSecondaryIndex
  { _dynamoDBTableGlobalSecondaryIndexIndexName = indexNamearg
  , _dynamoDBTableGlobalSecondaryIndexKeySchema = keySchemaarg
  , _dynamoDBTableGlobalSecondaryIndexProjection = projectionarg
  , _dynamoDBTableGlobalSecondaryIndexProvisionedThroughput = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-gsi.html#cfn-dynamodb-gsi-indexname
ddbtgsiIndexName :: Lens' DynamoDBTableGlobalSecondaryIndex (Val Text)
ddbtgsiIndexName = lens _dynamoDBTableGlobalSecondaryIndexIndexName (\s a -> s { _dynamoDBTableGlobalSecondaryIndexIndexName = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-gsi.html#cfn-dynamodb-gsi-keyschema
ddbtgsiKeySchema :: Lens' DynamoDBTableGlobalSecondaryIndex [DynamoDBTableKeySchema]
ddbtgsiKeySchema = lens _dynamoDBTableGlobalSecondaryIndexKeySchema (\s a -> s { _dynamoDBTableGlobalSecondaryIndexKeySchema = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-gsi.html#cfn-dynamodb-gsi-projection
ddbtgsiProjection :: Lens' DynamoDBTableGlobalSecondaryIndex DynamoDBTableProjection
ddbtgsiProjection = lens _dynamoDBTableGlobalSecondaryIndexProjection (\s a -> s { _dynamoDBTableGlobalSecondaryIndexProjection = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-dynamodb-gsi.html#cfn-dynamodb-gsi-provisionedthroughput
ddbtgsiProvisionedThroughput :: Lens' DynamoDBTableGlobalSecondaryIndex (Maybe DynamoDBTableProvisionedThroughput)
ddbtgsiProvisionedThroughput = lens _dynamoDBTableGlobalSecondaryIndexProvisionedThroughput (\s a -> s { _dynamoDBTableGlobalSecondaryIndexProvisionedThroughput = a })
