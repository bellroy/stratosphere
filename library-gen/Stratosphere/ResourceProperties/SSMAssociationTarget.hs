
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssm-association-target.html

module Stratosphere.ResourceProperties.SSMAssociationTarget where

import Prelude
import Stratosphere.ResourceImports


-- | Full data type definition for SSMAssociationTarget. See
-- 'ssmAssociationTarget' for a more convenient constructor.
data SSMAssociationTarget =
  SSMAssociationTarget
  { _sSMAssociationTargetKey :: Val Text
  , _sSMAssociationTargetValues :: ValList Text
  } deriving (Show, Eq)

instance ToJSON SSMAssociationTarget where
  toJSON SSMAssociationTarget{..} =
    object $
    catMaybes
    [ (Just . ("Key",) . toJSON) _sSMAssociationTargetKey
    , (Just . ("Values",) . toJSON) _sSMAssociationTargetValues
    ]

-- | Constructor for 'SSMAssociationTarget' containing required fields as
-- arguments.
ssmAssociationTarget
  :: Val Text -- ^ 'ssmatKey'
  -> ValList Text -- ^ 'ssmatValues'
  -> SSMAssociationTarget
ssmAssociationTarget keyarg valuesarg =
  SSMAssociationTarget
  { _sSMAssociationTargetKey = keyarg
  , _sSMAssociationTargetValues = valuesarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssm-association-target.html#cfn-ssm-association-target-key
ssmatKey :: Lens' SSMAssociationTarget (Val Text)
ssmatKey = lens _sSMAssociationTargetKey (\s a -> s { _sSMAssociationTargetKey = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-ssm-association-target.html#cfn-ssm-association-target-values
ssmatValues :: Lens' SSMAssociationTarget (ValList Text)
ssmatValues = lens _sSMAssociationTargetValues (\s a -> s { _sSMAssociationTargetValues = a })
