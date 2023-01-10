
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata.html

module Stratosphere.ResourceProperties.GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData where

import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.GreengrassResourceDefinitionVersionResourceDownloadOwnerSetting

-- | Full data type definition for
-- GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData.
-- See
-- 'greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData'
-- for a more convenient constructor.
data GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData =
  GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData
  { _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataDestinationPath :: Val Text
  , _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataOwnerSetting :: Maybe GreengrassResourceDefinitionVersionResourceDownloadOwnerSetting
  , _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataSageMakerJobArn :: Val Text
  } deriving (Show, Eq)

instance ToJSON GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData where
  toJSON GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData{..} =
    object $
    catMaybes
    [ (Just . ("DestinationPath",) . toJSON) _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataDestinationPath
    , fmap (("OwnerSetting",) . toJSON) _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataOwnerSetting
    , (Just . ("SageMakerJobArn",) . toJSON) _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataSageMakerJobArn
    ]

-- | Constructor for
-- 'GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData'
-- containing required fields as arguments.
greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData
  :: Val Text -- ^ 'grdvsmmlmrdDestinationPath'
  -> Val Text -- ^ 'grdvsmmlmrdSageMakerJobArn'
  -> GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData
greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData destinationPatharg sageMakerJobArnarg =
  GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData
  { _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataDestinationPath = destinationPatharg
  , _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataOwnerSetting = Nothing
  , _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataSageMakerJobArn = sageMakerJobArnarg
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata.html#cfn-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata-destinationpath
grdvsmmlmrdDestinationPath :: Lens' GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData (Val Text)
grdvsmmlmrdDestinationPath = lens _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataDestinationPath (\s a -> s { _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataDestinationPath = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata.html#cfn-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata-ownersetting
grdvsmmlmrdOwnerSetting :: Lens' GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData (Maybe GreengrassResourceDefinitionVersionResourceDownloadOwnerSetting)
grdvsmmlmrdOwnerSetting = lens _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataOwnerSetting (\s a -> s { _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataOwnerSetting = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata.html#cfn-greengrass-resourcedefinitionversion-sagemakermachinelearningmodelresourcedata-sagemakerjobarn
grdvsmmlmrdSageMakerJobArn :: Lens' GreengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceData (Val Text)
grdvsmmlmrdSageMakerJobArn = lens _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataSageMakerJobArn (\s a -> s { _greengrassResourceDefinitionVersionSageMakerMachineLearningModelResourceDataSageMakerJobArn = a })
