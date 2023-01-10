
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html

module Stratosphere.ResourceProperties.OpsWorksStackSource where

import Stratosphere.ResourceImports


-- | Full data type definition for OpsWorksStackSource. See
-- 'opsWorksStackSource' for a more convenient constructor.
data OpsWorksStackSource =
  OpsWorksStackSource
  { _opsWorksStackSourcePassword :: Maybe (Val Text)
  , _opsWorksStackSourceRevision :: Maybe (Val Text)
  , _opsWorksStackSourceSshKey :: Maybe (Val Text)
  , _opsWorksStackSourceType :: Maybe (Val Text)
  , _opsWorksStackSourceUrl :: Maybe (Val Text)
  , _opsWorksStackSourceUsername :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON OpsWorksStackSource where
  toJSON OpsWorksStackSource{..} =
    object $
    catMaybes
    [ fmap (("Password",) . toJSON) _opsWorksStackSourcePassword
    , fmap (("Revision",) . toJSON) _opsWorksStackSourceRevision
    , fmap (("SshKey",) . toJSON) _opsWorksStackSourceSshKey
    , fmap (("Type",) . toJSON) _opsWorksStackSourceType
    , fmap (("Url",) . toJSON) _opsWorksStackSourceUrl
    , fmap (("Username",) . toJSON) _opsWorksStackSourceUsername
    ]

-- | Constructor for 'OpsWorksStackSource' containing required fields as
-- arguments.
opsWorksStackSource
  :: OpsWorksStackSource
opsWorksStackSource  =
  OpsWorksStackSource
  { _opsWorksStackSourcePassword = Nothing
  , _opsWorksStackSourceRevision = Nothing
  , _opsWorksStackSourceSshKey = Nothing
  , _opsWorksStackSourceType = Nothing
  , _opsWorksStackSourceUrl = Nothing
  , _opsWorksStackSourceUsername = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-password
owssPassword :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssPassword = lens _opsWorksStackSourcePassword (\s a -> s { _opsWorksStackSourcePassword = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-revision
owssRevision :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssRevision = lens _opsWorksStackSourceRevision (\s a -> s { _opsWorksStackSourceRevision = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-sshkey
owssSshKey :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssSshKey = lens _opsWorksStackSourceSshKey (\s a -> s { _opsWorksStackSourceSshKey = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-type
owssType :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssType = lens _opsWorksStackSourceType (\s a -> s { _opsWorksStackSourceType = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-url
owssUrl :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssUrl = lens _opsWorksStackSourceUrl (\s a -> s { _opsWorksStackSourceUrl = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-opsworks-stack-source.html#cfn-opsworks-custcookbooksource-username
owssUsername :: Lens' OpsWorksStackSource (Maybe (Val Text))
owssUsername = lens _opsWorksStackSourceUsername (\s a -> s { _opsWorksStackSourceUsername = a })
