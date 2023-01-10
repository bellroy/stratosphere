
-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html

module Stratosphere.ResourceProperties.FSxFileSystemWindowsConfiguration where

import Stratosphere.ResourceImports
import Stratosphere.ResourceProperties.FSxFileSystemSelfManagedActiveDirectoryConfiguration

-- | Full data type definition for FSxFileSystemWindowsConfiguration. See
-- 'fSxFileSystemWindowsConfiguration' for a more convenient constructor.
data FSxFileSystemWindowsConfiguration =
  FSxFileSystemWindowsConfiguration
  { _fSxFileSystemWindowsConfigurationActiveDirectoryId :: Maybe (Val Text)
  , _fSxFileSystemWindowsConfigurationAutomaticBackupRetentionDays :: Maybe (Val Integer)
  , _fSxFileSystemWindowsConfigurationCopyTagsToBackups :: Maybe (Val Bool)
  , _fSxFileSystemWindowsConfigurationDailyAutomaticBackupStartTime :: Maybe (Val Text)
  , _fSxFileSystemWindowsConfigurationDeploymentType :: Maybe (Val Text)
  , _fSxFileSystemWindowsConfigurationPreferredSubnetId :: Maybe (Val Text)
  , _fSxFileSystemWindowsConfigurationSelfManagedActiveDirectoryConfiguration :: Maybe FSxFileSystemSelfManagedActiveDirectoryConfiguration
  , _fSxFileSystemWindowsConfigurationThroughputCapacity :: Maybe (Val Integer)
  , _fSxFileSystemWindowsConfigurationWeeklyMaintenanceStartTime :: Maybe (Val Text)
  } deriving (Show, Eq)

instance ToJSON FSxFileSystemWindowsConfiguration where
  toJSON FSxFileSystemWindowsConfiguration{..} =
    object $
    catMaybes
    [ fmap (("ActiveDirectoryId",) . toJSON) _fSxFileSystemWindowsConfigurationActiveDirectoryId
    , fmap (("AutomaticBackupRetentionDays",) . toJSON) _fSxFileSystemWindowsConfigurationAutomaticBackupRetentionDays
    , fmap (("CopyTagsToBackups",) . toJSON) _fSxFileSystemWindowsConfigurationCopyTagsToBackups
    , fmap (("DailyAutomaticBackupStartTime",) . toJSON) _fSxFileSystemWindowsConfigurationDailyAutomaticBackupStartTime
    , fmap (("DeploymentType",) . toJSON) _fSxFileSystemWindowsConfigurationDeploymentType
    , fmap (("PreferredSubnetId",) . toJSON) _fSxFileSystemWindowsConfigurationPreferredSubnetId
    , fmap (("SelfManagedActiveDirectoryConfiguration",) . toJSON) _fSxFileSystemWindowsConfigurationSelfManagedActiveDirectoryConfiguration
    , fmap (("ThroughputCapacity",) . toJSON) _fSxFileSystemWindowsConfigurationThroughputCapacity
    , fmap (("WeeklyMaintenanceStartTime",) . toJSON) _fSxFileSystemWindowsConfigurationWeeklyMaintenanceStartTime
    ]

-- | Constructor for 'FSxFileSystemWindowsConfiguration' containing required
-- fields as arguments.
fSxFileSystemWindowsConfiguration
  :: FSxFileSystemWindowsConfiguration
fSxFileSystemWindowsConfiguration  =
  FSxFileSystemWindowsConfiguration
  { _fSxFileSystemWindowsConfigurationActiveDirectoryId = Nothing
  , _fSxFileSystemWindowsConfigurationAutomaticBackupRetentionDays = Nothing
  , _fSxFileSystemWindowsConfigurationCopyTagsToBackups = Nothing
  , _fSxFileSystemWindowsConfigurationDailyAutomaticBackupStartTime = Nothing
  , _fSxFileSystemWindowsConfigurationDeploymentType = Nothing
  , _fSxFileSystemWindowsConfigurationPreferredSubnetId = Nothing
  , _fSxFileSystemWindowsConfigurationSelfManagedActiveDirectoryConfiguration = Nothing
  , _fSxFileSystemWindowsConfigurationThroughputCapacity = Nothing
  , _fSxFileSystemWindowsConfigurationWeeklyMaintenanceStartTime = Nothing
  }

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-activedirectoryid
fsfswcActiveDirectoryId :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Text))
fsfswcActiveDirectoryId = lens _fSxFileSystemWindowsConfigurationActiveDirectoryId (\s a -> s { _fSxFileSystemWindowsConfigurationActiveDirectoryId = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-automaticbackupretentiondays
fsfswcAutomaticBackupRetentionDays :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Integer))
fsfswcAutomaticBackupRetentionDays = lens _fSxFileSystemWindowsConfigurationAutomaticBackupRetentionDays (\s a -> s { _fSxFileSystemWindowsConfigurationAutomaticBackupRetentionDays = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-copytagstobackups
fsfswcCopyTagsToBackups :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Bool))
fsfswcCopyTagsToBackups = lens _fSxFileSystemWindowsConfigurationCopyTagsToBackups (\s a -> s { _fSxFileSystemWindowsConfigurationCopyTagsToBackups = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-dailyautomaticbackupstarttime
fsfswcDailyAutomaticBackupStartTime :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Text))
fsfswcDailyAutomaticBackupStartTime = lens _fSxFileSystemWindowsConfigurationDailyAutomaticBackupStartTime (\s a -> s { _fSxFileSystemWindowsConfigurationDailyAutomaticBackupStartTime = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-deploymenttype
fsfswcDeploymentType :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Text))
fsfswcDeploymentType = lens _fSxFileSystemWindowsConfigurationDeploymentType (\s a -> s { _fSxFileSystemWindowsConfigurationDeploymentType = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-preferredsubnetid
fsfswcPreferredSubnetId :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Text))
fsfswcPreferredSubnetId = lens _fSxFileSystemWindowsConfigurationPreferredSubnetId (\s a -> s { _fSxFileSystemWindowsConfigurationPreferredSubnetId = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-selfmanagedactivedirectoryconfiguration
fsfswcSelfManagedActiveDirectoryConfiguration :: Lens' FSxFileSystemWindowsConfiguration (Maybe FSxFileSystemSelfManagedActiveDirectoryConfiguration)
fsfswcSelfManagedActiveDirectoryConfiguration = lens _fSxFileSystemWindowsConfigurationSelfManagedActiveDirectoryConfiguration (\s a -> s { _fSxFileSystemWindowsConfigurationSelfManagedActiveDirectoryConfiguration = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-throughputcapacity
fsfswcThroughputCapacity :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Integer))
fsfswcThroughputCapacity = lens _fSxFileSystemWindowsConfigurationThroughputCapacity (\s a -> s { _fSxFileSystemWindowsConfigurationThroughputCapacity = a })

-- | http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-fsx-filesystem-windowsconfiguration.html#cfn-fsx-filesystem-windowsconfiguration-weeklymaintenancestarttime
fsfswcWeeklyMaintenanceStartTime :: Lens' FSxFileSystemWindowsConfiguration (Maybe (Val Text))
fsfswcWeeklyMaintenanceStartTime = lens _fSxFileSystemWindowsConfigurationWeeklyMaintenanceStartTime (\s a -> s { _fSxFileSystemWindowsConfigurationWeeklyMaintenanceStartTime = a })
