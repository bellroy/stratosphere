{-# OPTIONS -Wno-redundant-constraints #-}

-- | `Stratosphere.Check` exports functions to catch errors
-- that would be too expensive or unwieldy to encode in types.
--
-- Stability: Experimental

module Stratosphere.Check (duplicateProperties) where

import Data.Hashable (Hashable)
import Prelude
import Stratosphere.Resources (_resourceName, unResources)
import Stratosphere.Template (Template, _templateResources)

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

newtype DuplicateProperty = DuplicateProperty T.Text
  deriving (Show, Eq)

duplicateProperties :: Template -> [DuplicateProperty]
duplicateProperties =
    map DuplicateProperty
  . duplicates
  . map _resourceName
  . unResources
  . _templateResources

duplicates :: (Foldable f, Eq a, Hashable a) => f a -> [a]
duplicates =
  HM.keys . HM.filter (> one) . foldr (insertByAdding one) HM.empty
  where one :: Int
        one = 1

insertByAdding :: (Eq k, Hashable k, Num v) => v -> k -> HM.HashMap k v -> HM.HashMap k v
insertByAdding = flip $ HM.insertWith (+)
