{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module System.Nix.Store.DB.Schema where


import           Data.Text (Text)

import qualified Database.Persist


import           Database.Persist.TH         ( AtLeastOneUniqueKey(..)
                                             , OnlyOneUniqueKey(..)
                                             , mkDeleteCascade
                                             , mkMigrate
                                             , mkPersist
                                             , share
                                             , sqlSettings
                                             )


import Data.Time (UTCTime)
import Data.Word (Word64)

import System.Nix.StorePath (StorePath, ContentAddressableAddress)
import System.Nix.StorePathMetadata (StorePathTrust(..))

import System.Nix.Store.DB.Instances
import System.Nix.Store.DB.Util

-- shcema version 10
-- cat /nix/var/nix/db/schema
-- 10

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll" ] [persistLikeNix|
  ValidPath
    path      StorePath
    hash      Text
    regTime   NixUTCTime sql=registrationTime
    deriver   StorePath Maybe
    narBytes  Word64 sql=narSize
    ultimate  StorePathTrust Maybe
    sigs      Text Maybe
    ca        ContentAddressableAddress Maybe
    deriving Eq Show Ord

  Ref
    referrer  ValidPathId
    reference ValidPathId

    Primary referrer reference
    deriving Eq Show Ord

  DerivationOutput
    drv      ValidPathId
    name     Text sql=id -- symbolic output id, usually "out"
    path     StorePath

    Primary drv name
    deriving Eq Show Ord
|]
