{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module System.Nix.Store.DB.Instances where

import           Database.Persist (PersistField(..), PersistValue(..), SqlType(..))
import Database.Persist.Sql (PersistFieldSql(..))
--import Database.Persist.Sqlite
--import           Database.Persist.Sql.Class (PersistFieldSQL)

import Data.Time (UTCTime)

import System.Nix.StorePath (StorePath, ContentAddressableAddress)
import System.Nix.StorePathMetadata (StorePathTrust(..))

import qualified Data.ByteString.Char8
import qualified Data.Bifunctor
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Time.Clock.POSIX

import qualified System.Nix.Hash
import qualified System.Nix.StorePath
import qualified System.Nix.Store.Remote.Builders
import qualified System.Nix.Store.Remote.Parsers

instance PersistField StorePath where
  toPersistValue = PersistText . Data.Text.pack . show
  fromPersistValue (PersistText t) = either
    (Left . Data.Text.pack)
    Right
    $ Data.Attoparsec.Text.Lazy.parseOnly
      (System.Nix.StorePath.pathParser "/nix/store")
      t
  fromPersistValue wrongValue = Left
    $ Data.Text.pack
    $ "Received "
    ++ show wrongValue
    ++ " when a value of type PersistText was expected."

instance PersistFieldSql StorePath where
  sqlType _ = SqlString

-- XXX: handle errors
instance PersistField StorePathTrust where
  toPersistValue BuiltLocally     = PersistInt64 1
  toPersistValue BuiltElsewhere   = PersistNull
  fromPersistValue (PersistInt64 1) = pure BuiltLocally
  fromPersistValue _                = pure BuiltElsewhere

instance PersistFieldSql StorePathTrust where
  sqlType _ = SqlInt64

newtype NixUTCTime = NixUTCTime UTCTime
  deriving (Eq, Show, Ord)

instance PersistField NixUTCTime where
  toPersistValue (NixUTCTime u) = PersistInt64
    $ round $  Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds u
  fromPersistValue (PersistInt64 i) = pure $ NixUTCTime
    $ Data.Time.Clock.POSIX.posixSecondsToUTCTime $ fromIntegral i

instance PersistFieldSql NixUTCTime where
  sqlType _ = SqlInt64

instance PersistField ContentAddressableAddress where
  toPersistValue =
    PersistText
    . Data.Text.Lazy.toStrict
    . System.Nix.Store.Remote.Builders.buildContentAddressableAddress

  fromPersistValue (PersistText t) =
    Data.Bifunctor.first (\e -> error $ show (e, t))
    -- This is to adjust type to Either Text ... for error path
    -- but errors result in unhelpful
    -- *** Exception: PersistMarshalError "Couldn't parse field `ca` from table `ValidPaths`. string"
    -- so explicit error version above is used instead for now
    --
    -- Data.Bifunctor.first Data.Text.pack
    $ System.Nix.Store.Remote.Parsers.parseContentAddressableAddress
        (Data.ByteString.Char8.pack $ Data.Text.unpack t)

instance PersistFieldSql ContentAddressableAddress where
  sqlType _ = SqlString
