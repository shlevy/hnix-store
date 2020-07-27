module System.Nix.Store.DB.Query where

import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger)
import           Data.Text (Text)
import           Database.Esqueleto


import           System.Nix.StorePath        (StorePath)
import           System.Nix.StorePathMetadata (StorePathTrust(..))

import           System.Nix.Store.DB.Schema

queryPathInfo :: (MonadIO m, MonadLogger m)
              => StorePath
              -> SqlPersistT m ([Entity ValidPath])
queryPathInfo path = select $ from $ \validPath -> do
  where_ (validPath ^. ValidPathPath ==. val path)
  return validPath

queryReferences :: (MonadIO m, MonadLogger m)
              => ValidPathId
              -> SqlPersistT m ([Entity Ref])
queryReferences referrer = select $ from $ \(ref `InnerJoin` validPath) -> do
  on (ref ^. RefReference ==. validPath ^. ValidPathId)
  where_ (ref ^. RefReferrer ==. val referrer)
  return ref

queryReferrers :: (MonadIO m, MonadLogger m)
               => StorePath
               -> SqlPersistT m ([Entity Ref])
queryReferrers path = select $ from $ \(ref `InnerJoin` validPath) -> do
  let sub = subList_select $ from $ \vp -> do
        where_ (vp ^. ValidPathPath ==. val path)
        return $ vp ^. ValidPathId
  on (ref ^. RefReference ==. validPath ^. ValidPathId)
  where_ (ref ^. RefReference `in_` sub)
  return ref

queryValidDerivers :: (MonadIO m, MonadLogger m)
              => StorePath
              -> SqlPersistT m ([(Value Text, Value StorePath)])
queryValidDerivers path = select $ from $ \(drvOut `InnerJoin` validPath) -> do
  on (drvOut ^. DerivationOutputDrv ==. validPath ^. ValidPathId)
  where_ (drvOut ^. DerivationOutputPath ==. val path)
  return (drvOut ^. DerivationOutputName, drvOut ^. DerivationOutputPath)

queryDerivationOutputs :: (MonadIO m, MonadLogger m)
                       => ValidPathId
                       -> SqlPersistT m ([(Value Text, Value StorePath)])
queryDerivationOutputs drv = select $ from $ \drvOut -> do
  where_ (drvOut ^. DerivationOutputDrv ==. val drv)
  return (drvOut ^. DerivationOutputName, drvOut ^. DerivationOutputPath)

-- XXX: problematic, we need a way to format without -name
-- or use Text
queryPathFromHashPart :: (MonadIO m, MonadLogger m)
                       => StorePath
                       -> SqlPersistT m ([Value StorePath])
queryPathFromHashPart hashPart = select $ from $ \validPath -> do
  where_ (validPath ^. ValidPathPath >=. val hashPart)
  limit 1
  return (validPath ^. ValidPathPath)

queryValidPaths :: (MonadIO m, MonadLogger m)
                => SqlPersistT m ([Entity ValidPath])
queryValidPaths = select $ from return

-- for testing
queryAllRefs :: (MonadIO m, MonadLogger m)
             => SqlPersistT m ([Entity Ref])
queryAllRefs = select $ from return

queryAllDerivationOutputs :: (MonadIO m, MonadLogger m)
                          => SqlPersistT m ([Entity DerivationOutput])
queryAllDerivationOutputs = select $ from return

queryOneValidDerivation :: (MonadIO m, MonadLogger m)
                        => SqlPersistT m ([Entity ValidPath])
queryOneValidDerivation = select $ from $ \validPath -> do
  where_ (validPath ^. ValidPathUltimate ==. (val $ Just BuiltLocally))
  offset 100
  limit 1
  return validPath
