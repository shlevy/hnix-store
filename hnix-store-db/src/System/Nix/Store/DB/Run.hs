{-# LANGUAGE OverloadedStrings          #-}
module System.Nix.Store.DB.Run where

import           System.Nix.StorePath        (StorePath)
import           Database.Esqueleto

import           Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)

import qualified Control.Monad
import qualified Control.Monad.Logger
import qualified Control.Monad.Trans.Resource
import qualified Database.Persist.Sqlite
import qualified Safe

import           System.Nix.Store.DB.Schema
import           System.Nix.Store.DB.Query


{--
memTest :: IO ()
memTest = runner' ":memory:" $ do
  runMigration migrateAll
  queryAll
  return ()
--}

queryAll = (,,)
  <$> queryValidPaths
  <*> queryAllRefs
  <*> queryAllDerivationOutputs

runner con act =
    Control.Monad.Trans.Resource.runResourceT
  $ Control.Monad.Logger.runStdoutLoggingT
  $ Database.Persist.Sqlite.withSqliteConn con
  . runSqlConn
  $ act

runner' con act = Database.Persist.Sqlite.runSqlite con act

--runSystemSqliteDebug = runner "/nix/var/nix/db/db.sqlite"
--runSystemSqlite :: (Control.Monad.Logger.MonadLogger m) =>
--  (ReaderT SqlBackend (m t) (Control.Monad.Trans.Resource.ResourceT IO)) a -> IO a
runSystemSqlite = runner "/nix/var/nix/db/db.sqlite"

test :: IO ()
test = do
  runSystemSqlite $ do
    -- won't currently work due to FOREIGN KEY constraints
    --runMigration migrateAll
    (paths, refs, drvOuts) <- queryAll

    liftIO $ do
      putStrLn $ "Stats: "
      let stat name v = putStrLn $ "- " ++ name ++ ": " ++ show (length v)
      stat "ValidPath(s)" paths
      stat "Ref(s)" refs
      stat "DerivationOutput(s)" drvOuts

    mo <- queryOneValidDerivation
    case Safe.headMay mo of
      Nothing -> return ()
      Just o -> do
        let pth = validPathPath $ entityVal o
            pid = entityKey o

        (same, references, referrers, validDerivers, outputs) <- (,,,,)
          <$> (Safe.headMay <$> queryPathInfo pth)
          <*> queryReferences pid
          <*> queryReferrers pth
          <*> queryValidDerivers pth
          <*> queryDerivationOutputs pid

        Control.Monad.when (same /= Just o) $ error "queryPathInfo failed to roundtrip"
        liftIO $ do
          putStrLn $ "References: "
          print references
          putStrLn $ "Referrers: "
          print referrers
          putStrLn $ "Valid derivers: "
          print validDerivers
          putStrLn $ "Derivation outputs: "
          print outputs

    return ()

-- | Query everything and for each valid path
-- perform detailed queries
bench :: IO ()
bench = do
  runSystemSqlite $ do
    -- won't currently work due to FOREIGN KEY constraints
    --runMigration migrateAll
    (paths, refs, drvOuts) <- queryAll
    liftIO $ do
      putStrLn $ "Stats: "
      let stat name v = putStrLn $ "- " ++ name ++ ": " ++ show (length v)
      stat "ValidPath(s)" paths
      stat "Ref(s)" refs
      stat "DerivationOutput(s)" drvOuts

    Control.Monad.forM_ paths proc
  where
    proc o = do
      let pth = validPathPath $ entityVal o
          pid = entityKey o

      (same, _references, _referrers, _validDerivers, _outputs) <- (,,,,)
        <$> (Safe.headMay <$> queryPathInfo pth)
        <*> queryReferences pid
        <*> queryReferrers pth
        <*> queryValidDerivers pth
        <*> queryDerivationOutputs pid

      Control.Monad.when (same /= Just o) $ error "queryPathInfo failed to roundtrip"
