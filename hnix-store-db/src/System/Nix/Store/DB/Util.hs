{-# LANGUAGE OverloadedStrings          #-}

module System.Nix.Store.DB.Util where

import Database.Persist.Quasi
import Database.Persist.TH (persistWith)

persistLikeNix = persistWith $ upperCaseSettings {
    psToDBName = coerce . (psToDBName upperCaseSettings)
  }
  where
    coerce x | x `elem` ["ValidPath", "Ref", "DerivationOutput"] = plural x
    coerce x = x

    plural x = x <> "s"


