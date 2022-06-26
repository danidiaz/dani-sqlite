
module Common (
    -- Note: Do not add more exports for SQLite.Simple here.  This is
    -- so that we trap we by default export enough out of
    -- Database.SQLite.Simple to make it useful as a single import.
    module SQLite.Query
  , module Test.HUnit
  , TestEnv(..)
  , Solo (..)
) where

import Test.HUnit
import SQLite.Query
import Data.Tuple (Solo(..))

data TestEnv
    = TestEnv
        { conn     :: Connection
            -- ^ Connection shared by all the tests
        }
