module Common
  ( -- Note: Do not add more exports for Sqlite.Simple here.  This is
    -- so that we trap we by default export enough out of
    -- Database.Sqlite.Simple to make it useful as a single import.
    module Sqlite.Query,
    module Test.HUnit,
    TestEnv (..),
    Solo (..),
  )
where

import Sqlite.Query
import Test.HUnit

data TestEnv
  = TestEnv
  { -- | Connection shared by all the tests
    conn :: Connection
  }
