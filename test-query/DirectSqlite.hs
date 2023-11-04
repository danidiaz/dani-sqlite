{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DirectSqlite (
    testDirectSqlite
  ) where

import Common

import Control.Exception (bracket)
import qualified Sqlite as DS

testDirectSqlite :: TestEnv -> Test
testDirectSqlite TestEnv{..} = TestCase $ do
  let dsConn = conn
  bracket (DS.prepare dsConn "SELECT 1+1") DS.finalize testDirect
  [MkSolo (res :: Int)] <- select_ conn "SELECT 1+2"
  assertEqual "1+2" 3 res
  where
    testDirect stmt = do
      DS.Row <- DS.step stmt
      res <- DS.column stmt 0
      assertEqual "1+1" (SqlInteger 2) res
