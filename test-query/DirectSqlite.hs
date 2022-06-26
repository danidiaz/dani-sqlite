{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module DirectSqlite (
    testDirectSqlite
  ) where

import Common

import Control.Exception (bracket)
import qualified SQLite as DS
import Data.Tuple (Solo(..))

testDirectSqlite :: TestEnv -> Test
testDirectSqlite TestEnv{..} = TestCase $ do
  let dsConn = conn
  bracket (DS.prepare dsConn "SELECT 1+1") DS.finalize testDirect
  [Solo (res :: Int)] <- query_ conn "SELECT 1+2"
  assertEqual "1+2" 3 res
  where
    testDirect stmt = do
      DS.Row <- DS.step stmt
      res <- DS.column stmt 0
      assertEqual "1+1" (SQLInteger 2) res
