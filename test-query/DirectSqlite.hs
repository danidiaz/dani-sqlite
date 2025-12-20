{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DirectSqlite
  ( testDirectSqlite,
  )
where

import Common
import Control.Exception (bracket)
import Sqlite qualified as DS

testDirectSqlite :: IO TestEnv -> Tasty.Assertion
testDirectSqlite ioenv = do
  TestEnv {..} <- ioenv
  let dsConn = conn
  bracket (DS.prepare dsConn "SELECT 1+1") DS.finalize testDirect
  [MkSolo (res :: Int)] <- select_ conn "SELECT 1+2"
  assertEqual "1+2" 3 res
  where
    testDirect stmt = do
      DS.Row <- DS.step stmt
      res <- DS.column stmt 0
      assertEqual "1+1" (SqlInteger 2) res
