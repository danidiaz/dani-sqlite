{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PreparedStatement
  ( testBind,
    testDoubleBind,
    testPreparedStatements,
    testPreparedStatementsColumnCount,
  )
where

import Common
  ( ColumnIndex (ColumnIndex),
    Solo (..),
    TestEnv (..),
    columnCount,
    columnName,
    execute_,
    nextRow,
    withBind,
    withStatement,
  )
import Data.Maybe (fromJust)
import Sqlite qualified as Base
import Test.Tasty.HUnit
import Test.Tasty.HUnit qualified as Tasty

testBind :: IO TestEnv -> Tasty.Assertion
testBind ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE test_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_bind VALUES(1, 'result')"
  withStatement conn "SELECT t FROM test_bind WHERE id=?" $ \stmt ->
    withBind stmt [1 :: Int] $ do
      row <- nextRow stmt :: IO (Maybe (Solo String))
      Tasty.assertEqual "result" (MkSolo "result") (fromJust row)

testDoubleBind :: IO TestEnv -> Tasty.Assertion
testDoubleBind ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE test_double_bind (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test_double_bind VALUES(1, 'first result')"
  execute_ conn "INSERT INTO test_double_bind VALUES(2, 'second result')"
  withStatement conn "SELECT t FROM test_double_bind WHERE id=?" $ \stmt -> do
    withBind stmt [1 :: Int] $ do
      row <- nextRow stmt :: IO (Maybe (Solo String))
      Tasty.assertEqual "first result" (MkSolo "first result") (fromJust row)

    withBind stmt [2 :: Int] $ do
      row <- nextRow stmt :: IO (Maybe (Solo String))
      Tasty.assertEqual "second result" (MkSolo "second result") (fromJust row)

testPreparedStatements :: IO TestEnv -> Tasty.Assertion
testPreparedStatements ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE ps (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO ps VALUES(1, 'first result')"
  execute_ conn "INSERT INTO ps VALUES(2, 'second result')"
  withStatement conn "SELECT t FROM ps WHERE id=?" $ \stmt -> do
    colName <- columnName stmt 0
    colName @?= "t"
    elems <- mapM (queryOne stmt) [1 :: Int, 2]
    ["first result" :: String, "second result"] @=? elems
  where
    queryOne stmt rowId =
      withBind stmt (MkSolo rowId) $ do
        Just (MkSolo r) <- nextRow stmt
        Nothing <- nextRow stmt :: IO (Maybe (Solo String))
        return r

testPreparedStatementsColumnCount :: IO TestEnv -> Tasty.Assertion
testPreparedStatementsColumnCount ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE ps2 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO ps2 VALUES(1, 'first result')"
  withStatement conn "SELECT t FROM ps2 WHERE id=?" $ \stmt -> do
    colName <- columnName stmt 0
    colName @?= "t"
    ColumnIndex colCount <- columnCount stmt
    colCount @?= 1
    let baseStatment = stmt
    colCountBase <- Base.columnCount baseStatment
    colCountBase @?= 1
