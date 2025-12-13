{-# LANGUAGE OverloadedStrings #-}

module TestImports
  ( testImports,
  )
where

-- Test file to test that we can do most things with a single import

import Common
import Control.Exception
import Data.Text qualified as T
import Sqlite

data TestType = TestType Int Int Int

-- Hook up sqlite-simple to know how to read Test rows
instance FromRow TestType where
  fromRow = TestType <$> field <*> field <*> field

test1 :: IO ()
test1 = do
  conn <- open ":memory:"
  execute_ conn "CREATE TABLE testimp (id INTEGER PRIMARY KEY, id2 INTEGER, id3 INTEGER)"
  execute_ conn "INSERT INTO testimp (id, id2, id3) VALUES (1, 2, 3)"
  [_v] <- select_ conn "SELECT * FROM testimp" :: IO [TestType]
  [_v] <- select conn "SELECT ?+?" (3 :: Int, 4 :: Int) :: IO [(Solo Int)]
  close conn

test2 :: Connection -> IO ()
test2 conn = do
  execute_ conn "CREATE TABLE testimp (id INTEGER PRIMARY KEY)"
  execute_ conn "INSERT INTO testimp (id) VALUES (1)"
  [MkSolo _v] <- select_ conn (Sql q) :: IO [Solo Int]
  return ()
  where
    q = T.concat ["SELECT * FROM ", "testimp"]

test3 :: Connection -> IO ()
test3 conn = do
  [_v] <- select conn "SELECT ?+?" (3 :: Int, 4 :: Int) :: IO [(Solo Int)]
  return ()

testImports :: TestEnv -> Test
testImports env = TestCase $ do
  test1
  bracket (open ":memory:") close test2
  test3 (conn env)
