{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Query
  ( testSimpleOnePlusOne,
    testSimpleSelect,
    testOutOfRangeParserSelect,
    testSimpleParams,
    testSimpleInsertId,
    testSimpleMultiInsert,
    testSimpleQueryCov,
    testSimpleStrings,
    testSimpleChanges,
  )
where

-- orphan IsString instance in older byteString

import Common
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 ()
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Control.Exception

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- Simplest SELECT
testSimpleOnePlusOne :: TestEnv -> Test
testSimpleOnePlusOne TestEnv {..} = TestCase $ do
  rows <- select_ conn "SELECT 1+1" :: IO [Solo Int]
  assertEqual "row count" 1 (length rows)
  assertEqual "value" (MkSolo 2) (head rows)

testSimpleSelect :: TestEnv -> Test
testSimpleSelect TestEnv {..} = TestCase $ do
  execute_ conn "CREATE TABLE test1 (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string')"
  rows <- select_ conn "SELECT t FROM test1" :: IO [Solo String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (MkSolo "test string") (head rows)
  rows <- select_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "int,string" (1, "test string") (head rows)
  -- Add another row
  execute_ conn "INSERT INTO test1 (t) VALUES ('test string 2')"
  rows <- select_ conn "SELECT id,t FROM test1" :: IO [(Int, String)]
  assertEqual "row count" 2 (length rows)
  assertEqual "int,string" (1, "test string") (rows !! 0)
  assertEqual "int,string" (2, "test string 2") (rows !! 1)
  [MkSolo r] <- select_ conn "SELECT NULL" :: IO [Solo (Maybe Int)]
  assertEqual "nulls" Nothing r
  [MkSolo r] <- select_ conn "SELECT 1" :: IO [Solo (Maybe Int)]
  assertEqual "nulls" (Just 1) r
  [MkSolo r] <- select_ conn "SELECT 1.0" :: IO [Solo Double]
  assertEqual "doubles" 1.0 r
  [MkSolo r] <- select_ conn "SELECT 1.0" :: IO [Solo Float]
  assertEqual "floats" 1.0 r

testOutOfRangeParserSelect :: TestEnv -> Test
testOutOfRangeParserSelect TestEnv {..} = TestCase $ do
  e <- try @ResultError (select_ conn "SELECT 1" :: IO [(Int, Int)])
  case e of
    Left (ConversionFailed {}) -> pure ()
    Left _ -> assertFailure "Actually, we expected another type of error here!"
    Right _ -> assertFailure "Actually, we expected an error here!"

testSimpleParams :: TestEnv -> Test
testSimpleParams TestEnv {..} = TestCase $ do
  execute_ conn "CREATE TABLE testparams (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "CREATE TABLE testparams2 (id INTEGER, t TEXT, t2 TEXT)"
  [MkSolo i] <- select conn "SELECT ?" (MkSolo (42 :: Int)) :: IO [Solo Int]
  assertEqual "select int param" 42 i
  execute conn "INSERT INTO testparams (t) VALUES (?)" (MkSolo ("test string" :: String))
  rows <- select conn "SELECT t FROM testparams WHERE id = ?" (MkSolo (1 :: Int)) :: IO [Solo String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (MkSolo "test string") (head rows)
  execute_ conn "INSERT INTO testparams (t) VALUES ('test2')"
  [MkSolo row] <- select conn "SELECT t FROM testparams WHERE id = ?" (MkSolo (1 :: Int)) :: IO [Solo String]
  assertEqual "select params" "test string" row
  [MkSolo row] <- select conn "SELECT t FROM testparams WHERE id = ?" (MkSolo (2 :: Int)) :: IO [Solo String]
  assertEqual "select params" "test2" row
  [MkSolo r1, MkSolo r2] <- select conn "SELECT t FROM testparams WHERE (id = ? OR id = ?)" (1 :: Int, 2 :: Int) :: IO [Solo String]
  assertEqual "select params" "test string" r1
  assertEqual "select params" "test2" r2
  [MkSolo i] <- select conn "SELECT ?+?" [42 :: Int, 1 :: Int] :: IO [Solo Int]
  assertEqual "select int param" 43 i
  [MkSolo d] <- select conn "SELECT ?" [2.0 :: Double] :: IO [Solo Double]
  assertEqual "select double param" 2.0 d
  [MkSolo f] <- select conn "SELECT ?" [4.0 :: Float] :: IO [Solo Float]
  assertEqual "select double param" 4.0 f

testSimpleInsertId :: TestEnv -> Test
testSimpleInsertId TestEnv {..} = TestCase $ do
  execute_ conn "CREATE TABLE test_row_id (id INTEGER PRIMARY KEY, t TEXT)"
  execute conn "INSERT INTO test_row_id (t) VALUES (?)" (MkSolo ("test string" :: String))
  id1 <- lastInsertRowId conn
  execute_ conn "INSERT INTO test_row_id (t) VALUES ('test2')"
  id2 <- lastInsertRowId conn
  1 @=? id1
  2 @=? id2
  rows <- select conn "SELECT t FROM test_row_id WHERE id = ?" (MkSolo (1 :: Int)) :: IO [Solo String]
  1 @=? (length rows)
  (MkSolo "test string") @=? (head rows)
  [MkSolo row] <- select conn "SELECT t FROM test_row_id WHERE id = ?" (MkSolo (2 :: Int)) :: IO [Solo String]
  "test2" @=? row

testSimpleMultiInsert :: TestEnv -> Test
testSimpleMultiInsert TestEnv {..} = TestCase $ do
  execute_ conn "CREATE TABLE test_multi_insert (id INTEGER PRIMARY KEY, t1 TEXT, t2 TEXT)"
  executeMany conn "INSERT INTO test_multi_insert (t1, t2) VALUES (?, ?)" ([("foo", "bar"), ("baz", "bat")] :: [(String, String)])
  id2 <- lastInsertRowId conn
  2 @=? id2

  rows <- select_ conn "SELECT id,t1,t2 FROM test_multi_insert" :: IO [(Int, String, String)]
  [(1, "foo", "bar"), (2, "baz", "bat")] @=? rows

testSimpleQueryCov :: TestEnv -> Test
testSimpleQueryCov _ = TestCase $ do
  let str = "SELECT 1+1" :: T.Text
      q = "SELECT 1+1" :: Sql
  sqlText q @=? str
  show str @=? show q
  q @=? ((read . show $ q) :: Sql)
  q @=? q
  q @=? (Sql "SELECT 1" <> Sql "+1")
  q @=? foldr mappend mempty ["SELECT ", "1", "+", "1"]
  True @=? q <= q

testSimpleStrings :: TestEnv -> Test
testSimpleStrings TestEnv {..} = TestCase $ do
  [MkSolo s] <- select_ conn "SELECT 'str1'" :: IO [Solo T.Text]
  s @=? "str1"
  [MkSolo s] <- select_ conn "SELECT 'strLazy'" :: IO [Solo LT.Text]
  s @=? "strLazy"
  [MkSolo s] <- select conn "SELECT ?" (MkSolo ("strP" :: T.Text)) :: IO [Solo T.Text]
  s @=? "strP"
  [MkSolo s] <- select conn "SELECT ?" (MkSolo ("strPLazy" :: LT.Text)) :: IO [Solo T.Text]
  s @=? "strPLazy"
  -- ByteStrings are blobs in sqlite storage, so use ByteString for
  -- both input and output
  [MkSolo s] <- select conn "SELECT ?" (MkSolo ("strBsP" :: BS.ByteString)) :: IO [Solo BS.ByteString]
  s @=? "strBsP"
  [MkSolo s] <- select conn "SELECT ?" (MkSolo ("strBsPLazy" :: LBS.ByteString)) :: IO [Solo BS.ByteString]
  s @=? "strBsPLazy"
  [MkSolo s] <- select conn "SELECT ?" (MkSolo ("strBsPLazy2" :: BS.ByteString)) :: IO [Solo LBS.ByteString]
  s @=? "strBsPLazy2"

testSimpleChanges :: TestEnv -> Test
testSimpleChanges TestEnv {..} = TestCase $ do
  execute_ conn "CREATE TABLE testchanges (id INTEGER PRIMARY KEY, t TEXT)"
  execute conn "INSERT INTO testchanges(t) VALUES (?)" (MkSolo ("test string" :: String))
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute conn "INSERT INTO testchanges(t) VALUES (?)" (MkSolo ("test string 2" :: String))
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo' WHERE id = 1"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 1 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo' WHERE id = 100"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 0 numChanges
  execute_ conn "UPDATE testchanges SET t = 'foo'"
  numChanges <- changes conn
  assertEqual "changed/inserted rows" 2 numChanges
