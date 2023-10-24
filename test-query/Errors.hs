{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Errors (
    testErrorsColumns
  , testErrorsInvalidParams
  , testErrorsInvalidNamedParams
  , testErrorsWithStatement
  , testErrorsColumnName
  , testErrorsTransaction
  , testErrorsImmediateTransaction
  , testErrorsExclusiveTransaction
  ) where

import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Word

import           Common
import Sqlite.Query.Types (Null)

-- The "length (show e) `seq` .." trickery below is to force evaluate
-- the contents of error messages.  Another option would be to log
-- them (would be useful), but I don't know if HUnit has any logging
-- mechanisms.  Just printing them as is will look like the tests are
-- hitting errors and would be confusing.
assertResultErrorCaught :: IO a -> Assertion
assertResultErrorCaught action = do
  catch (action >> return False) (\(e :: ResultError) -> length (show e) `seq` return True) >>=
    assertBool "assertResultError exc"

assertFormatErrorCaught :: IO a -> Assertion
assertFormatErrorCaught action = do
  catch (action >> return False) (\(e :: FormatError) -> length (show e) `seq` return True) >>=
    assertBool "assertFormatError exc"

assertSQLErrorCaught :: IO a -> Assertion
assertSQLErrorCaught action = do
  catch (action >> return False) (\(e :: SqliteException) -> length (show e) `seq` return True) >>=
    assertBool "assertSQLError exc"

assertOOBCaught :: IO a -> Assertion
assertOOBCaught action = do
  catch (action >> return False) (\(e :: ArrayException) -> length (show e) `seq` return True) >>=
    assertBool "assertOOBCaught exc"

testErrorsColumns :: TestEnv -> Test
testErrorsColumns TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE cols (id INTEGER PRIMARY KEY, t TEXT)"
  execute_ conn "INSERT INTO cols (t) VALUES ('test string')"
  rows <- select_ conn "SELECT t FROM cols" :: IO [Solo String]
  assertEqual "row count" 1 (length rows)
  assertEqual "string" (MkSolo "test string") (head rows)
  -- Mismatched number of output columns (selects two, dest type has 1 field)
  assertResultErrorCaught (select_ conn "SELECT id,t FROM cols" :: IO [Solo Int])
  -- Same as above but the other way round (select 1, dst has two)
  assertResultErrorCaught (select_ conn "SELECT id FROM cols" :: IO [(Int, String)])
  -- Mismatching types (source int,text doesn't match dst string,int
  assertResultErrorCaught (select_ conn "SELECT id, t FROM cols" :: IO [(String, Int)])
  -- Mismatching types (source string doesn't match dst integer
  assertResultErrorCaught (select_ conn "SELECT 'foo'" :: IO [Solo Integer])
  -- Mismatching types (sources don't match destination float/double type)
  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo Double])
  assertResultErrorCaught (select_ conn "SELECT 'foo'" :: IO [Solo Double])
  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo Float])
  assertResultErrorCaught (select_ conn "SELECT 'foo'" :: IO [Solo Float])
  -- Mismatching types (sources don't match destination bool type, or is out of bounds)
  assertResultErrorCaught (select_ conn "SELECT 'true'" :: IO [Solo Bool])
  assertResultErrorCaught (select_ conn "SELECT 2" :: IO [Solo Bool])
  -- Mismatching types (sources don't match destination string types (text, string)
  -- It seems that these actually convert ok to text...
--  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo T.Text])
--  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo LT.Text])
--  assertResultErrorCaught (select_ conn "SELECT 1.0" :: IO [Solo T.Text])
--  assertResultErrorCaught (select_ conn "SELECT 1.0" :: IO [Solo LT.Text])
  -- Mismatching types (sources don't match destination bytestring)
  [MkSolo (_ :: B.ByteString)] <-  select_ conn "SELECT X'3177'"
  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo B.ByteString])
  assertResultErrorCaught (select_ conn "SELECT 1" :: IO [Solo LB.ByteString])
  assertResultErrorCaught (select_ conn "SELECT 'foo'" :: IO [Solo B.ByteString])
  assertResultErrorCaught (select_ conn "SELECT 'foo'" :: IO [Solo LB.ByteString])
  -- Trying to get a blob into a string
  let d = B.pack ([0..127] :: [Word8])
  execute_ conn "CREATE TABLE cols_blobs (id INTEGER, b BLOB)"
  execute conn "INSERT INTO cols_blobs (id, b) VALUES (?,?)" (1 :: Int, d)
  assertResultErrorCaught
    (do [MkSolo _t1] <- select conn "SELECT b FROM cols_blobs WHERE id = ?" (MkSolo (1 :: Int)) :: IO [Solo String]
        return ())
  execute_ conn "CREATE TABLE cols_bools (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- 3 = invalid value for bool, must be 0 or 1
  execute_ conn "INSERT INTO cols_bools (b) VALUES (3)"
  assertResultErrorCaught
    (do [MkSolo _t1] <- select_ conn "SELECT b FROM cols_bools" :: IO [Solo Bool]
        return ())
  [MkSolo (nullVal :: Null)] <- select_ conn "SELECT NULL"
  False @=? nullVal == nullVal
  False @=? nullVal /= nullVal
  assertResultErrorCaught
    (do [MkSolo (_t1 :: Null)] <- select_ conn "SELECT 1" :: IO [Solo Null]
        return ())

testErrorsInvalidParams :: TestEnv -> Test
testErrorsInvalidParams TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invparams (id INTEGER PRIMARY KEY, t TEXT)"
  -- Test that only unnamed params are accepted
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (t) VALUES (:v)" (MkSolo ("foo" :: String)))
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (id, t) VALUES (:v,$1)" (3::Int, "foo" :: String))
  -- In this case, we have two bound params but only one given to
  -- execute.  This should cause an error.
  assertFormatErrorCaught
    (execute conn "INSERT INTO invparams (id, t) VALUES (?, ?)" (MkSolo (3::Int)))

testErrorsInvalidNamedParams :: TestEnv -> Test
testErrorsInvalidNamedParams TestEnv{..} = TestCase $ do
  -- Test that only unnamed params are accepted
  assertFormatErrorCaught
    (selectNamed conn "SELECT :foo" [":foox" := (1 :: Int)] :: IO [Solo Int])
  -- In this case, we have two bound params but only one given to
  -- execute.  This should cause an error.
  assertFormatErrorCaught
    (selectNamed conn "SELECT :foo + :bar" [":foo" := (1 :: Int)] :: IO [Solo Int])
  -- Can't use named params in SQL string with the unnamed query/exec variants
  assertFormatErrorCaught
    (select conn "SELECT :foo" (MkSolo (1 :: Int)) :: IO [Solo Int])

testErrorsWithStatement :: TestEnv -> Test
testErrorsWithStatement TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invstat (id INTEGER PRIMARY KEY, t TEXT)"
  assertSQLErrorCaught $
    withStatement conn "SELECT id, t, t1 FROM invstat" $ \_stmt ->
      assertFailure "Error not detected"

testErrorsColumnName :: TestEnv -> Test
testErrorsColumnName TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE invcolumn (id INTEGER PRIMARY KEY, t TEXT)"
  assertOOBCaught $
    withStatement conn "SELECT id FROM invcolumn" $ \stmt ->
      columnName stmt (ColumnIndex (-1)) >> assertFailure "Error not detected"

testErrorsTransaction :: TestEnv -> Test
testErrorsTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE trans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withTransaction conn $ do
    executeNamed conn "INSERT INTO trans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [MkSolo r] <- select_ conn "SELECT t FROM trans" :: IO [Solo String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM trans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO trans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO trans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- select_ conn "SELECT t FROM trans" :: IO [Solo String]
      case rows of
        [MkSolo txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"

testErrorsImmediateTransaction :: TestEnv -> Test
testErrorsImmediateTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE itrans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withImmediateTransaction conn $ do
    executeNamed conn "INSERT INTO itrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [MkSolo r] <- select_ conn "SELECT t FROM itrans" :: IO [Solo String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM itrans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withImmediateTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO itrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO itrans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- select_ conn "SELECT t FROM itrans" :: IO [Solo String]
      case rows of
        [MkSolo txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"

testErrorsExclusiveTransaction :: TestEnv -> Test
testErrorsExclusiveTransaction TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE etrans (id INTEGER PRIMARY KEY, t TEXT)"
  v <- withExclusiveTransaction conn $ do
    executeNamed conn "INSERT INTO etrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
    [MkSolo r] <- select_ conn "SELECT t FROM etrans" :: IO [Solo String]
    return r
  v @=? "foo"
  e <- rowExists
  True @=? e
  execute_ conn "DELETE FROM etrans"
  e <- rowExists
  False @=? e
  assertFormatErrorCaught
    (withExclusiveTransaction conn $ do
        -- this execute should be automatically rolled back on error
        executeNamed conn
          "INSERT INTO etrans (t) VALUES (:txt)" [":txt" := ("foo" :: String)]
        -- intentional mistake here to hit an error & cause rollback of txn
        executeNamed conn
          "INSERT INTO etrans (t) VALUES (:txt)" [":missing" := ("foo" :: String)])
  e <- rowExists
  False @=? e
  where
    rowExists = do
      rows <- select_ conn "SELECT t FROM etrans" :: IO [Solo String]
      case rows of
        [MkSolo txt] -> do
          "foo" @=? txt
          return True
        [] ->
          return False
        _ -> error "should have only one row"
