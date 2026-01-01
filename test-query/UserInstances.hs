{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UserInstances
  ( testUserFromField,
    testSqlDataFromField,
  )
where

import Common
import Data.Int (Int64)
import Data.Text qualified as T
import Sqlite.Query.FromField
import Sqlite.Query.Ok
import Sqlite.Query.ToField
import Test.Tasty.HUnit
import Test.Tasty.HUnit qualified as Tasty

newtype MyType = MyType String deriving (Eq, Show)

instance FromField MyType where
  fromField f = cvt f . fieldData $ f
    where
      -- Prefix with "fromField " to really ensure we got here
      cvt _ (SqlText s) = Ok $ MyType ("fromField " ++ (T.unpack s))
      cvt f _ = returnError ConversionFailed f "expecting SqlText type"

instance ToField MyType where
  -- Prefix with "toField " to really ensure we got here
  toField (MyType s) = SqlText . T.pack $ ("toField " ++ s)

testUserFromField :: IO TestEnv -> Tasty.Assertion
testUserFromField ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE fromfield (t TEXT)"
  execute conn "INSERT INTO fromfield (t) VALUES (?)" (MkSolo ("test string" :: String))
  [MkSolo r] <- select_ conn "SELECT t FROM fromfield" :: IO [(Solo MyType)]
  (MyType "fromField test string") @=? r
  execute_ conn "DELETE FROM fromfield"
  execute conn "INSERT INTO fromfield (t) VALUES (?)" (MkSolo (MyType "test2"))
  [MkSolo r] <- select_ conn "SELECT t FROM fromfield" :: IO [(Solo String)]
  "toField test2" @=? r

testSqlDataFromField :: IO TestEnv -> Tasty.Assertion
testSqlDataFromField ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE sqldatafromfield (t TEXT, i INT, b BOOLEAN, f FLOAT)"
  execute
    conn
    "INSERT INTO sqldatafromfield (t,i,b,f) VALUES (?,?,?,?)"
    ( ( "test string" :: T.Text,
        1 :: Int64,
        True :: Bool,
        1.11 :: Double
      )
    )
  execute
    conn
    "INSERT INTO sqldatafromfield (t,i,b) VALUES (?,?,?)"
    ( ( "test string2" :: T.Text,
        2 :: Int64,
        False :: Bool
      )
    )
  r <- select_ conn "SELECT * FROM sqldatafromfield" :: IO [[SqlData]]
  let testData =
        [ [ SqlText "test string",
            SqlInteger 1,
            SqlInteger 1,
            SqlFloat 1.11
          ],
          [ SqlText "test string2",
            SqlInteger 2,
            SqlInteger 0,
            SqlNull
          ]
        ]
  testData @=? r
