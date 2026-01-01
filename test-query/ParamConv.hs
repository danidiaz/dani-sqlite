{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module ParamConv
  ( testParamConvNull,
    testParamConvInt,
    testParamConvIntWidths,
    testParamConvIntWidthsFromField,
    testParamConvFloat,
    testParamConvBools,
    testParamConvFromRow,
    testParamConvToRow,
    testParamConvComposite,
    testParamNamed,
  )
where

import Common
import Data.Int
import Data.Text qualified as T
import Data.Word
import Sqlite.Query.Types (Null (..))
import Test.Tasty.HUnit
import Test.Tasty.HUnit qualified as Tasty

one, two, three :: Int
one = 1
two = 2
three = 3

testParamConvNull :: IO TestEnv -> Tasty.Assertion
testParamConvNull ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE nulltype (id INTEGER PRIMARY KEY, t1 TEXT)"
  [MkSolo r] <- (select_ conn "SELECT NULL") :: IO [Solo Null]
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (one, r)
  [MkSolo mr1] <- select_ conn "SELECT t1 FROM nulltype WHERE id = 1" :: IO [Solo (Maybe String)]
  Tasty.assertEqual "nulls" Nothing mr1
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (two, "foo" :: String)
  [MkSolo mr2] <- select_ conn "SELECT t1 FROM nulltype WHERE id = 2" :: IO [Solo (Maybe String)]
  Tasty.assertEqual "nulls" (Just "foo") mr2

testParamConvInt :: IO TestEnv -> Tasty.Assertion
testParamConvInt ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo one)) :: IO [Solo Int]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo one)) :: IO [Solo Integer]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?+?" (one, two)) :: IO [Solo Int]
  Tasty.assertEqual "value" 3 r
  [MkSolo r] <- (select conn "SELECT ?+?" (one, 15 :: Int64)) :: IO [Solo Int]
  Tasty.assertEqual "value" 16 r
  [MkSolo r] <- (select conn "SELECT ?+?" (two, 14 :: Int32)) :: IO [Solo Int]
  Tasty.assertEqual "value" 16 r
  [MkSolo r] <- (select conn "SELECT ?+?" (two, 14 :: Integer)) :: IO [Solo Int]
  Tasty.assertEqual "value" 16 r
  -- This overflows 32-bit ints, verify that we get more than 32-bits out
  [MkSolo r] <- (select conn "SELECT 255*?" (MkSolo (0x7FFFFFFF :: Int32))) :: IO [Solo Int64]
  Tasty.assertEqual
    "> 32-bit result"
    (255 * 0x7FFFFFFF :: Int64)
    (fromIntegral r)
  [MkSolo r] <- (select conn "SELECT 2*?" (MkSolo (0x7FFFFFFFFF :: Int64))) :: IO [Solo Int64]
  Tasty.assertEqual
    "> 32-bit result & param"
    (2 * 0x7FFFFFFFFF :: Int64)
    (fromIntegral r)
  [MkSolo r] <- (select_ conn "SELECT NULL") :: IO [Solo (Maybe Int)]
  Tasty.assertEqual "should see nothing" Nothing r
  [MkSolo r] <- (select_ conn "SELECT 3") :: IO [Solo (Maybe Int)]
  Tasty.assertEqual "should see Just 3" (Just 3) r
  [MkSolo r] <- (select conn "SELECT ?") (MkSolo (Nothing :: Maybe Int)) :: IO [Solo (Maybe Int)]
  Tasty.assertEqual "should see nothing" Nothing r
  [MkSolo r] <- (select conn "SELECT ?") (MkSolo (Just three :: Maybe Int)) :: IO [Solo (Maybe Int)]
  Tasty.assertEqual "should see 4" (Just 3) r

testParamConvIntWidths :: IO TestEnv -> Tasty.Assertion
testParamConvIntWidths ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Int8))) :: IO [Solo Int]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int8))) :: IO [Solo Int] -- wrap around
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int16))) :: IO [Solo Int]
  Tasty.assertEqual "value" 257 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (258 :: Int32))) :: IO [Solo Int]
  Tasty.assertEqual "value" 258 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Word8))) :: IO [Solo Int]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Word8))) :: IO [Solo Int] -- wrap around
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Word16))) :: IO [Solo Int]
  Tasty.assertEqual "value" 257 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Word32))) :: IO [Solo Int]
  Tasty.assertEqual "value" 257 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (0x100000000 :: Word64))) :: IO [Solo Int]
  Tasty.assertEqual "value" 0x100000000 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Integer))) :: IO [Solo Int]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Word))) :: IO [Solo Int]
  Tasty.assertEqual "value" 1 r

testParamConvIntWidthsFromField :: IO TestEnv -> Tasty.Assertion
testParamConvIntWidthsFromField ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Int))) :: IO [Solo Int8]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int))) :: IO [Solo Int8] -- wrap around
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (65536 :: Int))) :: IO [Solo Int16] -- wrap around
  Tasty.assertEqual "value" 0 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (65536 :: Int))) :: IO [Solo Int32] -- wrap around
  Tasty.assertEqual "value" 65536 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (258 :: Int))) :: IO [Solo Int32]
  Tasty.assertEqual "value" 258 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Int))) :: IO [Solo Word8]
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int))) :: IO [Solo Word8] -- wrap around
  Tasty.assertEqual "value" 1 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int))) :: IO [Solo Word16]
  Tasty.assertEqual "value" 257 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (257 :: Int))) :: IO [Solo Word32]
  Tasty.assertEqual "value" 257 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (0x100000000 :: Int64))) :: IO [Solo Word64]
  Tasty.assertEqual "value" 0x100000000 r
  [MkSolo r] <- (select conn "SELECT ?" (MkSolo (1 :: Int))) :: IO [Solo Word]
  Tasty.assertEqual "value" 1 r

testParamConvFloat :: IO TestEnv -> Tasty.Assertion
testParamConvFloat ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo r] <- select conn "SELECT ?" (MkSolo (1.0 :: Double)) :: IO [Solo Double]
  Tasty.assertEqual "value" 1.0 r
  [MkSolo r] <- select conn "SELECT ?*0.25" (MkSolo (8.0 :: Double)) :: IO [Solo Double]
  Tasty.assertEqual "value" 2.0 r

testParamConvBools :: IO TestEnv -> Tasty.Assertion
testParamConvBools ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE bt (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- Booleans are ints with values 0 or 1 on Sqlite
  execute_ conn "INSERT INTO bt (b) VALUES (0)"
  execute_ conn "INSERT INTO bt (b) VALUES (1)"
  [MkSolo r1, MkSolo r2] <- select_ conn "SELECT b from bt" :: IO [Solo Bool]
  Tasty.assertEqual "bool" False r1
  Tasty.assertEqual "bool" True r2
  execute conn "INSERT INTO bt (b) VALUES (?)" (MkSolo True)
  execute conn "INSERT INTO bt (b) VALUES (?)" (MkSolo False)
  execute conn "INSERT INTO bt (b) VALUES (?)" (MkSolo False)
  [MkSolo r3, MkSolo r4, MkSolo r5] <-
    select_ conn "SELECT b from bt where id in (3, 4, 5) ORDER BY id" :: IO [Solo Bool]
  Tasty.assertEqual "bool" True r3
  Tasty.assertEqual "bool" False r4
  Tasty.assertEqual "bool" False r5

testParamConvFromRow :: IO TestEnv -> Tasty.Assertion
testParamConvFromRow ioenv = do
  TestEnv {..} <- ioenv
  [(1, 2)] <- select_ conn "SELECT 1,2" :: IO [(Int, Int)]
  [(1, 2, 3)] <- select_ conn "SELECT 1,2,3" :: IO [(Int, Int, Int)]
  [(1, 2, 3, 4)] <- select_ conn "SELECT 1,2,3,4" :: IO [(Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5)] <- select_ conn "SELECT 1,2,3,4,5" :: IO [(Int, Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5, 6)] <- select_ conn "SELECT 1,2,3,4,5,6" :: IO [(Int, Int, Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5, 6, 7)] <- select_ conn "SELECT 1,2,3,4,5,6,7" :: IO [(Int, Int, Int, Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5, 6, 7, 8)] <- select_ conn "SELECT 1,2,3,4,5,6,7,8" :: IO [(Int, Int, Int, Int, Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5, 6, 7, 8, 9)] <- select_ conn "SELECT 1,2,3,4,5,6,7,8,9" :: IO [(Int, Int, Int, Int, Int, Int, Int, Int, Int)]
  [(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)] <- select_ conn "SELECT 1,2,3,4,5,6,7,8,9,10" :: IO [(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]
  [[1, 2, 3]] <- select_ conn "SELECT 1,2,3" :: IO [[Int]]
  return ()

testParamConvToRow :: IO TestEnv -> Tasty.Assertion
testParamConvToRow ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo (s :: Int)] <- select conn "SELECT 13" ()
  13 @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?" (MkSolo one)
  1 @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?+?" (one, two)
  (1 + 2) @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?+?+?" (one, two, three)
  (1 + 2 + 3) @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?+?+?+?" (one, two, three, 4 :: Int)
  (1 + 2 + 3 + 4) @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int)
  (1 + 2 + 3 + 4 + 5) @=? s
  [MkSolo (s :: Int)] <- select conn "SELECT ?+?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int)
  (1 + 2 + 3 + 4 + 5 + 6) @=? s
  [MkSolo (s :: Int)] <-
    select
      conn
      "SELECT ?+?+?+?+?+?+?"
      (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int)
  (1 + 2 + 3 + 4 + 5 + 6 + 7) @=? s
  [MkSolo (s :: Int)] <-
    select
      conn
      "SELECT ?+?+?+?+?+?+?+?"
      (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int)
  (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8) @=? s
  [MkSolo (s :: Int)] <-
    select
      conn
      "SELECT ?+?+?+?+?+?+?+?+?"
      (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int)
  (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9) @=? s
  [MkSolo (s :: Int)] <-
    select
      conn
      "SELECT ?+?+?+?+?+?+?+?+?+?"
      (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int, 10 :: Int)
  (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10) @=? s

data TestTuple = TestTuple Int64 Int64 deriving (Eq, Show)

data TestTuple2 = TestTuple2 T.Text T.Text deriving (Eq, Show)

instance FromRow TestTuple where
  fromRow = TestTuple <$> field <*> field

instance FromRow TestTuple2 where
  fromRow = TestTuple2 <$> field <*> field

instance ToRow TestTuple where
  toRow (TestTuple a b) = [SqlInteger a, SqlInteger b]

instance ToRow TestTuple2 where
  toRow (TestTuple2 a b) = [SqlText a, SqlText b]

testParamConvComposite :: IO TestEnv -> Tasty.Assertion
testParamConvComposite ioenv = do
  TestEnv {..} <- ioenv
  [t1] <- select_ conn "SELECT 1,2"
  TestTuple 1 2 @=? t1
  [t2] <- select_ conn "SELECT 'foo','bar'"
  TestTuple2 "foo" "bar" @=? t2
  [a :. b] <- select_ conn "SELECT 4,5,'baz','xyzz'"
  TestTuple 4 5 :. TestTuple2 "baz" "xyzz" @=? a :. b
  [TestTuple x y :. TestTuple2 z w] <- select conn "SELECT ?,?,?,?" (a :. b)
  x @=? 4
  y @=? 5
  z @=? "baz"
  w @=? "xyzz"

testParamNamed :: IO TestEnv -> Tasty.Assertion
testParamNamed ioenv = do
  TestEnv {..} <- ioenv
  [MkSolo t1] <- selectNamed conn "SELECT :foo / :bar" [":foo" := two, ":bar" := one]
  t1 @=? (2 :: Int)
  [(t1, t2)] <- selectNamed conn "SELECT :foo,:bar" [":foo" := ("foo" :: T.Text), ":bar" := one]
  t1 @=? ("foo" :: T.Text)
  t2 @=? one
  execute_ conn "CREATE TABLE np (id INTEGER PRIMARY KEY, b BOOLEAN)"
  executeNamed conn "INSERT INTO np (b) VALUES (:b)" [":b" := True]
  [MkSolo t1] <- select_ conn "SELECT b FROM np"
  True @=? t1
