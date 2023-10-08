{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-overflowed-literals #-}
#endif

module ParamConv (
    testParamConvNull
  , testParamConvInt
  , testParamConvIntWidths
  , testParamConvIntWidthsFromField
  , testParamConvFloat
  , testParamConvBools
  , testParamConvFromRow
  , testParamConvToRow
  , testParamConvComposite
  , testParamNamed) where

import           Data.Int
import           Data.Word
import qualified Data.Text as T
import Sqlite.Query.Types (Null(..))
import Data.Tuple (Solo(..))
import Common

one, two, three :: Int
one   = 1
two   = 2
three = 3

testParamConvNull :: TestEnv -> Test
testParamConvNull TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE nulltype (id INTEGER PRIMARY KEY, t1 TEXT)"
  [Solo r] <- (query_ conn "SELECT NULL") :: IO [Solo Null]
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (one, r)
  [Solo mr1] <- query_ conn "SELECT t1 FROM nulltype WHERE id = 1" :: IO [Solo (Maybe String)]
  assertEqual "nulls" Nothing mr1
  execute conn "INSERT INTO nulltype (id, t1) VALUES (?,?)" (two, "foo" :: String)
  [Solo mr2] <- query_ conn "SELECT t1 FROM nulltype WHERE id = 2" :: IO [Solo (Maybe String)]
  assertEqual "nulls" (Just "foo") mr2

testParamConvInt :: TestEnv -> Test
testParamConvInt TestEnv{..} = TestCase $ do
  [Solo r] <- (query conn "SELECT ?" (Solo one)) :: IO [Solo Int]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo one)) :: IO [Solo Integer]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?+?" (one, two)) :: IO [Solo Int]
  assertEqual "value" 3 r
  [Solo r] <- (query conn "SELECT ?+?" (one, 15 :: Int64)) :: IO [Solo Int]
  assertEqual "value" 16 r
  [Solo r] <- (query conn "SELECT ?+?" (two, 14 :: Int32)) :: IO [Solo Int]
  assertEqual "value" 16 r
  [Solo r] <- (query conn "SELECT ?+?" (two, 14 :: Integer)) :: IO [Solo Int]
  assertEqual "value" 16 r
  -- This overflows 32-bit ints, verify that we get more than 32-bits out
  [Solo r] <- (query conn "SELECT 255*?" (Solo (0x7FFFFFFF :: Int32))) :: IO [Solo Int64]
  assertEqual "> 32-bit result"
    (255*0x7FFFFFFF :: Int64) (fromIntegral r)
  [Solo r] <- (query conn "SELECT 2*?" (Solo (0x7FFFFFFFFF :: Int64))) :: IO [Solo Int64]
  assertEqual "> 32-bit result & param"
    (2*0x7FFFFFFFFF :: Int64) (fromIntegral r)
  [Solo r] <- (query_ conn "SELECT NULL") :: IO [Solo (Maybe Int)]
  assertEqual "should see nothing" Nothing r
  [Solo r] <- (query_ conn "SELECT 3") :: IO [Solo (Maybe Int)]
  assertEqual "should see Just 3" (Just 3) r
  [Solo r] <- (query conn "SELECT ?") (Solo (Nothing :: Maybe Int)) :: IO [Solo (Maybe Int)]
  assertEqual "should see nothing" Nothing r
  [Solo r] <- (query conn "SELECT ?") (Solo (Just three :: Maybe Int)) :: IO [Solo (Maybe Int)]
  assertEqual "should see 4" (Just 3) r

testParamConvIntWidths :: TestEnv -> Test
testParamConvIntWidths TestEnv{..} = TestCase $ do
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Int8))) :: IO [Solo Int]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int8))) :: IO [Solo Int] -- wrap around
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int16))) :: IO [Solo Int]
  assertEqual "value" 257 r
  [Solo r] <- (query conn "SELECT ?" (Solo (258 :: Int32))) :: IO [Solo Int]
  assertEqual "value" 258 r
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Word8))) :: IO [Solo Int]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Word8))) :: IO [Solo Int] -- wrap around
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Word16))) :: IO [Solo Int]
  assertEqual "value" 257 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Word32))) :: IO [Solo Int]
  assertEqual "value" 257 r
  [Solo r] <- (query conn "SELECT ?" (Solo (0x100000000 :: Word64))) :: IO [Solo Int]
  assertEqual "value" 0x100000000 r
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Integer))) :: IO [Solo Int]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Word))) :: IO [Solo Int]
  assertEqual "value" 1 r

testParamConvIntWidthsFromField :: TestEnv -> Test
testParamConvIntWidthsFromField TestEnv{..} = TestCase $ do
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Int))) :: IO [Solo Int8]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int))) :: IO [Solo Int8] -- wrap around
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (65536 :: Int))) :: IO [Solo Int16] -- wrap around
  assertEqual "value" 0 r
  [Solo r] <- (query conn "SELECT ?" (Solo (65536 :: Int))) :: IO [Solo Int32] -- wrap around
  assertEqual "value" 65536 r
  [Solo r] <- (query conn "SELECT ?" (Solo (258 :: Int))) :: IO [Solo Int32]
  assertEqual "value" 258 r
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Int))) :: IO [Solo Word8]
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int))) :: IO [Solo Word8] -- wrap around
  assertEqual "value" 1 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int))) :: IO [Solo Word16]
  assertEqual "value" 257 r
  [Solo r] <- (query conn "SELECT ?" (Solo (257 :: Int))) :: IO [Solo Word32]
  assertEqual "value" 257 r
  [Solo r] <- (query conn "SELECT ?" (Solo (0x100000000 :: Int64))) :: IO [Solo Word64]
  assertEqual "value" 0x100000000 r
  [Solo r] <- (query conn "SELECT ?" (Solo (1 :: Int))) :: IO [Solo Word]
  assertEqual "value" 1 r

testParamConvFloat :: TestEnv -> Test
testParamConvFloat TestEnv{..} = TestCase $ do
  [Solo r] <- query conn "SELECT ?" (Solo (1.0 :: Double)) :: IO [Solo Double]
  assertEqual "value" 1.0 r
  [Solo r] <- query conn "SELECT ?*0.25" (Solo (8.0 :: Double)) :: IO [Solo Double]
  assertEqual "value" 2.0 r

testParamConvBools :: TestEnv -> Test
testParamConvBools TestEnv{..} = TestCase $ do
  execute_ conn "CREATE TABLE bt (id INTEGER PRIMARY KEY, b BOOLEAN)"
  -- Booleans are ints with values 0 or 1 on Sqlite
  execute_ conn "INSERT INTO bt (b) VALUES (0)"
  execute_ conn "INSERT INTO bt (b) VALUES (1)"
  [Solo r1, Solo r2] <- query_ conn "SELECT b from bt" :: IO [Solo Bool]
  assertEqual "bool" False r1
  assertEqual "bool" True r2
  execute conn "INSERT INTO bt (b) VALUES (?)" (Solo True)
  execute conn "INSERT INTO bt (b) VALUES (?)" (Solo False)
  execute conn "INSERT INTO bt (b) VALUES (?)" (Solo False)
  [Solo r3, Solo r4, Solo r5] <-
    query_ conn "SELECT b from bt where id in (3, 4, 5) ORDER BY id" :: IO [Solo Bool]
  assertEqual "bool" True r3
  assertEqual "bool" False r4
  assertEqual "bool" False r5

testParamConvFromRow :: TestEnv -> Test
testParamConvFromRow TestEnv{..} = TestCase $ do
  [(1,2)] <- query_ conn "SELECT 1,2" :: IO [(Int,Int)]
  [(1,2,3)] <- query_ conn "SELECT 1,2,3" :: IO [(Int,Int,Int)]
  [(1,2,3,4)] <- query_ conn "SELECT 1,2,3,4" :: IO [(Int,Int,Int,Int)]
  [(1,2,3,4,5)] <- query_ conn "SELECT 1,2,3,4,5" :: IO [(Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6)] <- query_ conn "SELECT 1,2,3,4,5,6" :: IO [(Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7)] <- query_ conn "SELECT 1,2,3,4,5,6,7" :: IO [(Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8,9)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8,9" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int,Int)]
  [(1,2,3,4,5,6,7,8,9,10)] <- query_ conn "SELECT 1,2,3,4,5,6,7,8,9,10" :: IO [(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)]
  [[1,2,3]] <- query_ conn "SELECT 1,2,3" :: IO [[Int]]
  return ()

testParamConvToRow :: TestEnv -> Test
testParamConvToRow TestEnv{..} = TestCase $ do
  [Solo (s :: Int)] <- query conn "SELECT 13" ()
  13 @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?" (Solo one)
  1 @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?" (one, two)
  (1+2) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?" (one, two, three)
  (1+2+3) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?" (one, two, three, 4 :: Int)
  (1+2+3+4) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int)
  (1+2+3+4+5) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?" (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int)
  (1+2+3+4+5+6) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int)
  (1+2+3+4+5+6+7) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int)
  (1+2+3+4+5+6+7+8) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int)
  (1+2+3+4+5+6+7+8+9) @=? s
  [Solo (s :: Int)] <- query conn "SELECT ?+?+?+?+?+?+?+?+?+?"
                         (one, two, three, 4 :: Int, 5 :: Int, 6 :: Int, 7 :: Int, 8 :: Int, 9 :: Int, 10 :: Int)
  (1+2+3+4+5+6+7+8+9+10) @=? s

data TestTuple  = TestTuple  Int64 Int64 deriving (Eq, Show)
data TestTuple2 = TestTuple2 T.Text T.Text deriving (Eq, Show)

instance FromRow TestTuple where
  fromRow = TestTuple <$> field <*> field

instance FromRow TestTuple2 where
  fromRow = TestTuple2 <$> field <*> field

instance ToRow TestTuple where
  toRow (TestTuple a b) = [SQLInteger a, SQLInteger b]

instance ToRow TestTuple2 where
  toRow (TestTuple2 a b) = [SQLText a, SQLText b]

testParamConvComposite :: TestEnv -> Test
testParamConvComposite TestEnv{..} = TestCase $ do
  [t1] <- query_ conn "SELECT 1,2"
  TestTuple 1 2 @=? t1
  [t2] <- query_ conn "SELECT 'foo','bar'"
  TestTuple2 "foo" "bar" @=? t2
  [a :. b] <- query_ conn "SELECT 4,5,'baz','xyzz'"
  TestTuple 4 5 :. TestTuple2 "baz" "xyzz" @=? a :. b
  [TestTuple x y :. TestTuple2 z w] <- query conn "SELECT ?,?,?,?" (a :. b)
  x @=? 4
  y @=? 5
  z @=? "baz"
  w @=? "xyzz"

testParamNamed :: TestEnv -> Test
testParamNamed TestEnv{..} = TestCase $ do
  [Solo t1] <- queryNamed conn "SELECT :foo / :bar" [":foo" := two, ":bar" := one]
  t1 @=? (2 :: Int)
  [(t1,t2)] <- queryNamed conn "SELECT :foo,:bar" [":foo" := ("foo" :: T.Text), ":bar" := one]
  t1 @=? ("foo" :: T.Text)
  t2 @=? one
  execute_ conn "CREATE TABLE np (id INTEGER PRIMARY KEY, b BOOLEAN)"
  executeNamed conn "INSERT INTO np (b) VALUES (:b)" [":b" := True]
  [Solo t1] <- query_ conn "SELECT b FROM np"
  True @=? t1
