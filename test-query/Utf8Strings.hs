{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Utf8Strings
  ( testUtf8Simplest,
    testBlobs,
  )
where

import Common
import Data.ByteString qualified as B
import Data.Word
import Test.Tasty.HUnit qualified as Tasty

testUtf8Simplest :: IO TestEnv -> Tasty.Assertion
testUtf8Simplest ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE utf (id INTEGER, t TEXT)"
  execute_ conn "INSERT INTO utf (id, t) VALUES (1, 'ääöö')"
  execute conn "INSERT INTO utf (id, t) VALUES (?,?)" (2 :: Int, "ääööåå" :: String)
  [MkSolo t1] <- select conn "SELECT t FROM utf WHERE id = ?" (MkSolo (1 :: Int))
  Tasty.assertEqual "utf8" ("ääöö" :: String) t1
  [MkSolo t2] <- select conn "SELECT t FROM utf WHERE id = ?" (MkSolo (2 :: Int))
  Tasty.assertEqual "utf8" ("ääööåå" :: String) t2

testBlobs :: IO TestEnv -> Tasty.Assertion
testBlobs ioenv = do
  TestEnv {..} <- ioenv
  let d = B.pack ([0 .. 127] :: [Word8])
  execute_ conn "CREATE TABLE blobs (id INTEGER, b BLOB)"
  execute conn "INSERT INTO blobs (id, b) VALUES (?,?)" (1 :: Int, d)
  [MkSolo t1] <- select conn "SELECT b FROM blobs WHERE id = ?" (MkSolo (1 :: Int))
  Tasty.assertEqual "blob" d t1
  Tasty.assertEqual "blob nul char" 0 (B.index d 0)
  Tasty.assertEqual "blob first char" 1 (B.index d 1)
