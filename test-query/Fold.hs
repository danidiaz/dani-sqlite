{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Fold
  ( testFolds,
  )
where

import Common
import qualified Test.Tasty.HUnit as Tasty


testFolds :: IO TestEnv -> Tasty.Assertion
testFolds ioenv = do
  TestEnv {..} <- ioenv
  execute_ conn "CREATE TABLE testf (id INTEGER PRIMARY KEY, t INT)"
  execute_ conn "INSERT INTO testf (t) VALUES (4)"
  execute_ conn "INSERT INTO testf (t) VALUES (5)"
  execute_ conn "INSERT INTO testf (t) VALUES (6)"
  val <- fold_ conn "SELECT id,t FROM testf" sumValues do pure ([], [])
  Tasty.assertEqual "fold_" val ([3, 2, 1], [6, 5, 4])
  val <- fold conn "SELECT id,t FROM testf WHERE id > ?" (MkSolo (1 :: Int)) sumValues do pure ([], [])
  Tasty.assertEqual "fold" val ([3, 2], [6, 5])
  val <- foldNamed conn "SELECT id,t FROM testf WHERE id > :id" [":id" := (1 :: Int)] sumValues do pure ([], [])
  Tasty.assertEqual "fold" val ([3, 2], [6, 5])
  where
    sumValues (accId, accT) (id_ :: Int, t :: Int) = return (id_ : accId, t : accT)
