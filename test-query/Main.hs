{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common
import DirectSqlite
import Errors
import Fold
import ParamConv
import PreparedStatement
import Query
import Sqlite
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty
import TestImports
import UserInstances
import Utf8Strings

main :: IO ()
main = do
  tastyMain

tastyMain :: IO ()
tastyMain = do
  Tasty.defaultMain $
    withTestEnv $ \ioenv ->
      Tasty.testGroup
        "All"
        [ Tasty.testGroup
            "Query"
            [ Tasty.testCase "SimpleSelect" $ testSimpleSelect ioenv,
              Tasty.testCase "OutOfRangeParserSelect" $ testOutOfRangeParserSelect ioenv,
              Tasty.testCase "SimpleOnePlusOne" $ testSimpleOnePlusOne ioenv,
              Tasty.testCase "SimpleParams" $ testSimpleParams ioenv,
              Tasty.testCase "SimpleInsertId" $ testSimpleInsertId ioenv,
              Tasty.testCase "SimpleMultiInsert" $ testSimpleMultiInsert ioenv,
              Tasty.testCase "SimpleQueryCov" $ testSimpleQueryCov ioenv,
              Tasty.testCase "SimpleStrings" $ testSimpleStrings ioenv,
              Tasty.testCase "SimpleChanges" $ testSimpleChanges ioenv
            ],
          Tasty.testGroup
            "ParamConv"
            [ Tasty.testCase "testParamConvNull" $ testParamConvNull ioenv,
              Tasty.testCase "testParamConvInt" $ testParamConvInt ioenv,
              Tasty.testCase "testParamConvIntWidths" $ testParamConvIntWidths ioenv,
              Tasty.testCase "testParamConvIntWidthsFromField" $ testParamConvIntWidthsFromField ioenv,
              Tasty.testCase "testParamConvFloat" $ testParamConvFloat ioenv,
              Tasty.testCase "testParamConvBools" $ testParamConvBools ioenv,
              Tasty.testCase "testParamConvToRow" $ testParamConvToRow ioenv,
              Tasty.testCase "testParamConvFromRow" $ testParamConvFromRow ioenv,
              Tasty.testCase "testParamConvComposite" $ testParamConvComposite ioenv,
              Tasty.testCase "testParamName" $ testParamNamed ioenv
            ],
          Tasty.testGroup
            "Errors"
            [ Tasty.testCase "ErrorsColumns" $ testErrorsColumns ioenv,
              Tasty.testCase "ErrorsInvalidParams" $ testErrorsInvalidParams ioenv,
              Tasty.testCase "ErrorsInvalidNamedParams" $ testErrorsInvalidNamedParams ioenv,
              Tasty.testCase "ErrorsWithStatement" $ testErrorsWithStatement ioenv,
              Tasty.testCase "ErrorsColumnName" $ testErrorsColumnName ioenv,
              Tasty.testCase "ErrorsTransaction" $ testErrorsTransaction ioenv,
              Tasty.testCase "ErrorsImmediateTransaction" $ testErrorsImmediateTransaction ioenv,
              Tasty.testCase "ErrorsExclusiveTransaction" $ testErrorsExclusiveTransaction ioenv
            ],
          Tasty.testGroup
            "Utf8"
            [ Tasty.testCase "Utf8Simplest" $ testUtf8Simplest ioenv,
              Tasty.testCase "Blobs" $ testBlobs ioenv
            ],
          Tasty.testGroup
            "Instances"
            [ Tasty.testCase "UserFromField" $ testUserFromField ioenv,
              Tasty.testCase "SqlDataFromField" $ testSqlDataFromField ioenv
            ],
          Tasty.testGroup
            "Fold"
            [ Tasty.testCase "Folds" $ testFolds ioenv
            ],
          Tasty.testGroup
            "Statement"
            [ Tasty.testCase "Bind" $ testBind ioenv,
              Tasty.testCase "DoubleBind" $ testDoubleBind ioenv,
              Tasty.testCase "PreparedStatements" $ testPreparedStatements ioenv,
              Tasty.testCase "PreparedStatementsColumnCount" $ testPreparedStatementsColumnCount ioenv
            ],
          Tasty.testGroup
            "Direct"
            [ Tasty.testCase "DirectSqlite" $ testDirectSqlite ioenv
            ],
          Tasty.testGroup
            "Imports"
            [ Tasty.testCase "Imports" $ testImports ioenv
            ]
        ]

withTestEnv ::
  (IO TestEnv -> Tasty.TestTree) ->
  Tasty.TestTree
withTestEnv =
  Tasty.withResource allocFile deallocFile
  where
    allocFile = do
      conn <- open ":memory:"
      pure TestEnv {conn}
    deallocFile TestEnv {conn} = close conn
