{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.Sqlite.Simple
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
module Sqlite.Query
  ( -- ** Examples of use
    -- $use

    -- ** The Sql type
    -- $querytype

    -- ** Parameter substitution
    -- $subst

    -- *** Positional parameters
    -- $substpos

    -- *** Named parameters
    -- $substnamed

    -- *** Type inference
    -- $inference

    -- ** Substituting a single parameter
    -- $only_param

    -- * Extracting results
    -- $result

    -- ** Handling null values
    -- $null

    -- ** Type conversions
    -- $types
    Sql (..),
    Connection (..),
    ToRow (..),
    FromRow (..),
    Solo (..),
    (:.) (..),
    SqlData (..),
    PreparedStatement,
    ColumnIndex (..),
    NamedParam (..),

    -- * Queries that return results
    select,
    select_,
    selectWith,
    selectWith_,
    selectNamed,
    lastInsertRowId,
    changes,
    totalChanges,

    -- * Queries that stream results
    fold,
    fold_,
    foldNamed,

    -- * Statements that do not return results
    execute,
    execute_,
    executeMany,
    executeNamed,
    field,

    -- * Low-level statement API for stream access and prepared statements
    openStatement,
    closeStatement,
    withStatement,
    bind,
    bindNamed,
    reset,
    columnName,
    columnCount,
    withBind,
    nextRow,
    foldPrepared,

    -- * Transactions
    withTransaction,
    withImmediateTransaction,
    withExclusiveTransaction,

    -- ** Exceptions
    FormatError (..),
    ResultError (..),
    Sqlite.SqliteException (..),
    Sqlite.Error (..),
  )
where

import Control.Exception
import Control.Monad (forM_, void, when)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Sqlite (ColumnIndex, PreparedStatement, SqlData)
import Sqlite qualified
import Sqlite.Direct (Connection)
import Sqlite.Direct qualified
import Sqlite.Query.FromField (ResultError (..))
import Sqlite.Query.FromRow
import Sqlite.Query.Internal
import Sqlite.Query.Ok
import Sqlite.Query.ToField (ToField (..))
import Sqlite.Query.ToRow (ToRow (..))
import Sqlite.Query.Types

data NamedParam where
  (:=) :: (ToField v) => T.Text -> v -> NamedParam

data TransactionType
  = Deferred
  | Immediate
  | Exclusive
  | Savepoint T.Text

infixr 3 :=

instance Show NamedParam where
  show (k := v) = show (k, toField v)

-- | Exception thrown if a 'Query' was malformed.
-- This may occur if the number of \'@?@\' characters in the query
-- string does not match the number of parameters provided.
data FormatError = FormatError
  { fmtMessage :: String,
    fmtSql :: Sql,
    fmtParams :: [String]
  }
  deriving (Eq, Show)

instance Exception FormatError

unUtf8 :: Sqlite.Direct.Utf8 -> T.Text
unUtf8 (Sqlite.Direct.Utf8 bs) = TE.decodeUtf8 bs

-- | Binds parameters to a prepared statement. Once 'nextRow' returns 'Nothing',
-- the statement must be reset with the 'reset' function before it can be
-- executed again by calling 'nextRow'.
bind :: (ToRow params) => PreparedStatement -> params -> IO ()
bind stmt params = do
  let qp = toRow params
  stmtParamCount <- Sqlite.bindParameterCount stmt
  when (length qp /= fromIntegral stmtParamCount) (throwColumnMismatch qp stmtParamCount)
  mapM_ (errorCheckParamName qp) [1 .. stmtParamCount]
  Sqlite.bind stmt qp
  where
    throwColumnMismatch qp nParams = do
      templ <- getSql stmt
      fmtError
        ( "Sql query contains "
            ++ show nParams
            ++ " params, but "
            ++ show (length qp)
            ++ " arguments given"
        )
        templ
        qp
    errorCheckParamName qp paramNdx = do
      templ <- getSql stmt
      name <- Sqlite.bindParameterName stmt paramNdx
      case name of
        Just n ->
          fmtError
            ("Only unnamed '?' query parameters are accepted, '" ++ T.unpack n ++ "' given")
            templ
            qp
        Nothing -> return ()

-- | Binds named parameters to a prepared statement.
bindNamed :: PreparedStatement -> [NamedParam] -> IO ()
bindNamed stmt params = do
  stmtParamCount <- Sqlite.bindParameterCount stmt
  when (length params /= fromIntegral stmtParamCount) $ throwColumnMismatch stmtParamCount
  mapM_
    ( \(n := v) -> do
        idx <- Sqlite.Direct.bindParameterIndex stmt (Sqlite.Direct.Utf8 . TE.encodeUtf8 $ n)
        case idx of
          Just i ->
            Sqlite.bindSqlData stmt i (toField v)
          Nothing -> do
            templ <- getSql stmt
            fmtError
              ("Unknown named parameter '" ++ T.unpack n ++ "'")
              templ
              params
    )
    params
  where
    throwColumnMismatch nParams = do
      templ <- getSql stmt
      fmtError
        ( "Sql query contains "
            ++ show nParams
            ++ " params, but "
            ++ show (length params)
            ++ " arguments given"
        )
        templ
        params

-- | Resets a statement. This does not reset bound parameters, if any, but
-- allows the statement to be reexecuted again by invoking 'nextRow'.
reset :: PreparedStatement -> IO ()
reset stmt = Sqlite.reset stmt

-- | Return the name of a a particular column in the result set of a
-- 'PreparedStatement'.  Throws an 'ArrayException' if the colum index is out
-- of bounds.
--
-- <http://www.sqlite.org/c3ref/column_name.html>
columnName :: PreparedStatement -> ColumnIndex -> IO T.Text
columnName stmt n = Sqlite.Direct.columnName stmt n >>= takeUtf8
  where
    takeUtf8 (Just s) = return $ unUtf8 s
    takeUtf8 Nothing =
      throwIO (IndexOutOfBounds ("Column index " ++ show n ++ " out of bounds"))

-- | Return number of columns in the query
columnCount :: PreparedStatement -> IO ColumnIndex
columnCount stmt = Sqlite.Direct.columnCount stmt

-- | Binds parameters to a prepared statement, and 'reset's the statement when
-- the callback completes, even in the presence of exceptions.
--
-- Use 'withBind' to reuse prepared statements.  Because it 'reset's the
-- statement /after/ each usage, it avoids a pitfall involving implicit
-- transactions.  Sqlite creates an implicit transaction if you don't say
-- @BEGIN@ explicitly, and does not commit it until all active statements are
-- finished with either 'reset' or 'closeStatement'.
withBind :: (ToRow params) => PreparedStatement -> params -> IO a -> IO a
withBind stmt params io = do
  bind stmt params
  io `finally` reset stmt

-- | Opens a prepared statement. A prepared statement must always be closed with
-- a corresponding call to 'closeStatement' before closing the connection. Use
-- 'nextRow' to iterate on the values returned. Once 'nextRow' returns
-- 'Nothing', you need to invoke 'reset' before reexecuting the statement again
-- with 'nextRow'.
openStatement :: Connection -> Sql -> IO PreparedStatement
openStatement conn (Sql t) = do
  Sqlite.prepare conn t

-- | Closes a prepared statement.
closeStatement :: PreparedStatement -> IO ()
closeStatement stmt = Sqlite.finalize stmt

-- | Opens a prepared statement, executes an action using this statement, and
-- closes the statement, even in the presence of exceptions.
withStatement :: Connection -> Sql -> (PreparedStatement -> IO a) -> IO a
withStatement conn query = bracket (openStatement conn query) closeStatement

-- A version of 'withStatement' which binds parameters.
withStatementParams ::
  (ToRow params) =>
  Connection ->
  Sql ->
  params ->
  (PreparedStatement -> IO a) ->
  IO a
withStatementParams conn template params action =
  withStatement conn template $ \stmt ->
    -- Don't use withBind here, there is no need to reset the parameters since
    -- we're destroying the statement
    bind stmt (toRow params) >> action stmt

-- A version of 'withStatement' which binds named parameters.
withStatementNamedParams ::
  Connection ->
  Sql ->
  [NamedParam] ->
  (PreparedStatement -> IO a) ->
  IO a
withStatementNamedParams conn template namedParams action =
  withStatement conn template $ \stmt -> bindNamed stmt namedParams >> action stmt

-- | Execute an @INSERT@, @UPDATE@, or other Sql query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
execute :: (ToRow q) => Connection -> Sql -> q -> IO ()
execute conn template qs =
  withStatementParams conn template qs $ \stmt ->
    void . Sqlite.step $ stmt

-- | Execute a multi-row @INSERT@, @UPDATE@, or other Sql query that is not
-- expected to return results.
--
-- Throws 'FormatError' if the query could not be formatted correctly.
executeMany :: (ToRow q) => Connection -> Sql -> [q] -> IO ()
executeMany conn template paramRows = withStatement conn template $ \stmt -> do
  forM_ paramRows $ \params ->
    withBind
      stmt
      params
      (void . Sqlite.step $ stmt)


-- | Perform a @SELECT@ or other Sql query that is expected to return
-- results. All results are retrieved and converted before this
-- function returns.
--
-- When processing large results, this function will consume a lot of
-- client-side memory.  Consider using 'fold' instead.
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
select ::
  (ToRow q, FromRow r) =>
  Connection ->
  Sql ->
  q ->
  IO [r]
select = selectWith fromRow

-- | A version of 'query' that does not perform query substitution.
select_ :: (FromRow r) => Connection -> Sql -> IO [r]
select_ = selectWith_ fromRow

-- | A version of 'query' that takes an explicit 'RowParser'.
selectWith :: (ToRow q) => RowParser r -> Connection -> Sql -> q -> IO [r]
selectWith fromRow_ conn templ qs =
  withStatementParams conn templ qs do doFoldToList fromRow_

-- | A version of 'query' that does not perform query substitution and
-- takes an explicit 'RowParser'.
selectWith_ :: RowParser r -> Connection -> Sql -> IO [r]
selectWith_ fromRow_ conn query =
  withStatement conn query do doFoldToList fromRow_

-- | A version of 'query' where the query parameters (placeholders)
-- are named.
--
-- Example:
--
-- @
-- r \<- 'queryNamed' c \"SELECT * FROM posts WHERE id=:id AND date>=:date\" [\":id\" ':=' postId, \":date\" ':=' afterDate]
-- @
selectNamed :: (FromRow r) => Connection -> Sql -> [NamedParam] -> IO [r]
selectNamed conn templ params =
  withStatementNamedParams conn templ params do doFoldToList fromRow

-- | A version of 'execute' that does not perform query substitution.
execute_ :: Connection -> Sql -> IO ()
execute_ conn template =
  withStatement conn template \stmt ->
    void do Sqlite.step stmt

-- | A version of 'execute' where the query parameters (placeholders)
-- are named.
executeNamed :: Connection -> Sql -> [NamedParam] -> IO ()
executeNamed conn template params =
  withStatementNamedParams conn template params $ \stmt ->
    void $ Sqlite.step stmt

-- | Perform a @SELECT@ or other Sql query that is expected to return results.
-- Results are converted and fed into the 'action' callback as they are being
-- retrieved from the database.
--
-- This allows gives the possibility of processing results in constant space
-- (for instance writing them to disk).
--
-- Exceptions that may be thrown:
--
-- * 'FormatError': the query string mismatched with given arguments.
--
-- * 'ResultError': result conversion failed.
fold ::
  (FromRow row, ToRow params) =>
  Connection ->
  Sql ->
  params ->
  (a -> row -> IO a) ->
  IO a ->
  IO a
fold conn query params action initalState =
  withStatementParams conn query params $ \stmt ->
    doFold fromRow stmt action initalState 

doFoldToList :: RowParser row -> PreparedStatement -> IO [row]
doFoldToList fromRow_ stmt =
  reverse <$> doFold fromRow_ stmt (\acc e -> return (e : acc)) (pure [])

-- | A version of 'fold' which does not perform parameter substitution.
fold_ ::
  (FromRow row) =>
  Connection ->
  Sql ->
  (a -> row -> IO a) ->
  IO a ->
  IO a
fold_ conn query action initalState =
  withStatement conn query $ \stmt ->
    doFold fromRow stmt action initalState 

-- | A version of 'fold' where the query parameters (placeholders) are
-- named.
foldNamed ::
  (FromRow row) =>
  Connection ->
  Sql ->
  [NamedParam] ->
  (a -> row -> IO a) ->
  IO a ->
  IO a
foldNamed conn query params action initalState =
  withStatementNamedParams conn query params $ \stmt ->
    doFold fromRow stmt action initalState 

foldPrepared :: (FromRow row) => PreparedStatement -> (a -> row -> IO a)  -> IO a -> IO a
foldPrepared = doFold fromRow

doFold :: RowParser row -> PreparedStatement -> (a -> row -> IO a)  -> IO a -> IO a
doFold fromRow_ stmt action initialAction = do
  initialValue <- initialAction
  loop initialValue
  where
    loop val = do
      maybeNextRow <- nextRowWith fromRow_ stmt
      case maybeNextRow of
        Just row -> do
          val' <- action val row
          val' `seq` loop val'
        Nothing -> return val

-- | Extracts the next row from the prepared statement.
nextRow :: (FromRow r) => PreparedStatement -> IO (Maybe r)
nextRow = nextRowWith fromRow

nextRowWith :: RowParser r -> PreparedStatement -> IO (Maybe r)
nextRowWith fromRow_ stmt = do
  statRes <- Sqlite.step stmt
  case statRes of
    Sqlite.Row -> do
      rowRes <- Sqlite.columns stmt
      let nCols = length rowRes
      row <- convertRow fromRow_ rowRes nCols
      return $ Just row
    Sqlite.Done -> return Nothing

convertRow :: RowParser r -> [SqlData] -> Int -> IO r
convertRow fromRow_ rowRes ncols = do
  let rw = RowParserRO ncols
  case runRowParser fromRow_ rw (0, rowRes) of
    Ok (val, (col, _))
      | col == ncols -> return val
      | otherwise -> errorColumnMismatch (ColumnOutOfBounds col)
    Errors [] -> throwIO $ ConversionFailed "" "" "unknown error"
    Errors [x] ->
      throw x `Control.Exception.catch` (\e -> errorColumnMismatch (e :: ColumnOutOfBounds))
    Errors xs -> throwIO $ ManyErrors xs
  where
    errorColumnMismatch :: ColumnOutOfBounds -> IO r
    errorColumnMismatch (ColumnOutOfBounds c) = do
      let vals = map (\f -> (gettypename f, ellipsis f)) rowRes
      throwIO
        ( ConversionFailed
            (show ncols ++ " values: " ++ show vals)
            ("at least " ++ show c ++ " slots in target type")
            "mismatch between number of columns to convert and number in target type"
        )

    ellipsis :: SqlData -> T.Text
    ellipsis sql
      | T.length bs > 20 = T.take 15 bs `T.append` "[...]"
      | otherwise = bs
      where
        bs = T.pack $ show sql

withTransactionPrivate :: Connection -> IO a -> TransactionType -> IO a
withTransactionPrivate conn action ttype =
  mask $ \restore -> do
    begin
    r <- restore action `onException` rollback
    commit
    return r
  where
    begin = execute_ conn $ case ttype of
      Deferred -> "BEGIN TRANSACTION"
      Immediate -> "BEGIN IMMEDIATE TRANSACTION"
      Exclusive -> "BEGIN EXCLUSIVE TRANSACTION"
      Savepoint name -> Sql $ "SAVEPOINT '" <> name <> "'"
    commit = execute_ conn $ case ttype of
      Savepoint name -> Sql $ "RELEASE '" <> name <> "'"
      _ -> "COMMIT TRANSACTION"
    rollback = execute_ conn $ case ttype of
      Savepoint name -> Sql $ "ROLLBACK TO '" <> name <> "'"
      _ -> "ROLLBACK TRANSACTION"

-- | Run an IO action inside a Sql transaction started with @BEGIN IMMEDIATE
-- TRANSACTION@, which immediately blocks all other database connections from
-- writing.  The default Sqlite3 @BEGIN TRANSACTION@ does not acquire the write
-- lock on @BEGIN@ nor on @SELECT@ but waits until you try to change data.  If
-- the action throws any kind of an exception, the transaction will be rolled
-- back with @ROLLBACK TRANSACTION@.  Otherwise the results are committed with
-- @COMMIT TRANSACTION@.
withImmediateTransaction :: Connection -> IO a -> IO a
withImmediateTransaction conn action =
  withTransactionPrivate conn action Immediate

-- | Run an IO action inside a Sql transaction started with @BEGIN EXCLUSIVE
-- TRANSACTION@, which immediately blocks all other database connections from
-- writing, and other connections from reading (exception: read_uncommitted
-- connections are allowed to read.) If the action throws any kind of an
-- exception, the transaction will be rolled back with @ROLLBACK TRANSACTION@.
-- Otherwise the results are committed with @COMMIT TRANSACTION@.
withExclusiveTransaction :: Connection -> IO a -> IO a
withExclusiveTransaction conn action =
  withTransactionPrivate conn action Exclusive

-- | Returns the rowid of the most recent successful INSERT on the
-- given database connection.
--
-- See also <http://www.sqlite.org/c3ref/last_insert_rowid.html>.
lastInsertRowId :: Connection -> IO Int64
lastInsertRowId = Sqlite.Direct.lastInsertRowId

-- | <http://www.sqlite.org/c3ref/changes.html>
--
-- Return the number of rows that were changed, inserted, or deleted
-- by the most recent @INSERT@, @DELETE@, or @UPDATE@ statement.
changes :: Connection -> IO Int
changes = Sqlite.Direct.changes

-- | <http://www.sqlite.org/c3ref/total_changes.html>
--
-- Return the total number of row changes caused by @INSERT@, @DELETE@,
-- or @UPDATE@ statements since the 'Database' was opened.
totalChanges :: Connection -> IO Int
totalChanges = Sqlite.Direct.totalChanges

-- | Run an IO action inside a Sql transaction started with @BEGIN
-- TRANSACTION@.  If the action throws any kind of an exception, the
-- transaction will be rolled back with @ROLLBACK TRANSACTION@.
-- Otherwise the results are committed with @COMMIT TRANSACTION@.
withTransaction :: Connection -> IO a -> IO a
withTransaction conn action =
  withTransactionPrivate conn action Deferred

fmtError :: (Show v) => String -> Sql -> [v] -> a
fmtError msg q xs =
  throw
    FormatError
      { fmtMessage = msg,
        fmtSql = q,
        fmtParams = map show xs
      }

getSql :: Sqlite.PreparedStatement -> IO Sql
getSql stmt =
  toQuery <$> Sqlite.Direct.statementSql stmt
  where
    toQuery =
      Sql . maybe "no query string" (\(Sqlite.Direct.Utf8 s) -> TE.decodeUtf8 s)

-- $use
-- An example that creates a table 'test', inserts a couple of rows
-- and proceeds to showcase how to update or delete rows.  This
-- example also demonstrates the use of 'lastInsertRowId' (how to
-- refer to a previously inserted row) and 'executeNamed' (an easier
-- to maintain form of query parameter naming).
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import           Control.Applicative
-- >import qualified Data.Text as T
-- >import           Database.Sqlite.Simple
-- >import           Database.Sqlite.Simple.FromRow
-- >
-- >data TestField = TestField Int T.Text deriving (Show)
-- >
-- >instance FromRow TestField where
-- >  fromRow = TestField <$> field <*> field
-- >
-- >instance ToRow TestField where
-- >  toRow (TestField id_ str) = toRow (id_, str)
-- >
-- >main :: IO ()
-- >main = do
-- >  conn <- open "test.db"
-- >  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
-- >  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
-- >  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
-- >  rowId <- lastInsertRowId conn
-- >  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
-- >  r <- query_ conn "SELECT * from test" :: IO [TestField]
-- >  mapM_ print r
-- >  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
-- >  close conn

-- $querytype
--
-- Sql-based applications are somewhat notorious for their
-- susceptibility to attacks through the injection of maliciously
-- crafted data. The primary reason for widespread vulnerability to
-- Sql injections is that many applications are sloppy in handling
-- user data when constructing Sql queries.
--
-- This library provides a 'Query' type and a parameter substitution
-- facility to address both ease of use and security.  A 'Query' is a
-- @newtype@-wrapped 'Text'. It intentionally exposes a tiny API that
-- is not compatible with the 'Text' API; this makes it difficult to
-- construct queries from fragments of strings.  The 'query' and
-- 'execute' functions require queries to be of type 'Query'.
--
-- To most easily construct a query, enable GHC's @OverloadedStrings@
-- language extension and write your query as a normal literal string.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.Sqlite.Simple
-- >
-- > hello = do
-- >   conn <- open "test.db"
-- >   [[x]] <- query_ conn "select 2 + 2"
-- >   print x
--
-- A 'Query' value does not represent the actual query that will be
-- executed, but is a template for constructing the final query.

-- $subst
--
-- Since applications need to be able to construct queries with
-- parameters that change, this library uses Sqlite's parameter
-- binding query substitution capability.
--
-- This library restricts parameter substitution to work only with
-- named parameters and positional arguments with the \"@?@\" syntax.
-- The API does not support for mixing these two types of bindings.
-- Unsupported parameters will be rejected and a 'FormatError' will be
-- thrown.
--
-- You should always use parameter substitution instead of inlining
-- your dynamic parameters into your queries with messy string
-- concatenation.  Sqlite will automatically quote and escape your
-- data into these placeholder parameters; this defeats the single
-- most common injection vector for malicious data.

-- $substpos
--
-- The 'Query' template accepted by 'query', 'execute' and 'fold' can
-- contain any number of \"@?@\" characters.  Both 'query' and
-- 'execute' accept a third argument, typically a tuple. When the
-- query executes, the first \"@?@\" in the template will be replaced
-- with the first element of the tuple, the second \"@?@\" with the
-- second element, and so on.  This substitution happens inside the
-- native Sqlite implementation.
--
-- For example, given the following 'Query' template:
--
-- > select * from user where first_name = ? and age > ?
--
-- And a tuple of this form:
--
-- > ("Boris" :: String, 37 :: Int)
--
-- The query to be executed will look like this after substitution:
--
-- > select * from user where first_name = 'Boris' and age > 37
--
-- If there is a mismatch between the number of \"@?@\" characters in
-- your template and the number of elements in your tuple, a
-- 'FormatError' will be thrown.
--
-- Note that the substitution functions do not attempt to parse or
-- validate your query. It's up to you to write syntactically valid
-- Sql, and to ensure that each \"@?@\" in your query template is
-- matched with the right tuple element.

-- $substnamed
--
-- Named parameters are accepted by 'queryNamed', 'executeNamed' and
-- 'foldNamed'.  These functions take a list of 'NamedParam's which
-- are key-value pairs binding a value to an argument name.  As is the
-- case with \"@?@\" parameters, named parameters are automatically
-- escaped by the Sqlite library.  The parameter names are prefixed
-- with either @:@ or @\@@, e.g. @:foo@ or @\@foo@.
--
-- Example:
--
-- @
-- r \<- 'queryNamed' c \"SELECT id,text FROM posts WHERE id = :id AND date >= :date\" [\":id\" ':=' postId, \":date\" ':=' afterDate]
-- @
--
-- Note that you can mix different value types in the same list.
-- E.g., the following is perfectly legal:
--
-- @
-- [\":id\" ':=' (3 :: Int), \":str\" ':=' (\"foo\" :: String)]
-- @
--
-- The parameter name (or key) in the 'NamedParam' must match exactly
-- the name written in the Sql query.  E.g., if you used @:foo@ in
-- your Sql statement, you need to use @\":foo\"@ as the parameter
-- key, not @\"foo\"@.  Some libraries like Python's sqlite3
-- automatically drop the @:@ character from the name.

-- $inference
--
-- Automated type inference means that you will often be able to avoid
-- supplying explicit type signatures for the elements of a tuple.
-- However, sometimes the compiler will not be able to infer your
-- types. Consider a case where you write a numeric literal in a
-- parameter tuple:
--
-- > query conn "select ? + ?" (40,2)
--
-- The above query will be rejected by the compiler, because it does
-- not know the specific numeric types of the literals @40@ and @2@.
-- This is easily fixed:
--
-- > query conn "select ? + ?" (40 :: Double, 2 :: Double)
--
-- The same kind of problem can arise with string literals if you have
-- the @OverloadedStrings@ language extension enabled.  Again, just
-- use an explicit type signature if this happens.

-- $only_param
--
-- Haskell lacks a single-element tuple type, so if you have just one
-- value you want substituted into a query, what should you do?
--
-- To represent a single value @val@ as a parameter, write a singleton
-- list @[val]@, use 'Just' @val@, or use 'Only' @val@.
--
-- Here's an example using a singleton list:
--
-- > execute conn "insert into users (first_name) values (?)"
-- >              ["Nuala"]
--
-- Or you can use named parameters which do not have this restriction.

-- $result
--
-- The 'query' and 'query_' functions return a list of values in the
-- 'FromRow' typeclass. This class performs automatic extraction
-- and type conversion of rows from a query result.
--
-- Here is a simple example of how to extract results:
--
-- > import qualified Data.Text as T
-- >
-- > xs <- query_ conn "select name,age from users"
-- > forM_ xs $ \(name,age) ->
-- >   putStrLn $ T.unpack name ++ " is " ++ show (age :: Int)
--
-- Notice two important details about this code:
--
-- * The number of columns we ask for in the query template must
--   exactly match the number of elements we specify in a row of the
--   result tuple.  If they do not match, a 'ResultError' exception
--   will be thrown.
--
-- * Sometimes, the compiler needs our help in specifying types. It
--   can infer that @name@ must be a 'Text', due to our use of the
--   @unpack@ function. However, we have to tell it the type of @age@,
--   as it has no other information to determine the exact type.

-- $null
--
-- The type of a result tuple will look something like this:
--
-- > (Text, Int, Int)
--
-- Although Sql can accommodate @NULL@ as a value for any of these
-- types, Haskell cannot. If your result contains columns that may be
-- @NULL@, be sure that you use 'Maybe' in those positions of of your
-- tuple.
--
-- > (Text, Maybe Int, Int)
--
-- If 'query' encounters a @NULL@ in a row where the corresponding
-- Haskell type is not 'Maybe', it will throw a 'ResultError'
-- exception.

-- $only_result
--
-- To specify that a query returns a single-column result, use the
-- 'Only' type.
--
-- > xs <- query_ conn "select id from users"
-- > forM_ xs $ \(Only dbid) -> {- ... -}

-- $types
--
-- Conversion of Sql values to Haskell values is somewhat
-- permissive. Here are the rules.
--
-- * For numeric types, any Haskell type that can accurately represent
--   an Sqlite INTEGER is considered \"compatible\".
--
-- * If a numeric incompatibility is found, 'query' will throw a
--   'ResultError'.
--
-- * Sqlite's TEXT type is always encoded in UTF-8.  Thus any text
--   data coming from an Sqlite database should always be compatible
--   with Haskell 'String' and 'Text' types.
--
-- * Sqlite's BLOB type will only be conversible to a Haskell
--   'ByteString'.
--
-- You can extend conversion support to your own types be adding your
-- own 'FromField' / 'ToField' instances.
