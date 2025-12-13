{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Sqlite
  ( -- * Connection management
    open,
    openV2,
    openV2NoMutexReadWrite,
    VFS (..),
    OpenV2Flag (..),
    OpenV2Mode (..),
    close,

    -- * Simple query execution

    -- | <https://sqlite.org/c3ref/exec.html>
    exec,
    execPrint,
    execWithCallback,
    ExecCallback,

    -- * Statement management
    prepare,
    -- prepareUtf8,
    step,
    stepNoCB,
    reset,
    finalize,
    clearBindings,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    columnCount,
    columnName,

    -- * Binding values to a prepared statement

    -- | <https://www.sqlite.org/c3ref/bind_blob.html>
    bindSqlData,
    bind,
    bindNamed,
    bindInt,
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindZeroBlob,
    bindNull,

    -- * Reading the result row

    -- | <https://www.sqlite.org/c3ref/column_blob.html>
    --
    -- Warning: 'column' and 'columns' will throw a 'DecodeError' if any @TEXT@
    -- datum contains invalid UTF-8.
    column,
    columns,
    typedColumns,
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * Result statistics
    lastInsertRowId,
    changes,
    totalChanges,

    -- * Create custom Sql functions
    createFunction,
    createAggregate,
    deleteFunction,

    -- ** Extract function arguments
    funcArgCount,
    funcArgType,
    funcArgInt64,
    funcArgDouble,
    funcArgText,
    funcArgBlob,

    -- ** Set the result of a function
    funcResultSqlData,
    funcResultInt64,
    funcResultDouble,
    funcResultText,
    funcResultBlob,
    funcResultZeroBlob,
    funcResultNull,
    getFuncContextConnection,

    -- * Create custom collations
    createCollation,
    deleteCollation,

    -- * Interrupting a long-running query
    interrupt,
    interruptibly,

    -- * Incremental blob I/O
    blobOpen,
    blobClose,
    blobReopen,
    blobBytes,
    blobRead,
    blobReadBuf,
    blobWrite,

    -- * Online Backup API

    -- | <https://www.sqlite.org/backup.html> and
    -- <https://www.sqlite.org/c3ref/backup_finish.html>
    backupInit,
    backupFinish,
    backupStep,
    backupRemaining,
    backupPagecount,

    -- * Types
    Connection,
    PreparedStatement,
    SqlData (..),
    SqliteException (..),
    ColumnType (..),
    FuncContext,
    FuncArgs,
    Blob,
    Backup,

    -- ** Results and errors
    StepResult (..),
    BackupStepResult (..),
    Error (..),

    -- ** Special integers
    ParamIndex (..),
    ColumnIndex (..),
    ColumnCount,
    ArgCount (..),
    ArgIndex,
  )
where

-- Re-exported from Database.Sqlite3.Direct without modification.
-- Note that if this module were in another package, source links would not
-- be generated for these functions.

import Control.Concurrent
import Control.Exception
import Control.Monad (when, zipWithM, zipWithM_)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException (..), lenientDecode)
import Data.Text.IO qualified as T
import Foreign.Ptr (Ptr)
import Sqlite.Direct
  ( ArgCount (..),
    ArgIndex,
    Backup,
    BackupStepResult (..),
    Blob,
    ColumnCount,
    ColumnIndex (..),
    ColumnType (..),
    Connection,
    Error (..),
    FuncArgs,
    FuncContext,
    OpenV2Flag (..),
    OpenV2Mode (..),
    ParamIndex (..),
    PreparedStatement,
    StepResult (..),
    Utf8 (..),
    backupPagecount,
    backupRemaining,
    bindParameterCount,
    blobBytes,
    changes,
    clearBindings,
    columnBlob,
    columnCount,
    columnDouble,
    columnInt64,
    columnType,
    funcArgBlob,
    funcArgCount,
    funcArgDouble,
    funcArgInt64,
    funcArgType,
    funcResultBlob,
    funcResultDouble,
    funcResultInt64,
    funcResultNull,
    funcResultZeroBlob,
    getFuncContextConnection,
    interrupt,
    lastInsertRowId,
    totalChanges,
  )
import Sqlite.Direct qualified as Direct
import Prelude hiding (error)

data SqlData
  = SqlInteger !Int64
  | SqlFloat !Double
  | SqlText !Text
  | SqlBlob !ByteString
  | SqlNull
  deriving (Eq, Show)

-- | Exception thrown when Sqlite3 reports an error.
--
-- direct-sqlite may throw other types of exceptions if you misuse the API.
data SqliteException = SqliteException
  { -- | Error code returned by API call
    sqliteError :: !Error,
    -- | Text describing the error
    sqliteErrorDetails :: Text,
    -- | Indicates what action produced this error,
    --   e.g. @exec \"SELECT * FROM foo\"@
    sqliteErrorContext :: Text
  }
  deriving (Eq)

-- NB: SqliteException is lazy in 'sqliteErrorDetails' and 'sqliteErrorContext',
-- to defer message construction in the case where a user catches and
-- immediately handles the error.

instance Show SqliteException where
  show
    SqliteException
      { sqliteError = code,
        sqliteErrorDetails = details,
        sqliteErrorContext = context
      } =
      T.unpack $
        T.concat
          [ "Sqlite3 returned ",
            T.pack $ show code,
            " while attempting to perform ",
            context,
            ": ",
            details
          ]

instance Exception SqliteException

-- | Like 'decodeUtf8', but substitute a custom error message if
-- decoding fails.
fromUtf8 :: String -> Utf8 -> IO Text
fromUtf8 desc utf8 = evaluate do fromUtf8' desc utf8

fromUtf8' :: String -> Utf8 -> Text
fromUtf8' desc (Utf8 bs) =
  decodeUtf8With (\_ c -> throw (DecodeError desc c)) bs

toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . encodeUtf8

data DetailSource
  = DetailConnection Connection
  | DetailStatement PreparedStatement
  | DetailMessage Utf8

renderDetailSource :: DetailSource -> IO Utf8
renderDetailSource src = case src of
  DetailConnection db ->
    Direct.errmsg db
  DetailStatement stmt -> do
    db <- Direct.getStatementConnection stmt
    Direct.errmsg db
  DetailMessage msg ->
    return msg

throwSqliteException :: DetailSource -> Text -> Error -> IO a
throwSqliteException detailSource context error = do
  Utf8 details <- renderDetailSource detailSource
  throwIO
    SqliteException
      { sqliteError = error,
        sqliteErrorDetails = decodeUtf8With lenientDecode details,
        sqliteErrorContext = context
      }

checkError :: DetailSource -> Text -> Either Error a -> IO a
checkError ds fn = either (throwSqliteException ds fn) return

checkErrorMsg :: Text -> Either (Error, Utf8) a -> IO a
checkErrorMsg fn result = case result of
  Left (err, msg) -> throwSqliteException (DetailMessage msg) fn err
  Right a -> return a

appendShow :: (Show a) => Text -> a -> Text
appendShow txt a = txt `T.append` (T.pack . show) a

-- | <https://www.sqlite.org/c3ref/open.html>
open ::
  -- | Connection filename.
  Text ->
  IO Connection
open path = do
  result <- Direct.open (toUtf8 path)
  checkErrorMsg ("open " `appendShow` path) result

-- | <https://www.sqlite.org/c3ref/open.html>
openV2 ::
  -- | Name of VFS module to use.
  VFS ->
  [OpenV2Flag] ->
  OpenV2Mode ->
  -- | Database filename.
  Text ->
  IO Connection
openV2 vfs flags mode path = do
  let mvfs = case vfs of
        DefaultVFS -> Nothing
        VFSWithName vfsName -> Just vfsName
  Direct.openV2 (toUtf8 <$> mvfs) flags mode (toUtf8 path)
    >>= checkErrorMsg ("openV2 " `appendShow` path)

-- | Typical set of arguments for a connection pool.
-- https://www.sqlite.org/threadsafe.html
-- SQLITE_OPEN_NOMUTEX flag causes the database connection to be in the multi-thread mode
-- In this mode, SQLite can be safely used by multiple threads provided that no
-- single database connection nor any object derived from database connection,
-- such as a prepared statement, is used in two or more threads at the same
-- time.
openV2NoMutexReadWrite ::
  -- | Database filename.
  Text ->
  IO Connection
openV2NoMutexReadWrite =
  Sqlite.openV2 DefaultVFS [OpenV2ExtendedResultCode, OpenV2NoMutex] OpenV2ReadWrite

data VFS
  = DefaultVFS
  | VFSWithName Text
  deriving (Show, Eq)

-- | <https://www.sqlite.org/c3ref/close.html>
close :: Connection -> IO ()
close db = do
  result <- Direct.close db
  checkError (DetailConnection db) "close" result

-- | Make it possible to interrupt the given database operation with an
-- asynchronous exception.  This only works if the program is compiled with
-- base >= 4.3 and @-threaded@.
--
-- It works by running the callback in a forked thread.  If interrupted,
-- it uses 'interrupt' to try to stop the operation.
interruptibly :: Connection -> IO a -> IO a
interruptibly db io
  | rtsSupportsBoundThreads =
      mask \restore -> do
        mv <- newEmptyMVar
        tid <- forkIO do try' (restore io) >>= putMVar mv

        let interruptAndWait =
              -- Don't let a second exception interrupt us.  Otherwise,
              -- the operation will dangle in the background, which could
              -- be really bad if it uses locally-allocated resources.
              uninterruptibleMask_ do
                -- Tell Sqlite3 to interrupt the current query.
                interrupt db

                -- Interrupt the thread in case it's blocked for some
                -- other reason.
                --
                -- NOTE: killThread blocks until the exception is delivered.
                -- That's fine, since we're going to wait for the thread
                -- to finish anyway.
                killThread tid

                -- Wait for the forked thread to finish.
                _ <- takeMVar mv
                return ()

        e <- takeMVar mv `onException` interruptAndWait
        either throwIO return e
  | otherwise = io
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = try

-- | Execute zero or more Sql statements delimited by semicolons.
exec :: Connection -> Text -> IO ()
exec db sql = do
  result <- Direct.exec db (toUtf8 sql)
  checkErrorMsg ("exec " `appendShow` sql) result

-- | Like 'exec', but print result rows to 'System.IO.stdout'.
--
-- This is mainly for convenience when experimenting in GHCi.
-- The output format may change in the future.
execPrint :: Connection -> Text -> IO ()
execPrint !db !sql =
  interruptibly db do
    execWithCallback db sql do
      \_count _colnames -> T.putStrLn . showValues
  where
    -- This mimics sqlite3's default output mode.  It displays a NULL and an
    -- empty string identically.
    showValues = T.intercalate "|" . map (fromMaybe "")

-- | Like 'exec', but invoke the callback for each result row.
execWithCallback :: Connection -> Text -> ExecCallback -> IO ()
execWithCallback db sql cb = do
  result <- Direct.execWithCallback db (toUtf8 sql) cb'
  checkErrorMsg ("execWithCallback " `appendShow` sql) result
  where
    -- We want 'names' computed once and shared with every call.
    cb' count namesUtf8 =
      let names = map fromUtf8'' namesUtf8
          {-# NOINLINE names #-}
       in cb count names . map (fmap fromUtf8'')

    fromUtf8'' = fromUtf8' "Database.Sqlite3.execWithCallback: Invalid UTF-8"

type ExecCallback =
  -- | Number of columns, which is the number of items in
  --   the following lists.  This will be the same for
  --   every row.
  ColumnCount ->
  -- | List of column names.  This will be the same
  --   for every row.
  [Text] ->
  -- | List of column values, as returned by 'columnText'.
  [Maybe Text] ->
  IO ()

-- | <https://www.sqlite.org/c3ref/prepare.html>
--
-- Unlike 'exec', 'prepare' only executes the first statement, and ignores
-- subsequent statements.
--
-- If the query string contains no Sql statements, this 'fail's.
prepare :: Connection -> Text -> IO PreparedStatement
prepare db sql = prepareUtf8 db (toUtf8 sql)

-- | <https://www.sqlite.org/c3ref/prepare.html>
--
-- It can help to avoid redundant Utf8 to Text conversion if you already
-- have Utf8
--
-- If the query string contains no Sql statements, this 'fail's.
prepareUtf8 :: Connection -> Utf8 -> IO PreparedStatement
prepareUtf8 db sql = do
  result <- Direct.prepare db sql
  m <- checkError (DetailConnection db) ("prepare " `appendShow` sql) result
  case m of
    Nothing -> fail "Direct.Sqlite3.prepare: empty query string"
    Just stmt -> return stmt

-- | <https://www.sqlite.org/c3ref/step.html>
step :: PreparedStatement -> IO StepResult
step statement = do
  result <- Direct.step statement
  checkError (DetailStatement statement) "step" result

-- | <https://www.sqlite.org/c3ref/step.html>
--
-- Faster step for statements that don't callback to Haskell
-- functions (e.g. by using custom Sql functions).
stepNoCB :: PreparedStatement -> IO StepResult
stepNoCB statement = do
  result <- Direct.stepNoCB statement
  checkError (DetailStatement statement) "stepNoCB" result

-- Note: sqlite3_reset and sqlite3_finalize return an error code if the most
-- recent sqlite3_step indicated an error.  I think these are the only times
-- these functions return an error (barring memory corruption and misuse of the API).
--
-- We don't replicate that behavior here.  Instead, 'reset' and 'finalize'
-- discard the error.  Otherwise, we would get "double jeopardy".
-- For example:
--
--  ok <- try $ step stmt :: IO (Either SqliteException StepResult)
--  finalize stmt
--
-- If 'finalize' threw its error, it would throw the exception the user was
-- trying to catch.
--
-- 'reset' and 'finalize' might return a different error than the step that
-- failed, leading to more cryptic error messages [1].  But we're not
-- completely sure about this.
--
--  [1]: https://github.com/yesodweb/persistent/issues/92#issuecomment-7806421

-- | <https://www.sqlite.org/c3ref/reset.html>
--
-- Note that in the C API, @sqlite3_reset@ returns an error code if the most
-- recent @sqlite3_step@ indicated an error.  We do not replicate that behavior
-- here.  'reset' never throws an exception.
reset :: PreparedStatement -> IO ()
reset statement = do
  _ <- Direct.reset statement
  return ()

-- | <https://www.sqlite.org/c3ref/finalize.html>
--
-- Like 'reset', 'finalize' never throws an exception.
finalize :: PreparedStatement -> IO ()
finalize statement = do
  _ <- Direct.finalize statement
  return ()

-- | <https://www.sqlite.org/c3ref/bind_parameter_name.html>
--
-- Return the N-th Sql parameter name.
--
-- Named parameters are returned as-is.  E.g. \":v\" is returned as
-- @Just \":v\"@.  Unnamed parameters, however, are converted to
-- @Nothing@.
--
-- Note that the parameter index starts at 1, not 0.
bindParameterName :: PreparedStatement -> ParamIndex -> IO (Maybe Text)
bindParameterName stmt idx = do
  m <- Direct.bindParameterName stmt idx
  case m of
    Nothing -> return Nothing
    Just name -> Just <$> fromUtf8 desc name
  where
    desc = "Database.Sqlite3.bindParameterName: Invalid UTF-8"

-- | <https://www.sqlite.org/c3ref/column_name.html>
--
-- Return the name of a result column.  If the column index is out of range,
-- return 'Nothing'.
columnName :: PreparedStatement -> ColumnIndex -> IO (Maybe Text)
columnName stmt idx = do
  m <- Direct.columnName stmt idx
  case m of
    Just name -> Just <$> fromUtf8 desc name
    Nothing -> do
      -- sqlite3_column_name only returns NULL if memory allocation fails
      -- or if the column index is out of range.
      count <- Direct.columnCount stmt
      if idx >= 0 && idx < count
        then throwIO outOfMemory
        else return Nothing
  where
    desc = "Database.Sqlite3.columnName: Invalid UTF-8"
    outOfMemory =
      SqliteException
        { sqliteError = ErrorNoMemory,
          sqliteErrorDetails = "out of memory (sqlite3_column_name returned NULL)",
          sqliteErrorContext = "column name"
        }

bindBlob :: PreparedStatement -> ParamIndex -> ByteString -> IO ()
bindBlob statement parameterIndex byteString = do
  result <- Direct.bindBlob statement parameterIndex byteString
  checkError (DetailStatement statement) "bind blob" result

bindZeroBlob :: PreparedStatement -> ParamIndex -> Int -> IO ()
bindZeroBlob statement parameterIndex len = do
  result <- Direct.bindZeroBlob statement parameterIndex len
  checkError (DetailStatement statement) "bind zeroblob" result

bindDouble :: PreparedStatement -> ParamIndex -> Double -> IO ()
bindDouble statement parameterIndex datum = do
  result <- Direct.bindDouble statement parameterIndex datum
  checkError (DetailStatement statement) "bind double" result

bindInt :: PreparedStatement -> ParamIndex -> Int -> IO ()
bindInt statement parameterIndex datum = do
  result <- Direct.bindInt64 statement parameterIndex (fromIntegral datum)
  checkError (DetailStatement statement) "bind int" result

bindInt64 :: PreparedStatement -> ParamIndex -> Int64 -> IO ()
bindInt64 statement parameterIndex datum = do
  result <- Direct.bindInt64 statement parameterIndex datum
  checkError (DetailStatement statement) "bind int64" result

bindNull :: PreparedStatement -> ParamIndex -> IO ()
bindNull statement parameterIndex = do
  result <- Direct.bindNull statement parameterIndex
  checkError (DetailStatement statement) "bind null" result

bindText :: PreparedStatement -> ParamIndex -> Text -> IO ()
bindText statement parameterIndex text = do
  result <- Direct.bindText statement parameterIndex (toUtf8 text)
  checkError (DetailStatement statement) "bind text" result

-- | If the index is not between 1 and 'bindParameterCount' inclusive, this
-- fails with 'ErrorRange'.  Otherwise, it succeeds, even if the query skips
-- this index by using numbered parameters.
--
-- Example:
--
-- >> stmt <- prepare conn "SELECT ?1, ?3, ?5"
-- >> bindSqlData stmt 1 (SqlInteger 1)
-- >> bindSqlData stmt 2 (SqlInteger 2)
-- >> bindSqlData stmt 6 (SqlInteger 6)
-- >*** Exception: Sqlite3 returned ErrorRange while attempting to perform bind int64.
-- >> step stmt >> columns stmt
-- >[SqlInteger 1,SqlNull,SqlNull]
bindSqlData :: PreparedStatement -> ParamIndex -> SqlData -> IO ()
bindSqlData statement idx datum =
  case datum of
    SqlInteger v -> bindInt64 statement idx v
    SqlFloat v -> bindDouble statement idx v
    SqlText v -> bindText statement idx v
    SqlBlob v -> bindBlob statement idx v
    SqlNull -> bindNull statement idx

-- | Convenience function for binding values to all parameters.  This will
-- 'fail' if the list has the wrong number of parameters.
bind :: PreparedStatement -> [SqlData] -> IO ()
bind statement sqlData = do
  ParamIndex nParams <- bindParameterCount statement
  when (nParams /= length sqlData) do
    fail
      ( "mismatched parameter count for bind.  Prepared statement "
          ++ "needs "
          ++ show nParams
          ++ ", "
          ++ show (length sqlData)
          ++ " given"
      )
  zipWithM_ (bindSqlData statement) [1 ..] sqlData

-- | Convenience function for binding named values to all parameters.
-- This will 'fail' if the list has the wrong number of parameters or
-- if an unknown name is used.
--
-- Example:
--
-- @
-- stmt <- prepare conn \"SELECT :foo + :bar\"
-- bindNamed stmt [(\":foo\", SqlInteger 1), (\":bar\", SqlInteger 2)]
-- @
bindNamed :: PreparedStatement -> [(T.Text, SqlData)] -> IO ()
bindNamed statement params = do
  ParamIndex nParams <- bindParameterCount statement
  when (nParams /= length params) do
    fail
      ( "mismatched parameter count for bind.  Prepared statement "
          ++ "needs "
          ++ show nParams
          ++ ", "
          ++ show (length params)
          ++ " given"
      )
  mapM_ bindIdx params
  where
    bindIdx (name, val) = do
      idx <- Direct.bindParameterIndex statement do toUtf8 name
      case idx of
        Just i ->
          bindSqlData statement i val
        Nothing ->
          fail ("unknown named parameter " ++ show name)

-- | This will throw a 'DecodeError' if the datum contains invalid UTF-8.
-- If this behavior is undesirable, you can use 'Direct.columnText' from
-- "Database.Sqlite3.Direct", which does not perform conversion to 'Text'.
columnText :: PreparedStatement -> ColumnIndex -> IO Text
columnText statement columnIndex = do
  result <- Direct.columnText statement columnIndex
  fromUtf8 "Database.Sqlite3.columnText: Invalid UTF-8" result

column :: PreparedStatement -> ColumnIndex -> IO SqlData
column statement idx = do
  theType <- columnType statement idx
  typedColumn theType statement idx

columns :: PreparedStatement -> IO [SqlData]
columns statement = do
  count <- columnCount statement
  mapM (column statement) [0 .. count - 1]

typedColumn :: ColumnType -> PreparedStatement -> ColumnIndex -> IO SqlData
typedColumn theType statement idx = case theType of
  IntegerColumn -> SqlInteger <$> columnInt64 statement idx
  FloatColumn -> SqlFloat <$> columnDouble statement idx
  TextColumn -> SqlText <$> columnText statement idx
  BlobColumn -> SqlBlob <$> columnBlob statement idx
  NullColumn -> return SqlNull

-- | This avoids extra API calls using the list of column types.
-- If passed types do not correspond to the actual types, the values will be
-- converted according to the rules at <https://www.sqlite.org/c3ref/column_blob.html>.
-- If the list contains more items that number of columns, the result is undefined.
typedColumns :: PreparedStatement -> [Maybe ColumnType] -> IO [SqlData]
typedColumns statement = zipWithM f [0 ..]
  where
    f idx theType = case theType of
      Nothing -> column statement idx
      Just t -> typedColumn t statement idx

-- | <https://sqlite.org/c3ref/create_function.html>
--
-- Create a custom Sql function or redefine the behavior of an existing
-- function. If the function is deterministic, i.e. if it always returns the
-- same result given the same input, you can set the boolean flag to let
-- @sqlite@ perform additional optimizations.
createFunction ::
  Connection ->
  -- | Name of the function.
  Text ->
  -- | Number of arguments. 'Nothing' means that the
  --   function accepts any number of arguments.
  Maybe ArgCount ->
  -- | Is the function deterministic?
  Bool ->
  -- | Implementation of the function.
  (FuncContext -> FuncArgs -> IO ()) ->
  IO ()
createFunction db name nArgs isDet fun = do
  result <- Direct.createFunction db (toUtf8 name) nArgs isDet fun
  checkError (DetailConnection db) ("createFunction " `appendShow` name) result

-- | Like 'createFunction' except that it creates an aggregate function.
createAggregate ::
  Connection ->
  -- | Name of the function.
  Text ->
  -- | Number of arguments.
  Maybe ArgCount ->
  -- | Initial aggregate state.
  a ->
  -- | Process one row and update the aggregate state.
  (FuncContext -> FuncArgs -> a -> IO a) ->
  -- | Called after all rows have been processed.
  --   Can be used to construct the returned value
  --   from the aggregate state.
  (FuncContext -> a -> IO ()) ->
  IO ()
createAggregate db name nArgs initSt xStep xFinal = do
  result <- Direct.createAggregate db (toUtf8 name) nArgs initSt xStep xFinal
  checkError (DetailConnection db) ("createAggregate " `appendShow` name) result

-- | Delete an Sql function (scalar or aggregate).
deleteFunction :: Connection -> Text -> Maybe ArgCount -> IO ()
deleteFunction db name nArgs = do
  result <- Direct.deleteFunction db (toUtf8 name) nArgs
  checkError (DetailConnection db) ("deleteFunction " `appendShow` name) result

funcArgText :: FuncArgs -> ArgIndex -> IO Text
funcArgText args argIndex = do
  result <- Direct.funcArgText args argIndex
  fromUtf8 "Database.Sqlite3.funcArgText: Invalid UTF-8" result

funcResultSqlData :: FuncContext -> SqlData -> IO ()
funcResultSqlData ctx datum =
  case datum of
    SqlInteger v -> funcResultInt64 ctx v
    SqlFloat v -> funcResultDouble ctx v
    SqlText v -> funcResultText ctx v
    SqlBlob v -> funcResultBlob ctx v
    SqlNull -> funcResultNull ctx

funcResultText :: FuncContext -> Text -> IO ()
funcResultText ctx value =
  Direct.funcResultText ctx (toUtf8 value)

-- | <https://www.sqlite.org/c3ref/create_collation.html>
createCollation ::
  Connection ->
  -- | Name of the collation.
  Text ->
  -- | Comparison function.
  (Text -> Text -> Ordering) ->
  IO ()
createCollation db name cmp = do
  result <- Direct.createCollation db (toUtf8 name) cmp'
  checkError (DetailConnection db) ("createCollation " `appendShow` name) result
  where
    cmp' (Utf8 s1) (Utf8 s2) = cmp (fromUtf8'' s1) (fromUtf8'' s2)
    -- avoid throwing exceptions as much as possible
    fromUtf8'' = decodeUtf8With lenientDecode

-- | Delete a collation.
deleteCollation :: Connection -> Text -> IO ()
deleteCollation db name = do
  result <- Direct.deleteCollation db (toUtf8 name)
  checkError (DetailConnection db) ("deleteCollation " `appendShow` name) result

-- | <https://www.sqlite.org/c3ref/blob_open.html>
--
-- Open a blob for incremental I/O.
blobOpen ::
  Connection ->
  -- | The symbolic name of the database (e.g. "main").
  Text ->
  -- | The table name.
  Text ->
  -- | The column name.
  Text ->
  -- | The @ROWID@ of the row.
  Int64 ->
  -- | Open the blob for read-write.
  Bool ->
  IO Blob
blobOpen db zDb zTable zColumn rowid rw = do
  result <- Direct.blobOpen db (toUtf8 zDb) (toUtf8 zTable) (toUtf8 zColumn) rowid rw
  checkError (DetailConnection db) "blobOpen" result

-- | <https://www.sqlite.org/c3ref/blob_close.html>
blobClose :: Blob -> IO ()
blobClose blob@(Direct.Blob db _) = do
  result <- Direct.blobClose blob
  checkError (DetailConnection db) "blobClose" result

-- | <https://www.sqlite.org/c3ref/blob_reopen.html>
blobReopen ::
  Blob ->
  -- | The @ROWID@ of the row.
  Int64 ->
  IO ()
blobReopen blob@(Direct.Blob db _) rowid = do
  result <- Direct.blobReopen blob rowid
  checkError (DetailConnection db) "blobReopen" result

-- | <https://www.sqlite.org/c3ref/blob_read.html>
blobRead ::
  Blob ->
  -- | Number of bytes to read.
  Int ->
  -- | Offset within the blob.
  Int ->
  IO ByteString
blobRead blob@(Direct.Blob db _) len offset = do
  result <- Direct.blobRead blob len offset
  checkError (DetailConnection db) "blobRead" result

blobReadBuf :: Blob -> Ptr a -> Int -> Int -> IO ()
blobReadBuf blob@(Direct.Blob db _) buf len offset = do
  result <- Direct.blobReadBuf blob buf len offset
  checkError (DetailConnection db) "blobReadBuf" result

-- | <https://www.sqlite.org/c3ref/blob_write.html>
blobWrite ::
  Blob ->
  ByteString ->
  -- | Offset within the blob.
  Int ->
  IO ()
blobWrite blob@(Direct.Blob db _) bs offset = do
  result <- Direct.blobWrite blob bs offset
  checkError (DetailConnection db) "blobWrite" result

backupInit ::
  -- | Destination database handle.
  Connection ->
  -- | Destination database name.
  Text ->
  -- | Source database handle.
  Connection ->
  -- | Source database name.
  Text ->
  IO Backup
backupInit dstDb dstName srcDb srcName = do
  result <- Direct.backupInit dstDb (toUtf8 dstName) srcDb (toUtf8 srcName)
  checkError (DetailConnection dstDb) "backupInit" result

backupFinish :: Backup -> IO ()
backupFinish backup@(Direct.Backup dstDb _) = do
  result <- Direct.backupFinish backup
  checkError (DetailConnection dstDb) "backupFinish" result

backupStep :: Backup -> Int -> IO BackupStepResult
backupStep backup pages = do
  result <- Direct.backupStep backup pages
  -- it appears that sqlite does not generate an
  -- error message when sqlite3_backup_step fails
  checkError (DetailMessage "failed") "backupStep" result
