{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This API is a slightly lower-level version of "Database.Sqlite3".  Namely:
--
--  * It returns errors instead of throwing them.
--
--  * It only uses cheap conversions.  None of these bindings convert from
--    'String' or 'T.Text'.
module Sqlite.Direct
  ( -- * Connection management
    open,
    openV2,
    OpenV2Flag (..),
    OpenV2Mode (..),
    close,
    errcode,
    errmsg,
    setTrace,
    getAutoCommit,
    setSharedCacheEnabled,

    -- * Simple query execution

    -- | <https://sqlite.org/c3ref/exec.html>
    exec,
    execWithCallback,
    ExecCallback,

    -- * Statement management
    prepare,
    getStatementConnection,
    step,
    stepNoCB,
    reset,
    finalize,
    clearBindings,
    statementSql,

    -- * Parameter and column information
    bindParameterCount,
    bindParameterName,
    bindParameterIndex,
    columnCount,
    columnName,

    -- * Binding values to a prepared statement

    -- | <https://www.sqlite.org/c3ref/bind_blob.html>
    bindInt64,
    bindDouble,
    bindText,
    bindBlob,
    bindZeroBlob,
    bindNull,

    -- * Reading the result row

    -- | <https://www.sqlite.org/c3ref/column_blob.html>
    columnType,
    columnInt64,
    columnDouble,
    columnText,
    columnBlob,

    -- * control loading of extensions
    setLoadExtensionEnabled,

    -- * Result statistics
    lastInsertRowId,
    changes,
    totalChanges,

    -- * Create custom SQL functions
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
    Connection (..),
    PreparedStatement (..),
    ColumnType (..),
    FuncContext (..),
    FuncArgs (..),
    Blob (..),
    Backup (..),

    -- ** Results and errors
    StepResult (..),
    BackupStepResult (..),
    Error (..),

    -- ** Special types
    Utf8 (..),
    ParamIndex (..),
    ColumnIndex (..),
    ColumnCount,
    ArgCount (..),
    ArgIndex,
  )
where

import Control.Exception as E
import Control.Monad (join, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Foreign
import Foreign.C
import Sqlite.Bindings
import System.IO.Unsafe qualified as IOU

newtype Connection = Connection (Ptr CConnection)
  deriving (Eq, Show)

newtype PreparedStatement = PreparedStatement (Ptr CStatement)
  deriving (Eq, Show)

data StepResult
  = Row
  | Done
  deriving (Eq, Show)

data BackupStepResult
  = -- | There are still more pages to be copied.
    BackupOK
  | -- | All pages were successfully copied.
    BackupDone
  deriving (Eq, Show)

-- | A 'ByteString' containing UTF8-encoded text with no NUL characters.
newtype Utf8 = Utf8 ByteString
  deriving (Eq, Ord, Semigroup, Monoid)

instance Show Utf8 where
  show (Utf8 s) = (show . T.decodeUtf8With lenientDecode) s

-- | @fromString = Utf8 . 'T.encodeUtf8' . 'T.pack'@
instance IsString Utf8 where
  fromString = Utf8 . T.encodeUtf8 . T.pack

packUtf8 :: a -> (Utf8 -> a) -> CString -> IO a
packUtf8 n f cstr
  | cstr == nullPtr = return n
  | otherwise = f . Utf8 <$> BS.packCString cstr

packCStringLen :: CString -> CNumBytes -> IO ByteString
packCStringLen cstr len =
  BS.packCStringLen (cstr, fromIntegral len)

packUtf8Array :: IO a -> (Utf8 -> IO a) -> Int -> Ptr CString -> IO [a]
packUtf8Array onNull onUtf8 count base =
  peekArray count base >>= mapM (join . packUtf8 onNull onUtf8)

-- | Like 'unsafeUseAsCStringLen', but if the string is empty,
-- never pass the callback a null pointer.
unsafeUseAsCStringLenNoNull :: ByteString -> (CString -> CNumBytes -> IO a) -> IO a
unsafeUseAsCStringLenNoNull bs cb
  | BS.null bs = cb (intPtrToPtr 1) 0
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
      cb ptr (fromIntegral len)

wrapNullablePtr :: (Ptr a -> b) -> Ptr a -> Maybe b
wrapNullablePtr f ptr
  | ptr == nullPtr = Nothing
  | otherwise = Just (f ptr)

-- Convert a 'CError' to a 'Either Error', in the common case where
-- SQLITE_OK signals success and anything else signals an error.
--
-- Note that SQLITE_OK == 0.
toResult :: a -> CError -> Either Error a
toResult a (CError 0) = Right a
toResult _ code = Left $ decodeError code

-- Only perform the action if the 'CError' is SQLITE_OK.
toResultM :: (Monad m) => m a -> CError -> m (Either Error a)
toResultM m (CError 0) = Right <$> m
toResultM _ code = return $ Left $ decodeError code

toStepResult :: CError -> Either Error StepResult
toStepResult code =
  case decodeError code of
    ErrorRow -> Right Row
    ErrorDone -> Right Done
    err -> Left err

toBackupStepResult :: CError -> Either Error BackupStepResult
toBackupStepResult code =
  case decodeError code of
    ErrorOK -> Right BackupOK
    ErrorDone -> Right BackupDone
    err -> Left err

-- | The context in which a custom SQL function is executed.
newtype FuncContext = FuncContext (Ptr CContext)
  deriving (Eq, Show)

-- | The arguments of a custom SQL function.
data FuncArgs = FuncArgs CArgCount (Ptr (Ptr CValue))

-- | The type of blob handles used for incremental blob I/O
data Blob = Blob Connection (Ptr CBlob) -- we include the db handle to use in
  deriving (Eq, Show) -- error messages since it cannot
  -- be retrieved any other way

-- | A handle for an online backup process.
data Backup = Backup Connection (Ptr CBackup)
  deriving (Eq, Show)

-- we include the destination db handle to use in error messages since
-- it cannot be retrieved any other way

------------------------------------------------------------------------

-- | <https://www.sqlite.org/c3ref/open.html>
open :: Utf8 -> IO (Either (Error, Utf8) Connection)
open (Utf8 path) =
  BS.useAsCString path \cpath ->
    alloca \dbOutParam -> do
      rc <- c_sqlite3_open cpath dbOutParam
      db <- Connection <$> peek dbOutParam
      -- sqlite3_open returns a sqlite3 even on failure.
      -- That's where we get a more descriptive error message.
      case toResult () rc of
        Left err -> do
          msg <- errmsg db -- This returns "out of memory" if db is null.
          _ <- close db -- This is harmless if db is null.
          return $ Left (err, msg)
        Right () ->
          if db == Connection nullPtr
            then fail "sqlite3_open unexpectedly returned NULL"
            else return $ Right db

-- | <https://www.sqlite.org/c3ref/open.html>
openV2 :: Maybe Utf8 -> [OpenV2Flag] -> OpenV2Mode -> Utf8 -> IO (Either (Error, Utf8) Connection)
openV2 mvfs flags mode (Utf8 path) =
  case mvfs of
    Nothing -> withVFSName nullPtr
    Just (Utf8 vfsName) -> BS.useAsCString vfsName withVFSName
  where
    withVFSName cvfsName =
      BS.useAsCString path \cpath -> do
        alloca \dbOutParam -> do
          rc <- c_sqlite3_open_v2 cpath dbOutParam (encodeOpenV2ModeAndFlags mode flags) cvfsName
          db <- Connection <$> peek dbOutParam
          -- sqlite3_open returns a sqlite3 even on failure.
          -- That's where we get a more descriptive error message.
          case toResult () rc of
            Left err -> do
              msg <- errmsg db -- This returns "out of memory" if db is null.
              _ <- close db -- This is harmless if db is null.
              return $ Left (err, msg)
            Right () ->
              if db == Connection nullPtr
                then fail "sqlite3_open_v2 unexpectedly returned NULL"
                else return $ Right db

-- | <https://www.sqlite.org/c3ref/close.html>
close :: Connection -> IO (Either Error ())
close (Connection db) =
  toResult () <$> c_sqlite3_close db

-- | <https://www.sqlite.org/c3ref/interrupt.html>
--
-- Cause any pending operation on the 'Database' handle to stop at its earliest
-- opportunity.  This simply sets a flag and returns immediately.  It does not
-- wait for the pending operation to finish.
--
-- You'll need to compile with @-threaded@ for this to do any good.
-- Without @-threaded@, FFI calls block the whole RTS, meaning 'interrupt'
-- would never run at the same time as 'step'.
interrupt :: Connection -> IO ()
interrupt (Connection db) =
  c_sqlite3_interrupt db

-- | <https://www.sqlite.org/c3ref/errcode.html>
errcode :: Connection -> IO Error
errcode (Connection db) =
  decodeError <$> c_sqlite3_errcode db

-- | <https://www.sqlite.org/c3ref/errcode.html>
errmsg :: Connection -> IO Utf8
errmsg (Connection db) =
  c_sqlite3_errmsg db >>= packUtf8 mempty id

withErrorMessagePtr :: (Ptr CString -> IO CError) -> IO (Either (Error, Utf8) ())
withErrorMessagePtr action =
  alloca $ \msgPtrOut -> mask $ \restore -> do
    poke msgPtrOut nullPtr
    rc <-
      restore (action msgPtrOut)
        `onException` (peek msgPtrOut >>= c_sqlite3_free)
    case toResult () rc of
      Left err -> do
        msgPtr <- peek msgPtrOut
        if msgPtr == nullPtr
          then return (Left (err, mempty))
          else do
            len <- BSI.c_strlen msgPtr
            fp <- newForeignPtr c_sqlite3_free_p msgPtr
            let bs = BSI.fromForeignPtr (castForeignPtr fp) 0 (fromIntegral len)
            return (Left (err, Utf8 bs))
      Right () -> return (Right ())

-- | <https://www.sqlite.org/c3ref/exec.html>
exec :: Connection -> Utf8 -> IO (Either (Error, Utf8) ())
exec (Connection db) (Utf8 sql) =
  BS.useAsCString sql $ \sql' ->
    withErrorMessagePtr (c_sqlite3_exec db sql' nullFunPtr nullPtr)

-- | Like 'exec', but invoke the callback for each result row.
--
-- If the callback throws an exception, it will be rethrown by
-- 'execWithCallback'.
execWithCallback :: Connection -> Utf8 -> ExecCallback -> IO (Either (Error, Utf8) ())
execWithCallback (Connection db) (Utf8 sql) cb = do
  abortReason <- newIORef Nothing :: IO (IORef (Maybe SomeException))
  cbCache <- newIORef Nothing :: IO (IORef (Maybe ([Maybe Utf8] -> IO ())))
  -- Cache the partial application of column count and name, so if the
  -- caller wants to convert them to something else, it only has to do
  -- the conversions once.

  let getCallback cCount cNames = do
        m <- readIORef cbCache
        case m of
          Nothing -> do
            names <-
              packUtf8Array
                (fail "execWithCallback: NULL column name")
                return
                (fromIntegral cCount)
                cNames
            let !cb' = cb (fromFFI cCount) names
            writeIORef cbCache $ Just cb'
            return cb'
          Just cb' -> return cb'

  let onExceptionAbort io =
        (io >> return 0) `E.catch` \ex -> do
          writeIORef abortReason $ Just ex
          return 1

  let cExecCallback _ctx cCount cValues cNames =
        onExceptionAbort $ do
          cb' <- getCallback cCount cNames
          values <-
            packUtf8Array
              (return Nothing)
              (return . Just)
              (fromIntegral cCount)
              cValues
          cb' values

  BS.useAsCString sql $ \sql' ->
    bracket (mkCExecCallback cExecCallback) freeHaskellFunPtr $ \pExecCallback -> do
      e <- withErrorMessagePtr (c_sqlite3_exec db sql' pExecCallback nullPtr)
      case e of
        Left r@(ErrorAbort, _) -> do
          m <- readIORef abortReason
          case m of
            Nothing -> return (Left r)
            Just ex -> throwIO ex
        r -> return r

type ExecCallback =
  -- | Number of columns, which is the number of items in
  --   the following lists.  This will be the same for
  --   every row.
  ColumnCount ->
  -- | List of column names.  This will be the same
  --   for every row.
  [Utf8] ->
  -- | List of column values, as returned by 'columnText'.
  [Maybe Utf8] ->
  IO ()

-- | <https://www.sqlite.org/c3ref/profile.html>
--
-- Enable/disable tracing of SQL execution.  Tracing can be disabled
-- by setting 'Nothing' as the logger callback.
--
-- Warning: If the logger callback throws an exception, your whole
-- program will crash.  Enable only for debugging!
setTrace :: Connection -> Maybe (Utf8 -> IO ()) -> IO ()
setTrace (Connection db) logger =
  case logger of
    Nothing -> do
      _ <- c_sqlite3_trace db nullFunPtr nullPtr
      return ()
    Just output -> do
      -- NB: this FunPtr never gets freed.  Shouldn't be a big deal,
      -- though, since 'setTrace' is mainly for debugging, and is
      -- typically only called once per application invocation.
      cb <- mkCTraceCallback $ \_ctx cStr -> do
        msg <- packUtf8 mempty id cStr
        output msg
      _ <- c_sqlite3_trace db cb nullPtr
      return ()

-- | <https://www.sqlite.org/c3ref/get_autocommit.html>
--
-- Return 'True' if the connection is in autocommit mode, or 'False' if a
-- transaction started with @BEGIN@ is still active.
--
-- Be warned that some errors roll back the transaction automatically,
-- and that @ROLLBACK@ will throw an error if no transaction is active.
-- Use 'getAutoCommit' to avoid such an error:
--
-- @
--  autocommit <- 'getAutoCommit' conn
--  'Control.Monad.when' (not autocommit) $
--      'Database.Sqlite3.exec' conn \"ROLLBACK\"
-- @
getAutoCommit :: Connection -> IO Bool
getAutoCommit (Connection db) =
  (/= 0) <$> c_sqlite3_get_autocommit db

-- | <https://www.sqlite.org/c3ref/enable_shared_cache.html>
--
-- Enable or disable shared cache for all future connections.
setSharedCacheEnabled :: Bool -> IO (Either Error ())
setSharedCacheEnabled val =
  toResult () <$> c_sqlite3_enable_shared_cache val

-- | <https://www.sqlite.org/c3ref/prepare.html>
--
-- If the query contains no SQL statements, this returns
-- @'Right' 'Nothing'@.
prepare :: Connection -> Utf8 -> IO (Either Error (Maybe PreparedStatement))
prepare (Connection db) (Utf8 sql) =
  BS.useAsCString sql $ \sql' ->
    alloca $ \statement ->
      c_sqlite3_prepare_v2 db sql' (-1) statement nullPtr
        >>= toResultM (wrapNullablePtr PreparedStatement <$> peek statement)

-- | <https://www.sqlite.org/c3ref/db_handle.html>
getStatementConnection :: PreparedStatement -> IO Connection
getStatementConnection (PreparedStatement stmt) = do
  db <- c_sqlite3_db_handle stmt
  if db == nullPtr
    then fail $ "sqlite3_db_handle(" ++ show stmt ++ ") returned NULL"
    else return (Connection db)

-- | <https://www.sqlite.org/c3ref/step.html>
step :: PreparedStatement -> IO (Either Error StepResult)
step (PreparedStatement stmt) =
  toStepResult <$> c_sqlite3_step stmt

-- | <https://www.sqlite.org/c3ref/step.html>
--
-- Faster step for statements that don't callback to Haskell
-- functions (e.g. by using custom SQL functions).
stepNoCB :: PreparedStatement -> IO (Either Error StepResult)
stepNoCB (PreparedStatement stmt) =
  toStepResult <$> c_sqlite3_step_unsafe stmt

-- | <https://www.sqlite.org/c3ref/reset.html>
--
-- Warning:
--
--  * If the most recent 'step' call failed,
--    this will return the corresponding error.
--
--  * This does not reset the bindings on a prepared statement.
--    Use 'clearBindings' to do that.
reset :: PreparedStatement -> IO (Either Error ())
reset (PreparedStatement stmt) =
  toResult () <$> c_sqlite3_reset stmt

-- | <https://www.sqlite.org/c3ref/finalize.html>
--
-- /Warning:/ If the most recent 'step' call failed,
-- this will return the corresponding error.
finalize :: PreparedStatement -> IO (Either Error ())
finalize (PreparedStatement stmt) =
  toResult () <$> c_sqlite3_finalize stmt

-- | <https://www.sqlite.org/c3ref/sql.html>
--
-- Return a copy of the original SQL text used to compile the statement.
statementSql :: PreparedStatement -> IO (Maybe Utf8)
statementSql (PreparedStatement stmt) =
  c_sqlite3_sql stmt >>= packUtf8 Nothing Just

-- | <https://www.sqlite.org/c3ref/clear_bindings.html>
--
-- Set all parameters in the prepared statement to null.
clearBindings :: PreparedStatement -> IO ()
clearBindings (PreparedStatement stmt) = do
  _ <- c_sqlite3_clear_bindings stmt
  return ()

-- | <https://www.sqlite.org/c3ref/bind_parameter_count.html>
--
-- This returns the index of the largest (rightmost) parameter.  Note that this
-- is not necessarily the number of parameters.  If numbered parameters like
-- @?5@ are used, there may be gaps in the list.
--
-- See 'ParamIndex' for more information.
bindParameterCount :: PreparedStatement -> IO ParamIndex
bindParameterCount (PreparedStatement stmt) =
  fromFFI <$> c_sqlite3_bind_parameter_count stmt

-- | <https://www.sqlite.org/c3ref/bind_parameter_name.html>
bindParameterName :: PreparedStatement -> ParamIndex -> IO (Maybe Utf8)
bindParameterName (PreparedStatement stmt) idx =
  c_sqlite3_bind_parameter_name stmt (toFFI idx)
    >>= packUtf8 Nothing Just

-- | <https://www.sqlite.org/c3ref/bind_parameter_index.html>
bindParameterIndex :: PreparedStatement -> Utf8 -> IO (Maybe ParamIndex)
bindParameterIndex (PreparedStatement stmt) (Utf8 name) =
  BS.useAsCString name $ \name' -> do
    idx <- fromFFI <$> c_sqlite3_bind_parameter_index stmt name'
    return $ if idx == 0 then Nothing else Just idx

-- | <https://www.sqlite.org/c3ref/column_count.html>
columnCount :: PreparedStatement -> IO ColumnCount
columnCount (PreparedStatement stmt) =
  fromFFI <$> c_sqlite3_column_count stmt

-- | <https://www.sqlite.org/c3ref/column_name.html>
columnName :: PreparedStatement -> ColumnIndex -> IO (Maybe Utf8)
columnName (PreparedStatement stmt) idx =
  c_sqlite3_column_name stmt (toFFI idx)
    >>= packUtf8 Nothing Just

bindInt64 :: PreparedStatement -> ParamIndex -> Int64 -> IO (Either Error ())
bindInt64 (PreparedStatement stmt) idx value =
  toResult () <$> c_sqlite3_bind_int64 stmt (toFFI idx) value

bindDouble :: PreparedStatement -> ParamIndex -> Double -> IO (Either Error ())
bindDouble (PreparedStatement stmt) idx value =
  toResult () <$> c_sqlite3_bind_double stmt (toFFI idx) value

bindText :: PreparedStatement -> ParamIndex -> Utf8 -> IO (Either Error ())
bindText (PreparedStatement stmt) idx (Utf8 value) =
  unsafeUseAsCStringLenNoNull value $ \ptr len ->
    toResult ()
      <$> c_sqlite3_bind_text stmt (toFFI idx) ptr len c_SQLITE_TRANSIENT

bindBlob :: PreparedStatement -> ParamIndex -> ByteString -> IO (Either Error ())
bindBlob (PreparedStatement stmt) idx value =
  unsafeUseAsCStringLenNoNull value $ \ptr len ->
    toResult ()
      <$> c_sqlite3_bind_blob stmt (toFFI idx) ptr len c_SQLITE_TRANSIENT

bindZeroBlob :: PreparedStatement -> ParamIndex -> Int -> IO (Either Error ())
bindZeroBlob (PreparedStatement stmt) idx len =
  toResult ()
    <$> c_sqlite3_bind_zeroblob stmt (toFFI idx) (fromIntegral len)

bindNull :: PreparedStatement -> ParamIndex -> IO (Either Error ())
bindNull (PreparedStatement stmt) idx =
  toResult () <$> c_sqlite3_bind_null stmt (toFFI idx)

columnType :: PreparedStatement -> ColumnIndex -> IO ColumnType
columnType (PreparedStatement stmt) idx =
  decodeColumnType <$> c_sqlite3_column_type stmt (toFFI idx)

columnInt64 :: PreparedStatement -> ColumnIndex -> IO Int64
columnInt64 (PreparedStatement stmt) idx =
  c_sqlite3_column_int64 stmt (toFFI idx)

columnDouble :: PreparedStatement -> ColumnIndex -> IO Double
columnDouble (PreparedStatement stmt) idx =
  c_sqlite3_column_double stmt (toFFI idx)

columnText :: PreparedStatement -> ColumnIndex -> IO Utf8
columnText (PreparedStatement stmt) idx = do
  ptr <- c_sqlite3_column_text stmt (toFFI idx)
  len <- c_sqlite3_column_bytes stmt (toFFI idx)
  Utf8 <$> packCStringLen ptr len

columnBlob :: PreparedStatement -> ColumnIndex -> IO ByteString
columnBlob (PreparedStatement stmt) idx = do
  ptr <- c_sqlite3_column_blob stmt (toFFI idx)
  len <- c_sqlite3_column_bytes stmt (toFFI idx)
  packCStringLen ptr len

-- | <https://www.sqlite.org/c3ref/last_insert_rowid.html>
lastInsertRowId :: Connection -> IO Int64
lastInsertRowId (Connection db) =
  c_sqlite3_last_insert_rowid db

-- | <https://www.sqlite.org/c3ref/changes.html>
--
-- Return the number of rows that were changed, inserted, or deleted
-- by the most recent @INSERT@, @DELETE@, or @UPDATE@ statement.
changes :: Connection -> IO Int
changes (Connection db) =
  fromIntegral <$> c_sqlite3_changes db

-- | <https://www.sqlite.org/c3ref/total_changes.html>
--
-- Return the total number of row changes caused by @INSERT@, @DELETE@,
-- or @UPDATE@ statements since the 'Connection' was opened.
totalChanges :: Connection -> IO Int
totalChanges (Connection db) =
  fromIntegral <$> c_sqlite3_total_changes db

-- We use CFuncPtrs to store the function pointers used in the implementation
-- of custom SQL functions so that sqlite can deallocate those pointers when
-- the function is deleted or overwritten
data CFuncPtrs = CFuncPtrs (FunPtr CFunc) (FunPtr CFunc) (FunPtr CFuncFinal)

-- Deallocate the function pointers used to implement a custom function
-- This is only called by sqlite so we create one global FunPtr to pass to
-- sqlite
destroyCFuncPtrs :: FunPtr (CFuncDestroy ())
destroyCFuncPtrs = IOU.unsafePerformIO $ mkCFuncDestroy destroy
  where
    destroy p = do
      let p' = castPtrToStablePtr p
      CFuncPtrs p1 p2 p3 <- deRefStablePtr p'
      unless (p1 == nullFunPtr) $ freeHaskellFunPtr p1
      unless (p2 == nullFunPtr) $ freeHaskellFunPtr p2
      unless (p3 == nullFunPtr) $ freeHaskellFunPtr p3
      freeStablePtr p'
{-# NOINLINE destroyCFuncPtrs #-}

-- | <https://sqlite.org/c3ref/create_function.html>
--
-- Create a custom SQL function or redefine the behavior of an existing
-- function.
createFunction ::
  Connection ->
  -- | Name of the function.
  Utf8 ->
  -- | Number of arguments. 'Nothing' means that the
  --   function accepts any number of arguments.
  Maybe ArgCount ->
  -- | Is the function deterministic?
  Bool ->
  -- | Implementation of the function.
  (FuncContext -> FuncArgs -> IO ()) ->
  IO (Either Error ())
createFunction (Connection db) (Utf8 name) nArgs isDet fun = mask_ $ do
  funPtr <- mkCFunc fun'
  u <- newStablePtr $ CFuncPtrs funPtr nullFunPtr nullFunPtr
  BS.useAsCString name $ \namePtr ->
    toResult ()
      <$> c_sqlite3_create_function_v2
        db
        namePtr
        (maybeArgCount nArgs)
        flags
        (castStablePtrToPtr u)
        funPtr
        nullFunPtr
        nullFunPtr
        destroyCFuncPtrs
  where
    flags = if isDet then c_SQLITE_DETERMINISTIC else 0
    fun' ctx nArgs' cvals =
      catchAsResultError ctx $
        fun (FuncContext ctx) (FuncArgs nArgs' cvals)

-- | Like 'createFunction' except that it creates an aggregate function.
createAggregate ::
  Connection ->
  -- | Name of the function.
  Utf8 ->
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
  IO (Either Error ())
createAggregate (Connection db) (Utf8 name) nArgs initSt xStep xFinal = mask_ $ do
  stepPtr <- mkCFunc xStep'
  finalPtr <- mkCFuncFinal xFinal'
  u <- newStablePtr $ CFuncPtrs nullFunPtr stepPtr finalPtr
  BS.useAsCString name $ \namePtr ->
    toResult ()
      <$> c_sqlite3_create_function_v2
        db
        namePtr
        (maybeArgCount nArgs)
        0
        (castStablePtrToPtr u)
        nullFunPtr
        stepPtr
        finalPtr
        destroyCFuncPtrs
  where
    -- we store the aggregate state in the buffer returned by
    -- c_sqlite3_aggregate_context as a StablePtr pointing to an IORef that
    -- contains the actual aggregate state
    xStep' ctx nArgs' cvals =
      catchAsResultError ctx $ do
        aggCtx <- getAggregateContext ctx
        aggStPtr <- peek aggCtx
        aggStRef <-
          if castStablePtrToPtr aggStPtr /= nullPtr
            then deRefStablePtr aggStPtr
            else do
              aggStRef <- newIORef initSt
              aggStPtr' <- newStablePtr aggStRef
              poke aggCtx aggStPtr'
              return aggStRef
        aggSt <- readIORef aggStRef
        aggSt' <- xStep (FuncContext ctx) (FuncArgs nArgs' cvals) aggSt
        writeIORef aggStRef aggSt'
    xFinal' ctx = do
      aggCtx <- getAggregateContext ctx
      aggStPtr <- peek aggCtx
      if castStablePtrToPtr aggStPtr == nullPtr
        then
          catchAsResultError ctx $
            xFinal (FuncContext ctx) initSt
        else do
          catchAsResultError ctx $ do
            aggStRef <- deRefStablePtr aggStPtr
            aggSt <- readIORef aggStRef
            xFinal (FuncContext ctx) aggSt
          freeStablePtr aggStPtr
    getAggregateContext ctx =
      c_sqlite3_aggregate_context ctx stPtrSize
    stPtrSize = fromIntegral $ sizeOf (undefined :: StablePtr ())

-- call c_sqlite3_result_error in the event of an error
catchAsResultError :: Ptr CContext -> IO () -> IO ()
catchAsResultError ctx action = E.catch action $ \exn -> do
  let msg = show (exn :: SomeException)
  withCAStringLen msg $ \(ptr, len) ->
    c_sqlite3_result_error ctx ptr (fromIntegral len)

-- | Delete an SQL function (scalar or aggregate).
deleteFunction :: Connection -> Utf8 -> Maybe ArgCount -> IO (Either Error ())
deleteFunction (Connection db) (Utf8 name) nArgs =
  BS.useAsCString name $ \namePtr ->
    toResult ()
      <$> c_sqlite3_create_function_v2
        db
        namePtr
        (maybeArgCount nArgs)
        0
        nullPtr
        nullFunPtr
        nullFunPtr
        nullFunPtr
        nullFunPtr

maybeArgCount :: Maybe ArgCount -> CArgCount
maybeArgCount (Just n) = toFFI n
maybeArgCount Nothing = -1

funcArgCount :: FuncArgs -> ArgCount
funcArgCount (FuncArgs nArgs _) = fromIntegral nArgs

funcArgType :: FuncArgs -> ArgIndex -> IO ColumnType
funcArgType =
  extractFuncArg NullColumn (fmap decodeColumnType . c_sqlite3_value_type)

funcArgInt64 :: FuncArgs -> ArgIndex -> IO Int64
funcArgInt64 = extractFuncArg 0 c_sqlite3_value_int64

funcArgDouble :: FuncArgs -> ArgIndex -> IO Double
funcArgDouble = extractFuncArg 0 c_sqlite3_value_double

funcArgText :: FuncArgs -> ArgIndex -> IO Utf8
funcArgText = extractFuncArg mempty $ \cval -> do
  ptr <- c_sqlite3_value_text cval
  len <- c_sqlite3_value_bytes cval
  Utf8 <$> packCStringLen ptr len

funcArgBlob :: FuncArgs -> ArgIndex -> IO ByteString
funcArgBlob = extractFuncArg mempty $ \cval -> do
  ptr <- c_sqlite3_value_blob cval
  len <- c_sqlite3_value_bytes cval
  packCStringLen ptr len

-- the c_sqlite3_value_* family of functions don't handle null pointers, so
-- we must use a wrapper to guarantee that a sensible value is returned if
-- we are out of bounds
extractFuncArg :: a -> (Ptr CValue -> IO a) -> FuncArgs -> ArgIndex -> IO a
extractFuncArg defVal extract (FuncArgs nArgs p) idx
  | 0 <= idx && idx < fromIntegral nArgs = do
      cval <- peekElemOff p (fromIntegral idx)
      extract cval
  | otherwise = return defVal

funcResultInt64 :: FuncContext -> Int64 -> IO ()
funcResultInt64 (FuncContext ctx) value =
  c_sqlite3_result_int64 ctx value

funcResultDouble :: FuncContext -> Double -> IO ()
funcResultDouble (FuncContext ctx) value =
  c_sqlite3_result_double ctx value

funcResultText :: FuncContext -> Utf8 -> IO ()
funcResultText (FuncContext ctx) (Utf8 value) =
  unsafeUseAsCStringLenNoNull value $ \ptr len ->
    c_sqlite3_result_text ctx ptr len c_SQLITE_TRANSIENT

funcResultBlob :: FuncContext -> ByteString -> IO ()
funcResultBlob (FuncContext ctx) value =
  unsafeUseAsCStringLenNoNull value $ \ptr len ->
    c_sqlite3_result_blob ctx ptr len c_SQLITE_TRANSIENT

funcResultZeroBlob :: FuncContext -> Int -> IO ()
funcResultZeroBlob (FuncContext ctx) len =
  c_sqlite3_result_zeroblob ctx (fromIntegral len)

funcResultNull :: FuncContext -> IO ()
funcResultNull (FuncContext ctx) =
  c_sqlite3_result_null ctx

-- | <https://www.sqlite.org/c3ref/context_db_handle.html>
getFuncContextConnection :: FuncContext -> IO Connection
getFuncContextConnection (FuncContext ctx) = do
  db <- c_sqlite3_context_db_handle ctx
  if db == nullPtr
    then fail $ "sqlite3_context_db_handle(" ++ show ctx ++ ") returned NULL"
    else return (Connection db)

-- | Deallocate the function pointer to the comparison function used to
-- implement a custom collation
destroyCCompare :: CFuncDestroy ()
destroyCCompare ptr = freeHaskellFunPtr ptr'
  where
    ptr' = castPtrToFunPtr ptr :: FunPtr (CCompare ())

-- | This is called by sqlite so we create one global FunPtr to pass to sqlite
destroyCComparePtr :: FunPtr (CFuncDestroy ())
destroyCComparePtr = IOU.unsafePerformIO $ mkCFuncDestroy destroyCCompare
{-# NOINLINE destroyCComparePtr #-}

-- | <https://www.sqlite.org/c3ref/create_collation.html>
createCollation ::
  Connection ->
  -- | Name of the collation.
  Utf8 ->
  -- | Comparison function.
  (Utf8 -> Utf8 -> Ordering) ->
  IO (Either Error ())
createCollation (Connection db) (Utf8 name) cmp = mask_ $ do
  cmpPtr <- mkCCompare cmp'
  let u = castFunPtrToPtr cmpPtr
  BS.useAsCString name $ \namePtr ->
    toResult () <$> do
      r <-
        c_sqlite3_create_collation_v2
          db
          namePtr
          c_SQLITE_UTF8
          u
          cmpPtr
          destroyCComparePtr
      -- sqlite does not call the destructor for us in case of an
      -- error
      unless (r == CError 0) $
        destroyCCompare $
          castFunPtrToPtr cmpPtr
      return r
  where
    cmp' _ len1 ptr1 len2 ptr2 = handle exnHandler $ do
      s1 <- Utf8 <$> packCStringLen ptr1 len1
      s2 <- Utf8 <$> packCStringLen ptr2 len2
      let c = cmp s1 s2
      evaluate (fromIntegral $ fromEnum c - 1)
    exnHandler (_ :: SomeException) = return (-1)

-- | Delete a collation.
deleteCollation :: Connection -> Utf8 -> IO (Either Error ())
deleteCollation (Connection db) (Utf8 name) =
  BS.useAsCString name $ \namePtr ->
    toResult ()
      <$> c_sqlite3_create_collation_v2
        db
        namePtr
        c_SQLITE_UTF8
        nullPtr
        nullFunPtr
        nullFunPtr

-- | <https://www.sqlite.org/c3ref/enable_load_extension.html>
--
-- Enable or disable extension loading.
setLoadExtensionEnabled :: Connection -> Bool -> IO (Either Error ())
setLoadExtensionEnabled (Connection db) enabled =
  toResult () <$> c_sqlite3_enable_load_extension db enabled

-- | <https://www.sqlite.org/c3ref/blob_open.html>
--
-- Open a blob for incremental I/O.
blobOpen ::
  Connection ->
  -- | The symbolic name of the database (e.g. "main").
  Utf8 ->
  -- | The table name.
  Utf8 ->
  -- | The column name.
  Utf8 ->
  -- | The @ROWID@ of the row.
  Int64 ->
  -- | Open the blob for read-write.
  Bool ->
  IO (Either Error Blob)
blobOpen (Connection db) (Utf8 zDb) (Utf8 zTable) (Utf8 zColumn) rowid rw =
  BS.useAsCString zDb $ \ptrDb ->
    BS.useAsCString zTable $ \ptrTable ->
      BS.useAsCString zColumn $ \ptrColumn ->
        alloca $ \ptrBlob ->
          c_sqlite3_blob_open db ptrDb ptrTable ptrColumn rowid flags ptrBlob
            >>= toResultM (Blob (Connection db) <$> peek ptrBlob)
  where
    flags = if rw then 1 else 0

-- | <https://www.sqlite.org/c3ref/blob_close.html>
blobClose :: Blob -> IO (Either Error ())
blobClose (Blob _ blob) =
  toResult () <$> c_sqlite3_blob_close blob

-- | <https://www.sqlite.org/c3ref/blob_reopen.html>
blobReopen ::
  Blob ->
  -- | The @ROWID@ of the row.
  Int64 ->
  IO (Either Error ())
blobReopen (Blob _ blob) rowid =
  toResult () <$> c_sqlite3_blob_reopen blob rowid

-- | <https://www.sqlite.org/c3ref/blob_bytes.html>
blobBytes :: Blob -> IO Int
blobBytes (Blob _ blob) =
  fromIntegral <$> c_sqlite3_blob_bytes blob

-- | <https://www.sqlite.org/c3ref/blob_read.html>
blobRead ::
  Blob ->
  -- | Number of bytes to read.
  Int ->
  -- | Offset within the blob.
  Int ->
  IO (Either Error ByteString)
blobRead blob len offset = do
  fp <- BSI.mallocByteString len
  fmap (\_ -> BSI.fromForeignPtr fp 0 len)
    <$> withForeignPtr fp (\p -> blobReadBuf blob p len offset)

blobReadBuf :: Blob -> Ptr a -> Int -> Int -> IO (Either Error ())
blobReadBuf (Blob _ blob) buf len offset =
  toResult ()
    <$> c_sqlite3_blob_read blob buf (fromIntegral len) (fromIntegral offset)

-- | <https://www.sqlite.org/c3ref/blob_write.html>
blobWrite ::
  Blob ->
  ByteString ->
  -- | Offset within the blob.
  Int ->
  IO (Either Error ())
blobWrite (Blob _ blob) bs offset =
  BSU.unsafeUseAsCStringLen bs $ \(buf, len) ->
    toResult ()
      <$> c_sqlite3_blob_write blob buf (fromIntegral len) (fromIntegral offset)

-- | <https://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupinit>
backupInit ::
  -- | Destination database handle.
  Connection ->
  -- | Destination database name.
  Utf8 ->
  -- | Source database handle.
  Connection ->
  -- | Source database name.
  Utf8 ->
  IO (Either Error Backup)
backupInit (Connection dstDb) (Utf8 dstName) (Connection srcDb) (Utf8 srcName) =
  BS.useAsCString dstName $ \dstName' ->
    BS.useAsCString srcName $ \srcName' -> do
      r <- c_sqlite3_backup_init dstDb dstName' srcDb srcName'
      if r == nullPtr
        then Left <$> errcode (Connection dstDb)
        else return (Right (Backup (Connection dstDb) r))

-- | <https://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupfinish>
backupFinish :: Backup -> IO (Either Error ())
backupFinish (Backup _ backup) =
  toResult ()
    <$> c_sqlite3_backup_finish backup

-- | <https://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupstep>
backupStep ::
  Backup ->
  -- | Number of pages to copy; if negative, all remaining source pages are copied.
  Int ->
  IO (Either Error BackupStepResult)
backupStep (Backup _ backup) pages =
  toBackupStepResult
    <$> c_sqlite3_backup_step backup (fromIntegral pages)

-- | <https://www.sqlite.org/c3ref/backup_finish.html#sqlite3backupremaining>
backupRemaining :: Backup -> IO Int
backupRemaining (Backup _ backup) =
  fromIntegral <$> c_sqlite3_backup_remaining backup

-- | <https://www.sqlite.org/c3ref/backup_finish.html#sqlite3backuppagecount>
backupPagecount :: Backup -> IO Int
backupPagecount (Backup _ backup) =
  fromIntegral <$> c_sqlite3_backup_pagecount backup
