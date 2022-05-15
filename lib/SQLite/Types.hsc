{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
module SQLite.Types (
    -- * Objects
    -- | <https://www.sqlite.org/c3ref/objlist.html>
    CDatabase,
    CStatement,
    CValue,
    CContext,
    CBlob,
    CBackup,

    -- * Enumerations
    -- * Open V2 arguments
    COpenV2Flags (..),
    encodeOpenV2ModeAndFlags,
    OpenV2Mode (..),
    OpenV2Flag (..),

    -- ** Error
    CError(..),
    decodeError,
    encodeError,
    Error(..),

    -- ** ColumnType
    CColumnType(..),
    decodeColumnType,
    encodeColumnType,
    ColumnType(..),

    -- * Indices
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,

    -- ** Indices (FFI)
    CParamIndex(..),
    CColumnIndex(..),
    CColumnCount,

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_STATIC,
    c_SQLITE_TRANSIENT,
    c_SQLITE_UTF8,

    -- * Custom functions
    ArgCount(..),
    ArgIndex,
    CArgCount(..),
    c_SQLITE_DETERMINISTIC,

    -- * Conversion to and from FFI types
    FFIType(..),
) where

#include <sqlite3.h>

import Foreign.C.Types
import Foreign.Ptr
import Data.Bits (Bits, (.|.))

-- | <https://www.sqlite.org/c3ref/c_blob.html>
data OpenV2Mode =
      OpenV2ReadOnly
    | OpenV2ReadWrite
    | OpenV2ReadWriteCreate
    deriving (Eq, Show)

encodeOpenV2Mode :: OpenV2Mode -> COpenV2Flags
encodeOpenV2Mode mode = COpenV2Flags $ case mode of
    OpenV2ReadOnly -> #{const SQLITE_OPEN_READONLY}
    OpenV2ReadWrite -> #{const SQLITE_OPEN_READWRITE}
    OpenV2ReadWriteCreate -> #{const SQLITE_OPEN_READWRITE} .|. #{const SQLITE_OPEN_CREATE}

-- | <https://www.sqlite.org/c3ref/c_blob.html>
data OpenV2Flag = 
      OpenV2URI
    | OpenV2Memory
    | OpenV2NoMutex
    | OpenV2FullMutex
    | OpenV2SharedCache
    | OpenV2PrivateCache
    | OpenV2ExtendedResultCode
    | OpenV2NoFollow
    deriving (Eq, Show)

-- | <https://www.sqlite.org/c3ref/c_blob.html>
newtype COpenV2Flags = COpenV2Flags CInt
    deriving stock (Eq, Show)
    deriving newtype Bits

encodeOpenV2Flag :: OpenV2Flag -> COpenV2Flags
encodeOpenV2Flag flag = COpenV2Flags $ case flag of
    OpenV2URI -> #{const SQLITE_OPEN_URI}
    OpenV2Memory -> #{const  SQLITE_OPEN_MEMORY }
    OpenV2NoMutex -> #{const SQLITE_OPEN_NOMUTEX }
    OpenV2FullMutex -> #{const SQLITE_OPEN_FULLMUTEX}
    OpenV2SharedCache -> #{const SQLITE_OPEN_SHAREDCACHE}
    OpenV2PrivateCache -> #{const SQLITE_OPEN_PRIVATECACHE}
    -- SQLITE_OPEN_EXRESCODE was added in 3.37.0   
    OpenV2ExtendedResultCode -> #{const SQLITE_OPEN_EXRESCODE}
    OpenV2NoFollow -> #{const SQLITE_OPEN_NOFOLLOW}

encodeOpenV2ModeAndFlags :: OpenV2Mode -> [OpenV2Flag] -> COpenV2Flags
encodeOpenV2ModeAndFlags (encodeOpenV2Mode -> mode) (map encodeOpenV2Flag -> flags) = 
    mode .|. foldr (.|.) (COpenV2Flags 0) flags

-- Result code documentation copied from <https://www.sqlite.org/c3ref/c_abort.html>

-- | <https://www.sqlite.org/c3ref/c_abort.html>
data Error = ErrorOK                     -- ^ Successful result
           | ErrorError                  -- ^ SQL error or missing database
           | ErrorInternal               -- ^ Internal logic error in SQLite
           | ErrorPermission             -- ^ Access permission denied
           | ErrorAbort                  -- ^ Callback routine requested an abort
           | ErrorBusy                   -- ^ The database file is locked
           | ErrorLocked                 -- ^ A table in the database is locked
           | ErrorNoMemory               -- ^ A @malloc()@ failed
           | ErrorReadOnly               -- ^ Attempt to write a readonly database
           | ErrorInterrupt              -- ^ Operation terminated by @sqlite3_interrupt()@
           | ErrorIO                     -- ^ Some kind of disk I/O error occurred
           | ErrorCorrupt                -- ^ The database disk image is malformed
           | ErrorNotFound               -- ^ Unknown opcode in @sqlite3_file_control()@
           | ErrorFull                   -- ^ Insertion failed because database is full
           | ErrorCan'tOpen              -- ^ Unable to open the database file
           | ErrorProtocol               -- ^ Database lock protocol error
           | ErrorEmpty                  -- ^ Database is empty
           | ErrorSchema                 -- ^ The database schema changed
           | ErrorTooBig                 -- ^ String or BLOB exceeds size limit
           | ErrorConstraint             -- ^ Abort due to constraint violation
           | ErrorMismatch               -- ^ Data type mismatch
           | ErrorMisuse                 -- ^ Library used incorrectly
           | ErrorNoLargeFileSupport     -- ^ Uses OS features not supported on host
           | ErrorAuthorization          -- ^ Authorization denied
           | ErrorFormat                 -- ^ Auxiliary database format error
           | ErrorRange                  -- ^ 2nd parameter to sqlite3_bind out of range
           | ErrorNotADatabase           -- ^ File opened that is not a database file
           | ErrorNotice                 -- ^ Notifications from sqlite3_log()
           | ErrorWarning                -- ^ Warnings from sqlite3_log()
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing
           -- Extended result codes.
           -- https://www.sqlite.org/rescode.html
           | ErrorAbortRollback
           | ErrorAuthorizationUser
           | ErrorBusyRecovery
           | ErrorBusySnapshot
           | ErrorBusyTimeout
           | ErrorCan'tOpenConvPath
           | ErrorCan'tOpenDirtyWAL
           | ErrorCan'tOpenFullPath
           | ErrorCan'tOpenIsDir
           | ErrorCan'tOpenNoTempDir
           | ErrorCan'tOpenSymlink
           | ErrorConstraintCheck
           | ErrorConstraintCommitHook
           | ErrorConstraintDatatype
           | ErrorConstraintForeignKey
           | ErrorConstraintFunction
           | ErrorConstraintNotNull
           | ErrorConstraintPinned
           | ErrorConstraintPrimaryKey
           | ErrorConstraintRowid
           | ErrorConstraintTrigger
           | ErrorConstraintUnique
           | ErrorConstraintVTab
           | ErrorCorruptIndex                
           | ErrorCorruptSequence
           | ErrorCorruptVTab
           | ErrorMissingColSeq
           | ErrorRetry
           | ErrorSnapshot
           | ErrorIOAccess   
           | ErrorIOAuth
           | ErrorIOBeginAtomic
           | ErrorIOBlocked
           | ErrorIOCheckReservedLock
           | ErrorIOCheckClose
           | ErrorIOCheckCommitAtomic
           | ErrorIOConvPath
           | ErrorIOCorruptFS
           | ErrorIOData
           | ErrorIODelete
           | ErrorIODeleteNOENT
           | ErrorIODirClose
           | ErrorIODirFSync
           | ErrorIODirFStat
           | ErrorIOFSync
           | ErrorIOGetTempPath
           | ErrorIOLock
           | ErrorIOMmap
           | ErrorIONoMem
           | ErrorIORDLock
           | ErrorIORead
           | ErrorIORollbackAtomic
           | ErrorIOSeek
           | ErrorIOShmLock
           | ErrorIOShmMap
           | ErrorIOShmOpen
           | ErrorIOShmSize
           | ErrorIOShortRead
           | ErrorIOTruncate
           | ErrorIOUnlock
           | ErrorIOVNode
           | ErrorIOWrite
           | ErrorLockedSharedCache
           | ErrorLockedVTab
           | ErrorNoticeRecoverRollback
           | ErrorNoticeRecoverWAL
           | ErrorOKLoadPermanently
           | ErrorReadOnlyCan'tInit
           | ErrorReadOnlyCan'tLock
           | ErrorReadOnlyDBMoved
           | ErrorReadOnlyDirectory
           | ErrorReadRecovery
           | ErrorReadRollback
           | ErrorWarningAutoindex
             deriving (Eq, Show)

-- | <https://www.sqlite.org/c3ref/c_blob.html>
data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

-- | <https://www.sqlite.org/c3ref/sqlite3.html>
--
-- @CDatabase@ = @sqlite3@
data CDatabase

-- | <https://www.sqlite.org/c3ref/stmt.html>
--
-- @CStatement@ = @sqlite3_stmt@
data CStatement

-- | <https://www.sqlite.org/c3ref/value.html>
--
-- @CValue@ = @sqlite3_value@
data CValue

-- | <https://www.sqlite.org/c3ref/context.html>
--
-- @CContext@ = @sqlite3_context@
data CContext

-- | <https://www.sqlite.org/c3ref/blob.html>
--
-- @CBlob@ = @sqlite3_blob@
data CBlob

-- | <https://www.sqlite.org/c3ref/backup.html>
--
-- @CBackup@ = @sqlite3_backup@
data CBackup

-- | Index of a parameter in a parameterized query.
-- Parameter indices start from 1.
--
-- When a query is 'Database.SQLite3.prepare'd, SQLite allocates an
-- array indexed from 1 to the highest parameter index.  For example:
--
-- >>Right stmt <- prepare conn "SELECT ?1, ?5, ?3, ?"
-- >>bindParameterCount stmt
-- >ParamIndex 6
--
-- This will allocate an array indexed from 1 to 6 (@?@ takes the highest
-- preceding index plus one).  The array is initialized with null values.
-- When you bind a parameter with 'Database.SQLite3.bindSQLData', it assigns a
-- new value to one of these indices.
--
-- See <https://www.sqlite.org/lang_expr.html#varparam> for the syntax of
-- parameter placeholders, and how parameter indices are assigned.
newtype ParamIndex = ParamIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ParamIndex where
    show (ParamIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ParamIndex where
    minBound = ParamIndex (fromIntegral (minBound :: CInt))
    maxBound = ParamIndex (fromIntegral (maxBound :: CInt))

-- | Index of a column in a result set.  Column indices start from 0.
newtype ColumnIndex = ColumnIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ColumnIndex where
    show (ColumnIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ColumnIndex where
    minBound = ColumnIndex (fromIntegral (minBound :: CInt))
    maxBound = ColumnIndex (fromIntegral (maxBound :: CInt))

-- | Number of columns in a result set.
type ColumnCount = ColumnIndex

newtype CParamIndex = CParamIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CParamIndex where
    show (CParamIndex n) = show n

newtype CColumnIndex = CColumnIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CColumnIndex where
    show (CColumnIndex n) = show n

type CColumnCount = CColumnIndex

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | <https://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

-- | Tells SQLite3 that the content pointer is constant and will never change
c_SQLITE_STATIC :: Ptr CDestructor
c_SQLITE_STATIC = intPtrToPtr 0

-- | Tells SQLite3 to make its own private copy of the data
c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)

c_SQLITE_UTF8 :: CInt
c_SQLITE_UTF8 = #{const SQLITE_UTF8}

-- | Number of arguments of a user defined SQL function.
newtype ArgCount = ArgCount Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ArgCount where
    show (ArgCount n) = show n

instance Bounded ArgCount where
    minBound = ArgCount 0
    maxBound = ArgCount (#{const SQLITE_LIMIT_FUNCTION_ARG})

-- | Index of an argument to a custom function. Indices start from 0.
type ArgIndex = ArgCount

newtype CArgCount = CArgCount CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CArgCount where
    show (CArgCount n) = show n

instance Bounded CArgCount where
    minBound = CArgCount (-1)
    maxBound = CArgCount #{const SQLITE_LIMIT_FUNCTION_ARG}

-- | Tells SQLite3 that the defined custom SQL function is deterministic.
c_SQLITE_DETERMINISTIC :: CInt
c_SQLITE_DETERMINISTIC = #{const SQLITE_DETERMINISTIC}

-- | <https://www.sqlite.org/c3ref/c_abort.html>
newtype CError = CError CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.  If the error code is invalid, or
-- perhaps introduced in a newer version of SQLite but this library has not
-- been updated to support it, the result is undefined.
--
-- To be clear, if 'decodeError' fails, it is /undefined behavior/, not an
-- exception you can handle.
--
-- Therefore, do not use direct-sqlite with a different version of SQLite than
-- the one bundled (currently, 3.24.0).  If you do, ensure that 'decodeError'
-- and 'decodeColumnType' are still exhaustive.
decodeError :: CError -> Error
decodeError (CError n) = case n of
    #{const SQLITE_OK}         -> ErrorOK
    #{const SQLITE_ERROR}      -> ErrorError
    #{const SQLITE_INTERNAL}   -> ErrorInternal
    #{const SQLITE_PERM}       -> ErrorPermission
    #{const SQLITE_ABORT}      -> ErrorAbort
    #{const SQLITE_BUSY}       -> ErrorBusy
    #{const SQLITE_LOCKED}     -> ErrorLocked
    #{const SQLITE_NOMEM}      -> ErrorNoMemory
    #{const SQLITE_READONLY}   -> ErrorReadOnly
    #{const SQLITE_INTERRUPT}  -> ErrorInterrupt
    #{const SQLITE_IOERR}      -> ErrorIO
    #{const SQLITE_CORRUPT}    -> ErrorCorrupt
    #{const SQLITE_NOTFOUND}   -> ErrorNotFound
    #{const SQLITE_FULL}       -> ErrorFull
    #{const SQLITE_CANTOPEN}   -> ErrorCan'tOpen
    #{const SQLITE_PROTOCOL}   -> ErrorProtocol
    #{const SQLITE_EMPTY}      -> ErrorEmpty
    #{const SQLITE_SCHEMA}     -> ErrorSchema
    #{const SQLITE_TOOBIG}     -> ErrorTooBig
    #{const SQLITE_CONSTRAINT} -> ErrorConstraint
    #{const SQLITE_MISMATCH}   -> ErrorMismatch
    #{const SQLITE_MISUSE}     -> ErrorMisuse
    #{const SQLITE_NOLFS}      -> ErrorNoLargeFileSupport
    #{const SQLITE_AUTH}       -> ErrorAuthorization
    #{const SQLITE_FORMAT}     -> ErrorFormat
    #{const SQLITE_RANGE}      -> ErrorRange
    #{const SQLITE_NOTADB}     -> ErrorNotADatabase
    #{const SQLITE_NOTICE}     -> ErrorNotice
    #{const SQLITE_WARNING}    -> ErrorWarning
    #{const SQLITE_ROW}        -> ErrorRow
    #{const SQLITE_DONE}       -> ErrorDone
    -- Extended result codes.
    -- https://www.sqlite.org/rescode.html
    #{const SQLITE_ABORT_ROLLBACK} ->     ErrorAbortRollback      
    #{const SQLITE_AUTH_USER} ->     ErrorAuthorizationUser  
    #{const SQLITE_BUSY_RECOVERY} ->     ErrorBusyRecovery       
    #{const SQLITE_BUSY_SNAPSHOT} ->     ErrorBusySnapshot       
    #{const SQLITE_BUSY_TIMEOUT} ->     ErrorBusyTimeout        
    #{const SQLITE_CANTOPEN_CONVPATH} ->     ErrorCan'tOpenConvPath  
    #{const SQLITE_CANTOPEN_DIRTYWAL} ->     ErrorCan'tOpenDirtyWAL  
    #{const SQLITE_CANTOPEN_FULLPATH} ->     ErrorCan'tOpenFullPath  
    #{const SQLITE_CANTOPEN_ISDIR} ->     ErrorCan'tOpenIsDir     
    #{const SQLITE_CANTOPEN_NOTEMPDIR} ->     ErrorCan'tOpenNoTempDir 
    #{const SQLITE_CANTOPEN_SYMLINK} ->     ErrorCan'tOpenSymlink   
    #{const SQLITE_CONSTRAINT_CHECK} ->     ErrorConstraintCheck    
    #{const SQLITE_CONSTRAINT_COMMITHOOK} ->     ErrorConstraintCommitHook 
    #{const SQLITE_CONSTRAINT_DATATYPE} ->     ErrorConstraintDatatype 
    #{const SQLITE_CONSTRAINT_FOREIGNKEY} ->     ErrorConstraintForeignKey 
    #{const SQLITE_CONSTRAINT_FUNCTION} ->     ErrorConstraintFunction 
    #{const SQLITE_CONSTRAINT_NOTNULL} ->     ErrorConstraintNotNull  
    #{const SQLITE_CONSTRAINT_PINNED} ->     ErrorConstraintPinned   
    #{const SQLITE_CONSTRAINT_PRIMARYKEY} ->     ErrorConstraintPrimaryKey 
    #{const SQLITE_CONSTRAINT_ROWID} ->     ErrorConstraintRowid    
    #{const SQLITE_CONSTRAINT_TRIGGER} ->     ErrorConstraintTrigger  
    #{const SQLITE_CONSTRAINT_UNIQUE} ->     ErrorConstraintUnique   
    #{const SQLITE_CONSTRAINT_VTAB} ->     ErrorConstraintVTab     
    #{const SQLITE_CORRUPT_INDEX}        ->     ErrorCorruptIndex       
    #{const SQLITE_CORRUPT_SEQUENCE} ->     ErrorCorruptSequence    
    #{const SQLITE_CORRUPT_VTAB} ->     ErrorCorruptVTab        
    #{const SQLITE_ERROR_MISSING_COLLSEQ} ->     ErrorMissingColSeq      
    #{const SQLITE_ERROR_RETRY} ->     ErrorRetry              
    #{const SQLITE_ERROR_SNAPSHOT} ->     ErrorSnapshot           
    #{const SQLITE_IOERR_ACCESS} ->     ErrorIOAccess           
    #{const SQLITE_IOERR_AUTH} ->     ErrorIOAuth             
    #{const SQLITE_IOERR_BEGIN_ATOMIC} ->     ErrorIOBeginAtomic      
    #{const SQLITE_IOERR_BLOCKED} ->     ErrorIOBlocked          
    #{const SQLITE_IOERR_CHECKRESERVEDLOCK} ->     ErrorIOCheckReservedLock 
    #{const SQLITE_IOERR_CLOSE} ->     ErrorIOCheckClose       
    #{const SQLITE_IOERR_COMMIT_ATOMIC} ->     ErrorIOCheckCommitAtomic 
    #{const SQLITE_IOERR_CONVPATH} ->     ErrorIOConvPath         
    #{const SQLITE_IOERR_CORRUPTFS} ->     ErrorIOCorruptFS        
    #{const SQLITE_IOERR_DATA} ->     ErrorIOData             
    #{const SQLITE_IOERR_DELETE} ->     ErrorIODelete           
    #{const SQLITE_IOERR_DELETE_NOENT} ->     ErrorIODeleteNOENT      
    #{const SQLITE_IOERR_DIR_CLOSE} ->     ErrorIODirClose         
    #{const SQLITE_IOERR_DIR_FSYNC} ->     ErrorIODirFSync         
    #{const SQLITE_IOERR_FSTAT} ->     ErrorIODirFStat         
    #{const SQLITE_IOERR_FSYNC} ->     ErrorIOFSync         
    #{const SQLITE_IOERR_GETTEMPPATH} ->     ErrorIOGetTempPath      
    #{const SQLITE_IOERR_LOCK} ->     ErrorIOLock             
    #{const SQLITE_IOERR_MMAP} ->     ErrorIOMmap             
    #{const SQLITE_IOERR_NOMEM} ->     ErrorIONoMem            
    #{const SQLITE_IOERR_RDLOCK} ->     ErrorIORDLock           
    #{const SQLITE_IOERR_READ} ->     ErrorIORead             
    #{const SQLITE_IOERR_ROLLBACK_ATOMIC} ->     ErrorIORollbackAtomic   
    #{const SQLITE_IOERR_SEEK} ->     ErrorIOSeek             
    #{const SQLITE_IOERR_SHMLOCK} ->     ErrorIOShmLock          
    #{const SQLITE_IOERR_SHMMAP} ->     ErrorIOShmMap           
    #{const SQLITE_IOERR_SHMOPEN} ->     ErrorIOShmOpen          
    #{const SQLITE_IOERR_SHMSIZE} ->     ErrorIOShmSize          
    #{const SQLITE_IOERR_SHORT_READ} ->     ErrorIOShortRead        
    #{const SQLITE_IOERR_TRUNCATE} ->     ErrorIOTruncate         
    #{const SQLITE_IOERR_UNLOCK} ->     ErrorIOUnlock           
    #{const SQLITE_IOERR_VNODE} ->     ErrorIOVNode            
    #{const SQLITE_IOERR_WRITE} ->     ErrorIOWrite            
    #{const SQLITE_LOCKED_SHAREDCACHE} ->     ErrorLockedSharedCache  
    #{const SQLITE_LOCKED_VTAB} ->     ErrorLockedVTab         
    #{const SQLITE_NOTICE_RECOVER_ROLLBACK} ->     ErrorNoticeRecoverRollback 
    #{const SQLITE_NOTICE_RECOVER_WAL} ->     ErrorNoticeRecoverWAL   
    #{const SQLITE_OK_LOAD_PERMANENTLY} ->     ErrorOKLoadPermanently  
    #{const SQLITE_READONLY_CANTINIT} ->     ErrorReadOnlyCan'tInit  
    #{const SQLITE_READONLY_CANTLOCK} ->     ErrorReadOnlyCan'tLock  
    #{const SQLITE_READONLY_DBMOVED} ->     ErrorReadOnlyDBMoved    
    #{const SQLITE_READONLY_DIRECTORY} ->     ErrorReadOnlyDirectory  
    #{const SQLITE_READONLY_RECOVERY} ->     ErrorReadRecovery       
    #{const SQLITE_READONLY_ROLLBACK} ->     ErrorReadRollback       
    #{const SQLITE_WARNING_AUTOINDEX} ->     ErrorWarningAutoindex   
    _                          -> error $ "decodeError " ++ show n

encodeError :: Error -> CError
encodeError err = CError $ case err of
    ErrorOK                 -> #{const SQLITE_OK}
    ErrorError              -> #{const SQLITE_ERROR}
    ErrorInternal           -> #{const SQLITE_INTERNAL}
    ErrorPermission         -> #{const SQLITE_PERM}
    ErrorAbort              -> #{const SQLITE_ABORT}
    ErrorBusy               -> #{const SQLITE_BUSY}
    ErrorLocked             -> #{const SQLITE_LOCKED}
    ErrorNoMemory           -> #{const SQLITE_NOMEM}
    ErrorReadOnly           -> #{const SQLITE_READONLY}
    ErrorInterrupt          -> #{const SQLITE_INTERRUPT}
    ErrorIO                 -> #{const SQLITE_IOERR}
    ErrorCorrupt            -> #{const SQLITE_CORRUPT}
    ErrorNotFound           -> #{const SQLITE_NOTFOUND}
    ErrorFull               -> #{const SQLITE_FULL}
    ErrorCan'tOpen          -> #{const SQLITE_CANTOPEN}
    ErrorProtocol           -> #{const SQLITE_PROTOCOL}
    ErrorEmpty              -> #{const SQLITE_EMPTY}
    ErrorSchema             -> #{const SQLITE_SCHEMA}
    ErrorTooBig             -> #{const SQLITE_TOOBIG}
    ErrorConstraint         -> #{const SQLITE_CONSTRAINT}
    ErrorMismatch           -> #{const SQLITE_MISMATCH}
    ErrorMisuse             -> #{const SQLITE_MISUSE}
    ErrorNoLargeFileSupport -> #{const SQLITE_NOLFS}
    ErrorAuthorization      -> #{const SQLITE_AUTH}
    ErrorFormat             -> #{const SQLITE_FORMAT}
    ErrorRange              -> #{const SQLITE_RANGE}
    ErrorNotADatabase       -> #{const SQLITE_NOTADB}
    ErrorNotice             -> #{const SQLITE_NOTICE}
    ErrorWarning            -> #{const SQLITE_WARNING}
    ErrorRow                -> #{const SQLITE_ROW}
    ErrorDone               -> #{const SQLITE_DONE}
    -- Extended result codes.
    -- https://www.sqlite.org/rescode.html
    ErrorAbortRollback      -> #{const SQLITE_ABORT_ROLLBACK}
    ErrorAuthorizationUser  -> #{const SQLITE_AUTH_USER}
    ErrorBusyRecovery       -> #{const SQLITE_BUSY_RECOVERY}
    ErrorBusySnapshot       -> #{const SQLITE_BUSY_SNAPSHOT}
    ErrorBusyTimeout        -> #{const SQLITE_BUSY_TIMEOUT}
    ErrorCan'tOpenConvPath  -> #{const SQLITE_CANTOPEN_CONVPATH}
    ErrorCan'tOpenDirtyWAL  -> #{const SQLITE_CANTOPEN_DIRTYWAL}
    ErrorCan'tOpenFullPath  -> #{const SQLITE_CANTOPEN_FULLPATH}
    ErrorCan'tOpenIsDir     -> #{const SQLITE_CANTOPEN_ISDIR}
    ErrorCan'tOpenNoTempDir -> #{const SQLITE_CANTOPEN_NOTEMPDIR}
    ErrorCan'tOpenSymlink   -> #{const SQLITE_CANTOPEN_SYMLINK}
    ErrorConstraintCheck    -> #{const SQLITE_CONSTRAINT_CHECK}
    ErrorConstraintCommitHook -> #{const SQLITE_CONSTRAINT_COMMITHOOK}
    ErrorConstraintDatatype -> #{const SQLITE_CONSTRAINT_DATATYPE}
    ErrorConstraintForeignKey -> #{const SQLITE_CONSTRAINT_FOREIGNKEY}
    ErrorConstraintFunction -> #{const SQLITE_CONSTRAINT_FUNCTION}
    ErrorConstraintNotNull  -> #{const SQLITE_CONSTRAINT_NOTNULL}
    ErrorConstraintPinned   -> #{const SQLITE_CONSTRAINT_PINNED}
    ErrorConstraintPrimaryKey -> #{const SQLITE_CONSTRAINT_PRIMARYKEY}
    ErrorConstraintRowid    -> #{const SQLITE_CONSTRAINT_ROWID}
    ErrorConstraintTrigger  -> #{const SQLITE_CONSTRAINT_TRIGGER}
    ErrorConstraintUnique   -> #{const SQLITE_CONSTRAINT_UNIQUE}
    ErrorConstraintVTab     -> #{const SQLITE_CONSTRAINT_VTAB}
    ErrorCorruptIndex       -> #{const SQLITE_CORRUPT_INDEX}       
    ErrorCorruptSequence    -> #{const SQLITE_CORRUPT_SEQUENCE}
    ErrorCorruptVTab        -> #{const SQLITE_CORRUPT_VTAB}
    ErrorMissingColSeq      -> #{const SQLITE_ERROR_MISSING_COLLSEQ}
    ErrorRetry              -> #{const SQLITE_ERROR_RETRY}
    ErrorSnapshot           -> #{const SQLITE_ERROR_SNAPSHOT}
    ErrorIOAccess           -> #{const SQLITE_IOERR_ACCESS}
    ErrorIOAuth             -> #{const SQLITE_IOERR_AUTH}
    ErrorIOBeginAtomic      -> #{const SQLITE_IOERR_BEGIN_ATOMIC}
    ErrorIOBlocked          -> #{const SQLITE_IOERR_BLOCKED}
    ErrorIOCheckReservedLock -> #{const SQLITE_IOERR_CHECKRESERVEDLOCK}
    ErrorIOCheckClose       -> #{const SQLITE_IOERR_CLOSE}
    ErrorIOCheckCommitAtomic -> #{const SQLITE_IOERR_COMMIT_ATOMIC}
    ErrorIOConvPath         -> #{const SQLITE_IOERR_CONVPATH}
    ErrorIOCorruptFS        -> #{const SQLITE_IOERR_CORRUPTFS}
    ErrorIOData             -> #{const SQLITE_IOERR_DATA}
    ErrorIODelete           -> #{const SQLITE_IOERR_DELETE}
    ErrorIODeleteNOENT      -> #{const SQLITE_IOERR_DELETE_NOENT}
    ErrorIODirClose         -> #{const SQLITE_IOERR_DIR_CLOSE}
    ErrorIODirFSync         -> #{const SQLITE_IOERR_DIR_FSYNC}
    ErrorIODirFStat         -> #{const SQLITE_IOERR_FSTAT}
    ErrorIOFSync         -> #{const SQLITE_IOERR_FSYNC}
    ErrorIOGetTempPath      -> #{const SQLITE_IOERR_GETTEMPPATH}
    ErrorIOLock             -> #{const SQLITE_IOERR_LOCK}
    ErrorIOMmap             -> #{const SQLITE_IOERR_MMAP}
    ErrorIONoMem            -> #{const SQLITE_IOERR_NOMEM}
    ErrorIORDLock           -> #{const SQLITE_IOERR_RDLOCK}
    ErrorIORead             -> #{const SQLITE_IOERR_READ}
    ErrorIORollbackAtomic   -> #{const SQLITE_IOERR_ROLLBACK_ATOMIC}
    ErrorIOSeek             -> #{const SQLITE_IOERR_SEEK}
    ErrorIOShmLock          -> #{const SQLITE_IOERR_SHMLOCK}
    ErrorIOShmMap           -> #{const SQLITE_IOERR_SHMMAP}
    ErrorIOShmOpen          -> #{const SQLITE_IOERR_SHMOPEN}
    ErrorIOShmSize          -> #{const SQLITE_IOERR_SHMSIZE}
    ErrorIOShortRead        -> #{const SQLITE_IOERR_SHORT_READ}
    ErrorIOTruncate         -> #{const SQLITE_IOERR_TRUNCATE}
    ErrorIOUnlock           -> #{const SQLITE_IOERR_UNLOCK}
    ErrorIOVNode            -> #{const SQLITE_IOERR_VNODE}
    ErrorIOWrite            -> #{const SQLITE_IOERR_WRITE}
    ErrorLockedSharedCache  -> #{const SQLITE_LOCKED_SHAREDCACHE}
    ErrorLockedVTab         -> #{const SQLITE_LOCKED_VTAB}
    ErrorNoticeRecoverRollback -> #{const SQLITE_NOTICE_RECOVER_ROLLBACK}
    ErrorNoticeRecoverWAL   -> #{const SQLITE_NOTICE_RECOVER_WAL}
    ErrorOKLoadPermanently  -> #{const SQLITE_OK_LOAD_PERMANENTLY}
    ErrorReadOnlyCan'tInit  -> #{const SQLITE_READONLY_CANTINIT}
    ErrorReadOnlyCan'tLock  -> #{const SQLITE_READONLY_CANTLOCK}
    ErrorReadOnlyDBMoved    -> #{const SQLITE_READONLY_DBMOVED}
    ErrorReadOnlyDirectory  -> #{const SQLITE_READONLY_DIRECTORY}
    ErrorReadRecovery       -> #{const SQLITE_READONLY_RECOVERY}
    ErrorReadRollback       -> #{const SQLITE_READONLY_ROLLBACK}
    ErrorWarningAutoindex   -> #{const SQLITE_WARNING_AUTOINDEX}

-- | <https://www.sqlite.org/c3ref/c_blob.html>
newtype CColumnType = CColumnType CInt
    deriving (Eq, Show)

-- | Note that this is a partial function.
-- See 'decodeError' for more information.
decodeColumnType :: CColumnType -> ColumnType
decodeColumnType (CColumnType n) = case n of
    #{const SQLITE_INTEGER} -> IntegerColumn
    #{const SQLITE_FLOAT}   -> FloatColumn
    #{const SQLITE_TEXT}    -> TextColumn
    #{const SQLITE_BLOB}    -> BlobColumn
    #{const SQLITE_NULL}    -> NullColumn
    _                       -> error $ "decodeColumnType " ++ show n

encodeColumnType :: ColumnType -> CColumnType
encodeColumnType t = CColumnType $ case t of
    IntegerColumn -> #{const SQLITE_INTEGER}
    FloatColumn   -> #{const SQLITE_FLOAT}
    TextColumn    -> #{const SQLITE_TEXT}
    BlobColumn    -> #{const SQLITE_BLOB}
    NullColumn    -> #{const SQLITE_NULL}

------------------------------------------------------------------------
-- Conversion to and from FFI types

-- | The "Database.SQLite3" and "Database.SQLite3.Direct" modules use
-- higher-level representations of some types than those used in the
-- FFI signatures ("Database.SQLite3.Bindings").  This typeclass
-- helps with the conversions.
class FFIType public ffi | public -> ffi, ffi -> public where
    toFFI   :: public -> ffi
    fromFFI :: ffi -> public

instance FFIType ParamIndex CParamIndex where
    toFFI (ParamIndex n) = CParamIndex (fromIntegral n)
    fromFFI (CParamIndex n) = ParamIndex (fromIntegral n)

instance FFIType ColumnIndex CColumnIndex where
    toFFI (ColumnIndex n) = CColumnIndex (fromIntegral n)
    fromFFI (CColumnIndex n) = ColumnIndex (fromIntegral n)

instance FFIType Error CError where
    toFFI = encodeError
    fromFFI = decodeError

instance FFIType ColumnType CColumnType where
    toFFI = encodeColumnType
    fromFFI = decodeColumnType

instance FFIType ArgCount CArgCount where
    toFFI (ArgCount n)  = CArgCount (fromIntegral n)
    fromFFI (CArgCount n) = ArgCount (fromIntegral n)
