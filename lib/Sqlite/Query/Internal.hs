{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.Sqlite.Simple.Internal
-- Copyright:   (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- Internal bits.  This interface is less stable and can change at any time.
-- In particular this means that while the rest of the sqlite-simple
-- package endeavors to follow the package versioning policy,  this module
-- does not.  Also, at the moment there are things in here that aren't
-- particularly internal and are exported elsewhere;  these will eventually
-- disappear from this module.
module Sqlite.Query.Internal where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.IORef
import Data.Typeable (Typeable)
import Data.Word
import Sqlite.Query.Ok
import qualified Sqlite as Base

-- -- | Connection to an open database.
-- --
-- -- You can use 'connectionHandle' to gain access to the underlying
-- -- <http://hackage.haskell.org/package/direct-sqlite> connection.
-- -- This may be useful if you need to access some direct-sqlite
-- -- functionality that's not exposed in the sqlite-simple API.  This
-- -- should be a safe thing to do although mixing both APIs is
-- -- discouraged.
-- data Connection = Connection
--   { connectionHandle :: {-# UNPACK #-} !Base.Database,
--     connectionTempNameCounter :: {-# UNPACK #-} !(IORef Word64)
--   }

data ColumnOutOfBounds = ColumnOutOfBounds {errorColumnIndex :: !Int}
  deriving (Eq, Show, Typeable)

instance Exception ColumnOutOfBounds

-- | A Field represents metadata about a particular field
data Field = Field
  { result :: Base.SqlData,
    column :: {-# UNPACK #-} !Int
  }

-- Named type for holding RowParser read-only state.  Just for making
-- it easier to make sense out of types in FromRow.
newtype RowParseRO = RowParseRO {nColumns :: Int}

newtype RowParser a = RP {unRP :: ReaderT RowParseRO (StateT (Int, [Base.SqlData]) Ok) a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

gettypename :: Base.SqlData -> ByteString
gettypename (Base.SqlInteger _) = "INTEGER"
gettypename (Base.SqlFloat _) = "FLOAT"
gettypename (Base.SqlText _) = "TEXT"
gettypename (Base.SqlBlob _) = "BLOB"
gettypename Base.SqlNull = "NULL"
