{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
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
import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Sqlite qualified as Base
import Sqlite.Query.Ok

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

newtype ColumnOutOfBounds = ColumnOutOfBounds {errorColumnIndex :: Int}
  deriving (Eq, Show)

instance Exception ColumnOutOfBounds

-- | A Field represents metadata about a particular field
data Field = Field
  { result :: Base.SqlData,
    column :: {-# UNPACK #-} !Int
  }

-- Named type for holding RowParser read-only state.  Just for making
-- it easier to make sense out of types in FromRow.
newtype RowParserRO = RowParserRO {nColumns :: Int}

type RowParserState = (Int, [Base.SqlData])

newtype RowParser a = RowParser {runRowParser :: RowParserRO -> RowParserState -> Ok (a, RowParserState)}
  deriving (Functor)

instance Applicative RowParser where
  pure a = RowParser $ \_ s -> pure (a, s)
  RowParser pf <*> RowParser pa = RowParser $ \ro s -> do
    (f, s') <- pf ro s
    (a, s'') <- pa ro s'
    pure (f a, s'')

instance Alternative RowParser where
  empty = RowParser $ \_ _ -> empty
  RowParser p1 <|> RowParser p2 = RowParser $ \ro s -> p1 ro s <|> p2 ro s

instance Monad RowParser where
  RowParser pa >>= f = RowParser $ \ro s -> do
    (a, s') <- pa ro s
    runRowParser (f a) ro s'

instance MonadPlus RowParser where
  mzero = empty
  mplus = (<|>)

-- | Get the read-only RowParseRO environment.
asksRowParserRO :: (RowParserRO -> a) -> RowParser a
asksRowParserRO f = RowParser $ \ro s -> pure (f ro, s)

-- | Get the current RowParserState.
getRowParserState :: RowParser RowParserState
getRowParserState = RowParser $ \_ s -> pure (s, s)

-- | Set the RowParserState.
putRowParserState :: RowParserState -> RowParser ()
putRowParserState s = RowParser $ \_ _ -> pure ((), s)

liftOk :: Ok a -> RowParser a
liftOk oka = RowParser $ \_ s -> do
  a <- oka
  pure (a, s)

gettypename :: Base.SqlData -> ByteString
gettypename (Base.SqlInteger _) = "INTEGER"
gettypename (Base.SqlFloat _) = "FLOAT"
gettypename (Base.SqlText _) = "TEXT"
gettypename (Base.SqlBlob _) = "BLOB"
gettypename Base.SqlNull = "NULL"
