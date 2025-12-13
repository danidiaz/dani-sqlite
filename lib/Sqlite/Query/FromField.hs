{-# LANGUAGE BlockArguments #-}

------------------------------------------------------------------------------
-- |
-- Module:      Database.Sqlite.Simple.FromField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'FromField' typeclass, for converting a single value in a row
-- returned by a Sql query into a more useful Haskell representation.
--
-- A Haskell numeric type is considered to be compatible with all
-- Sqlite numeric types that are less accurate than it. For instance,
-- the Haskell 'Double' type is compatible with the Sqlite's 32-bit
-- @Int@ type because it can represent a @Int@ exactly. On the other hand,
-- since a 'Double' might lose precision if representing a 64-bit @BigInt@,
-- the two are /not/ considered compatible.
--
------------------------------------------------------------------------------

module Sqlite.Query.FromField
    (
      FromField(..)
    , FieldParser
    , ResultError(..)
    , Field
    , fieldData
    , returnError
    ) where

import           Control.Exception (SomeException(..), Exception)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word8, Word16, Word32, Word64)
import           GHC.Float (double2Float)

import           Sqlite as Base
import           Sqlite.Query.Types
import           Sqlite.Query.Internal
import           Sqlite.Query.Ok

-- | Exception thrown if conversion from a Sql value to a Haskell
-- value fails.
data ResultError = Incompatible { errSqlType :: String
                                , errHaskellType :: String
                                , errMessage :: String }
                 -- ^ The Sql and Haskell types are not compatible.
                 | UnexpectedNull { errSqlType :: String
                                  , errHaskellType :: String
                                  , errMessage :: String }
                 -- ^ A Sql @NULL@ was encountered when the Haskell
                 -- type did not permit it.
                 | ConversionFailed { errSqlType :: String
                                    , errHaskellType :: String
                                    , errMessage :: String }
                 -- ^ The Sql value could not be parsed, or could not
                 -- be represented as a valid Haskell value, or an
                 -- unexpected low-level error occurred (e.g. mismatch
                 -- between metadata and actual data in a row).
                   deriving (Eq, Show)

instance Exception ResultError

left :: Exception a => a -> Ok b
left = Errors . (:[]) . SomeException

type FieldParser a = Field -> Ok a

-- | A type that may be converted from a Sql type.
class FromField a where
    fromField :: FieldParser a
    -- ^ Convert a Sql value to a Haskell value.
    --
    -- Returns a list of exceptions if the conversion fails.  In the case of
    -- library instances,  this will usually be a single 'ResultError',  but
    -- may be a 'UnicodeException'.
    --
    -- Implementations of 'fromField' should not retain any references to
    -- the 'Field' nor the 'ByteString' arguments after the result has
    -- been evaluated to WHNF.  Such a reference causes the entire
    -- @LibPQ.'PQ.Result'@ to be retained.
    --
    -- For example,  the instance for 'ByteString' uses 'B.copy' to avoid
    -- such a reference,  and that using bytestring functions such as 'B.drop'
    -- and 'B.takeWhile' alone will also trigger this memory leak.

instance (FromField a) => FromField (Maybe a) where
    fromField (Field SqlNull _) = pure Nothing
    fromField f                 = Just <$> fromField f

instance FromField Null where
    fromField (Field SqlNull _) = pure Null
    fromField f                 = returnError ConversionFailed f "data is not null"

takeInt :: (Num a, Typeable a) => Field -> Ok a
takeInt (Field (SqlInteger i) _) = Ok . fromIntegral $ i
takeInt f                        = returnError ConversionFailed f "need an int"

instance FromField Int8 where
    fromField = takeInt

instance FromField Int16 where
    fromField = takeInt

instance FromField Int32 where
    fromField = takeInt

instance FromField Int where
    fromField = takeInt

instance FromField Int64 where
    fromField = takeInt

instance FromField Integer where
    fromField = takeInt

instance FromField Word8 where
    fromField = takeInt

instance FromField Word16 where
    fromField = takeInt

instance FromField Word32 where
    fromField = takeInt

instance FromField Word64 where
    fromField = takeInt

instance FromField Word where
    fromField = takeInt

instance FromField Double where
    fromField (Field (SqlFloat flt) _) = Ok flt
    fromField f                        = returnError ConversionFailed f "expecting an SqlFloat column type"

instance FromField Float where
    fromField (Field (SqlFloat flt) _) = Ok . double2Float $ flt
    fromField f                        = returnError ConversionFailed f "expecting an SqlFloat column type"

instance FromField Bool where
    fromField f@(Field (SqlInteger b) _)
      | (b == 0) || (b == 1) = Ok (b /= 0)
      | otherwise = returnError ConversionFailed f ("bool must be 0 or 1, got " ++ show b)

    fromField f = returnError ConversionFailed f "expecting an SqlInteger column type"

instance FromField T.Text where
  fromField f@(Field sqlData _) = case sqlData of
    SqlText v -> Ok v
    SqlInteger v -> Ok $ T.pack $ show v
    SqlFloat v -> Ok $ T.pack $ show v
    _ -> returnError ConversionFailed f "need convertible to text"

instance FromField LT.Text where
  fromField f = LT.fromStrict <$> fromField f

instance FromField [Char] where
  fromField f = T.unpack <$> fromField f

instance FromField ByteString where
  fromField (Field (SqlBlob blb) _) = Ok blb
  fromField f                       = returnError ConversionFailed f "expecting SqlBlob column type"

instance FromField LB.ByteString where
  fromField (Field (SqlBlob blb) _) = Ok . LB.fromChunks $ [blb]
  fromField f                       = returnError ConversionFailed f "expecting SqlBlob column type"

instance FromField SqlData where
  fromField :: FieldParser SqlData
  fromField f = Ok (fieldData f)

fieldTypename :: Field -> String
fieldTypename = B.unpack . gettypename . result

-- | Return the actual Sql data for a database field.  This allows
-- user-defined 'FromField' instances to access the Sql data
-- associated with a field being parsed.
fieldData :: Field -> SqlData
fieldData = result

-- | Given one of the constructors from 'ResultError',  the field,
--   and an 'errMessage',  this fills in the other fields in the
--   exception value and returns it in a 'Left . SomeException'
--   constructor.
returnError :: forall a err . (Typeable a, Exception err)
            => (String -> String -> String -> err)
            -> Field -> String -> Ok a
returnError mkErr f = left . mkErr (fieldTypename f)
                                   (show (typeOf (undefined :: a)))
