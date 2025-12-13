{-# LANGUAGE BlockArguments #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.Sqlite.Simple.ToField
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- The 'ToField' typeclass, for rendering a parameter to an Sqlite
-- value to be bound as a Sql query parameter.
module Sqlite.Query.ToField (ToField (..)) where

import Data.ByteString qualified as SB
import Data.ByteString.Lazy qualified as LB
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float
import Sqlite as Base
import Sqlite.Query.Types (Null)

-- | A type that may be used as a single parameter to a Sql query.
class ToField a where
  toField :: a -> SqlData
  -- ^ Prepare a value for substitution into a query string.

instance ToField SqlData where
  toField a = a
  {-# INLINE toField #-}

instance (ToField a) => ToField (Maybe a) where
  toField Nothing = Base.SqlNull
  toField (Just a) = toField a
  {-# INLINE toField #-}

instance ToField Null where
  toField _ = Base.SqlNull
  {-# INLINE toField #-}

instance ToField Bool where
  toField False = SqlInteger 0
  toField True = SqlInteger 1
  {-# INLINE toField #-}

instance ToField Int8 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Int16 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Int32 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Int where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Int64 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Integer where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Word8 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Word16 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Word32 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Word where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Word64 where
  toField = SqlInteger . fromIntegral
  {-# INLINE toField #-}

instance ToField Float where
  toField = SqlFloat . float2Double
  {-# INLINE toField #-}

instance ToField Double where
  toField = SqlFloat
  {-# INLINE toField #-}

instance ToField SB.ByteString where
  toField = SqlBlob
  {-# INLINE toField #-}

instance ToField LB.ByteString where
  toField = toField . SB.concat . LB.toChunks
  {-# INLINE toField #-}

instance ToField T.Text where
  toField = SqlText
  {-# INLINE toField #-}

instance ToField [Char] where
  toField = SqlText . T.pack
  {-# INLINE toField #-}

instance ToField LT.Text where
  toField = toField . LT.toStrict
  {-# INLINE toField #-}

-- instance ToField UTCTime where
--     toField = SqlText . T.decodeUtf8 . toByteString . utcTimeToBuilder
--     {-# INLINE toField #-}
--
-- instance ToField Day where
--     toField = SqlText . T.decodeUtf8 . toByteString . dayToBuilder
--     {-# INLINE toField #-}

-- TODO enable these
-- instance ToField ZonedTime where
--    toField = SqlText . zonedTimeToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField LocalTime where
--    toField = SqlText . localTimeToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField Day where
--    toField = SqlText . dayToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField TimeOfDay where
--    toField = SqlText . timeOfDayToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField UTCTimestamp where
--    toField = SqlText . utcTimestampToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField ZonedTimestamp where
--    toField = SqlText . zonedTimestampToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField LocalTimestamp where
--    toField = SqlText . localTimestampToBuilder
--    {-# INLINE toField #-}
--
-- instance ToField Date where
--    toField = SqlText . dateToBuilder
--    {-# INLINE toField #-}
