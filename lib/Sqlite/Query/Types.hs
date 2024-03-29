{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

------------------------------------------------------------------------------

------------------------------------------------------------------------------

-- |
-- Module:      Database.Sqlite.Simple.Types
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2011-2012 Leon P Smith
--              (c) 2012-2013 Janne Hellsten
-- License:     BSD3
-- Maintainer:  Janne Hellsten <jjhellst@gmail.com>
-- Portability: portable
--
-- Top-level module for sqlite-simple.
module Sqlite.Query.Types
  ( Null (..),
    Solo (..),
    Sql (..),
    (:.) (..),
  )
where

import Control.Arrow (first)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Tuple (Solo (..))
import Data.Typeable (Typeable)

-- | A placeholder for the Sql @NULL@ value.
data Null = Null
  deriving (Read, Show, Typeable)

instance Eq Null where
  _ == _ = False
  _ /= _ = False

-- | A query string. This type is intended to make it difficult to
-- construct a Sql query by concatenating string fragments, as that is
-- an extremely common way to accidentally introduce Sql injection
-- vulnerabilities into an application.
--
-- This type is an instance of 'IsString', so the easiest way to
-- construct a query is to enable the @OverloadedStrings@ language
-- extension and then simply write the query in double quotes.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Database.Sqlite.Simple
-- >
-- > q :: Query
-- > q = "select ?"
--
-- The underlying type is a 'Text', and literal Haskell strings that
-- contain Unicode characters will be correctly transformed to UTF-8.
newtype Sql = Sql
  { sqlText :: T.Text
  }
  deriving (Eq, Ord, Typeable)

instance Show Sql where
  show = show . sqlText

instance Read Sql where
  readsPrec i = fmap (first Sql) . readsPrec i

instance IsString Sql where
  fromString = Sql . T.pack

instance Semigroup Sql where
  Sql a <> Sql b = Sql (T.append a b)
  {-# INLINE (<>) #-}

instance Monoid Sql where
  mempty = Sql T.empty
  mappend = (<>)
  {-# INLINE mappend #-}

-- | A composite type to parse your custom data structures without
-- having to define dummy newtype wrappers every time.
--
--
-- > instance FromRow MyData where ...
--
-- > instance FromRow MyData2 where ...
--
--
-- then I can do the following for free:
--
-- @
-- res <- query' c "..."
-- forM res $ \\(MyData{..} :. MyData2{..}) -> do
--   ....
-- @
data h :. t = h :. t deriving (Eq, Ord, Show, Read, Typeable)

infixr 3 :.
