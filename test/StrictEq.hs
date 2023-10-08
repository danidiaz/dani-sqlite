{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
module StrictEq (
    StrictEq(..),
    (/==),
) where

import Data.ByteString          (ByteString)
import Data.Int                 (Int64)
import Data.Text                (Text)
import Sqlite
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe         (unsafePerformIO)

foreign import capi unsafe "string.h memcmp"
    c_memcmp :: Ptr a -> Ptr a -> CSize -> IO CInt

-- | Variant of Eq that compares Double based on raw representation,
-- rather than applying IEEE 754 coercion rules.
class StrictEq a where
    (===) :: a -> a -> Bool

(/==) :: StrictEq a => a -> a -> Bool
(/==) a b = not (a === b)

instance StrictEq Double where
    a === b = unsafePerformIO $
        alloca $ \aptr ->
        alloca $ \bptr -> do
            poke aptr a
            poke bptr b
            rc <- c_memcmp aptr bptr (fromIntegral $ sizeOf a)
            return (rc == 0)

instance StrictEq Int64 where
    a === b = a == b

instance StrictEq Text where
    a === b = a == b

instance StrictEq ByteString where
    a === b = a == b

instance StrictEq a => StrictEq [a] where
    []      === []      = True
    (x:xs)  === (y:ys)  = x === y && xs === ys
    _       === _       = False

instance StrictEq SqlData where
    SqlInteger  a === SqlInteger b = a === b
    SqlFloat    a === SqlFloat   b = a === b
    SqlText     a === SqlText    b = a === b
    SqlBlob     a === SqlBlob    b = a === b
    SqlNull       === SqlNull      = True
    _             === _            = False
