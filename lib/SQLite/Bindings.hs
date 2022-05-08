{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
module SQLite.Bindings (
    CTraceCallback,
    mkCTraceCallback,
) where


import Foreign
import Foreign.C

type CTraceCallback a
     = Ptr a
    -> CString      -- ^ UTF-8 rendering of the SQL statement text as
                    --   the statement first begins executing.
    -> IO ()

-- | A couple important things to know about callbacks from Haskell code:
--
--  * If the callback throws an exception, apparently, the /whole program/ is
--    terminated.
--
--  * Remember to call 'freeHaskellFunPtr' when you are done with the wrapper,
--    to avoid leaking memory.

foreign import capi "wrapper"
    mkCTraceCallback :: CTraceCallback a -> IO (FunPtr (CTraceCallback a))
