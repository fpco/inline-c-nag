{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.C.Inline.Nag
  ( module Language.C.Inline
    -- * Context
  , nagCtx
    -- * Utilities
  , withNagError
  , initNagError
  , checkNagError
    -- * Types
  , Complex(..)
  , NagError
  , Nag_Boolean
  , Nag_Integer
  , Nag_Comm
  , Nag_User
  ) where

import           Prelude hiding (exp)

import           Data.Functor ((<$>))
import           Foreign.C.String (peekCString)
import           Foreign.C.Types
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)

import           Language.C.Inline.Nag.Internal
import           Language.C.Inline

context nagCtx

include "<nag.h>"

-- | Allocates a @'Ptr' 'NagError'@ which can be used with many of the
-- NAG functions.  After the action has run, it inspects its contents
-- and reports an error if present.
withNagError :: (Ptr NagError -> IO a) -> IO (Either String a)
withNagError f = initNagError $ \ptr -> checkNagError ptr $ f ptr

-- | Like 'withNagError', but with no error check.
initNagError :: (Ptr NagError -> IO a) -> IO a
initNagError f = alloca $ \ptr -> do
  [exp| void{ INIT_FAIL(*$(NagError *ptr)) } |]
  f ptr

-- | Runs the provided actoin, and checks if the 'NagError' reports
-- an error.
checkNagError :: Ptr NagError -> IO a -> IO (Either String a)
checkNagError ptr f = do
  x <- f
  errCode <- [exp| int { $(NagError *ptr)->code } |]
  if errCode /= _NE_NOERROR
    then do
      ch <- [exp| char * { $(NagError *ptr)->message } |]
      Left <$> peekCString ch
    else return $ Right x
