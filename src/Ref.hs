{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls    #-}
module Ref (
  Ref
  , newRef
  , readRef
  , writeRef
  ) where

import Prelude
import FFI

data Ref a

-- | Create a new Fay reference/mutable variable
newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

-- | Read a mutable variable
readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

-- | Write a mutable variable
writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"


