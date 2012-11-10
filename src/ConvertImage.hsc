{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module ConvertImage (
  loadPicture
, CBinaryPattern (..)
) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

#include "convertImage.h"

-- From: http://www.haskell.org/haskellwiki/FFI_cook_book
#let alignment t = "%lu", (unsigned long) offsetof(struct { char x__; t (y__); }, y__)

data CBinaryPattern = CBinaryPattern {
    size :: Word32
  , pattern :: [Word32]
} deriving (Eq, Show)


foreign import ccall "convertImage.h load_picture" load_picture :: CString -> CInt -> CInt -> Ptr CBinaryPattern


instance Storable CBinaryPattern where
  alignment _ = #{alignment binary_pattern_t}
  sizeOf _ = #{size binary_pattern_t}
  peek ptr = do s <- #{peek binary_pattern_t, size} ptr
                pattern_ptr <- #{peek binary_pattern_t, pattern} ptr
                pattern_01s <- peekArray (fromIntegral s) pattern_ptr
                return $ CBinaryPattern s pattern_01s
  poke _ptr (CBinaryPattern _s _p) = error "Storable CBinaryPattern: poke not implemented"



loadPicture :: String -> Int -> Int-> IO CBinaryPattern
loadPicture path w h = do
  cpath <- newCString path
  -- I need to create CInt from Int in haskell
  --cw    <- newCInt w
  --ch    <- newCInt h
  peek (load_picture cpath 3 3)
