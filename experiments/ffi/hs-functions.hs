{-# LANGUAGE ForeignFunctionInterface #-}

module FfiExperiment where

import Foreign.C


foreign import ccall "c-functions.h c_add" c_add :: CInt -> CInt -> CInt

add :: Int -> Int -> Int
add x y = fromIntegral $ c_add (fromIntegral x) (fromIntegral y)
