{-# LANGUAGE ForeignFunctionInterface #-}
module Foo where
 
import Foreign.C.Types
 
foreign export ccall hs_fact :: CInt -> CInt
hs_fact :: CInt -> CInt
hs_fact n = product [1..n]
