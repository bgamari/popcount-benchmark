{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Bits (popCount)
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Unsafe qualified as ByteString
import Foreign (Ptr)
import Foreign.C (CChar, CSize(..))
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ benchPopcount 16
  , benchPopcount 256
  , benchPopcount 1024
  , benchPopcount (16*1024)
  , benchPopcount (1024*1024)
  ]

benchPopcount :: Int -> Benchmark
benchPopcount len =
    env (evaluate (force bytestring)) $ \benchData -> bgroup ("Benchmark " ++ show len)
    [ bench "ByteString.foldl" $ nf foldlPopcount benchData
    , bcompare "ByteString.foldl" $ bench "FFI popcount (capi)" $ nfAppIO (ffiPopcount c_popcount_capi) benchData
    , bcompare "ByteString.foldl" $ bench "FFI popcount (ccall)" $ nfAppIO (ffiPopcount c_popcount_ccall) benchData
    , bcompare "ByteString.foldl" $ bench "FFI popcount2" $ nfAppIO (ffiPopcount c_popcount2) benchData
    ]
  where
    !bytestring = testPattern len

foldlPopcount :: StrictByteString -> Int
foldlPopcount = ByteString.foldl' (\acc element -> acc + fromIntegral (popCount element)) 0

ffiPopcount :: (Ptr CChar -> CSize -> IO CSize) -> StrictByteString -> IO Int
ffiPopcount f string = ByteString.unsafeUseAsCStringLen string $ \(cString, len) ->
  fromIntegral <$> f cString (fromIntegral len)
{-# INLINE ffiPopcount #-}

foreign import capi "popcount.h popcount"
  c_popcount_capi :: Ptr CChar -> CSize -> IO CSize

foreign import ccall "popcount.h popcount"
  c_popcount_ccall :: Ptr CChar -> CSize -> IO CSize

foreign import ccall "popcount.h popcount2"
  c_popcount2 :: Ptr CChar -> CSize -> IO CSize

testPattern :: Int -> StrictByteString
testPattern len = ByteString.replicate len 0xaa
