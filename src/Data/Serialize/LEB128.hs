{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}

-- |
-- Module      : Data.Serialize.LEB128
-- Description : LEB128 encoding
-- License     : MIT
-- Maintainer  : Joachim Breitner
--
-- | This module implements encoding and decoding of 'Natural' and 'Integer'
-- values according to LEB128 and SLEB128. See
-- https://en.wikipedia.org/wiki/LEB128 for a specification.
--
-- The module provides conversion to and from strict bytestrings.
--
-- Additionally, to integrate these into your own parsers and serializers, you
-- can use the interfaces based on 'B.Builder' as well as @cereal@'s 'G.Get'
-- and 'P.Put' monad.
--
-- The decoders will fail if the input is not in canonical representation,
-- i.e. longer than necessary.
--
-- This code is inspired by Andreas Klebinger's LEB128 implementation in GHC.
module Data.Serialize.LEB128
    (
    -- * The class of encodable and decodable types
      LEB128
    , SLEB128
    -- * Bytestring-based interface
    , toLEB128
    , fromLEB128
    , toSLEB128
    , fromSLEB128
    -- * Builder interface
    , buildLEB128
    , buildSLEB128
    -- * Cereal interface
    , getLEB128
    , getSLEB128
    , putLEB128
    , putSLEB128
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.Serialize.Get as G
import qualified Data.Serialize.Put as P
import Numeric.Natural
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word
import Data.Int
import Data.Monoid ((<>))
import Prelude hiding ((<>))

class (Bits a, Num a, Integral a) => LEB128 a
instance LEB128 Natural
instance LEB128 Word
instance LEB128 Word8
instance LEB128 Word16
instance LEB128 Word32
instance LEB128 Word64

class (Bits a, Num a, Integral a) => SLEB128 a
instance SLEB128 Integer
instance SLEB128 Int
instance SLEB128 Int8
instance SLEB128 Int16
instance SLEB128 Int32
instance SLEB128 Int64

-- | LEB128-encodes a natural number to a strict bytestring
toLEB128 :: LEB128 a => a -> BS.ByteString
toLEB128 = BSL.toStrict . B.toLazyByteStringWith (B.safeStrategy 32 32) BSL.empty . buildLEB128

-- | SLEB128-encodes an integer to a strict bytestring
toSLEB128 :: SLEB128 a => a -> BS.ByteString
toSLEB128 = BSL.toStrict . B.toLazyByteStringWith (B.safeStrategy 32 32) BSL.empty . buildSLEB128

-- | LEB128-encodes a natural number via a builder
buildLEB128 :: LEB128 a => a -> B.Builder
buildLEB128 = go
  where
    go i
      | i <= 127
      = B.word8 (fromIntegral i :: Word8)
      | otherwise =
        -- bit 7 (8th bit) indicates more to come.
        B.word8 (setBit (fromIntegral i) 7) <> go (i `unsafeShiftR` 7)

-- | SLEB128-encodes an integer via a builder
buildSLEB128 :: SLEB128 a => a -> B.Builder
buildSLEB128 = go
  where
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and and last value can
                -- be discriminated from a negative number.
                (val' == 0 && not signBit) ||
                -- Signed value,
                (val' == -1 && signBit)
        let !byte' = if done then byte else setBit byte 7
        B.word8 byte' <> if done then mempty else go val'

-- | LEB128-encodes a natural number in @cereal@'s 'P.Put' monad
putLEB128 :: LEB128 a => P.Putter a
putLEB128 = P.putBuilder . buildLEB128

-- | SLEB128-encodes an integer in @cereal@'s 'P.Put' monad
putSLEB128 :: SLEB128 a => P.Putter a
putSLEB128 = P.putBuilder . buildSLEB128

-- | LEB128-decodes a natural number from a strict bytestring
fromLEB128 :: LEB128 a => BS.ByteString -> Either String a
fromLEB128 = runComplete getLEB128

-- | SLEB128-decodes an integer from a strict bytestring
fromSLEB128 :: SLEB128 a => BS.ByteString -> Either String a
fromSLEB128 = runComplete getSLEB128

runComplete :: G.Get a -> BS.ByteString -> Either String a
runComplete p bs = do
    (x,r) <- G.runGetState p bs 0
    unless (BS.null r) $ Left "extra bytes in input"
    return x

-- | LEB128-decodes a natural number via @cereal@
getLEB128 :: forall a. LEB128 a => G.Get a
getLEB128 = G.label "LEB128" $ go 0 0
  where
    go :: Int -> a -> G.Get a
    go !shift !w = do
      byte <- G.getWord8 <|> fail "short encoding"
      let !byteVal = fromIntegral (clearBit byte 7)
      let !hasMore = testBit byte 7
      let !val = w .|. (byteVal `unsafeShiftL` shift)
      let !shift' = shift+7
      if hasMore
        then go shift' val
        else do
          when (byte == 0x00 && shift > 0)
            $ fail "overlong encoding"
          return $! val

-- | SLEB128-decodes an integer via @cereal@
getSLEB128 :: forall a. SLEB128 a => G.Get a
getSLEB128 = G.label "SLEB128" $ go 0 0 0
  where
    go :: Word8 -> Int -> a -> G.Get a
    go !prev !shift !w = do
        byte <- G.getWord8 <|> fail "short encoding"
        let !byteVal = fromIntegral (clearBit byte 7)
        let !hasMore = testBit byte 7
        let !val = w .|. (byteVal `unsafeShiftL` shift)
        let !shift' = shift+7
        if hasMore
            then go byte shift' val
            else if signed byte
              then do
                when (byte == 0x7f && signed prev && shift > 0)
                  $ fail "overlong encoding"
                return $! val - bit shift'
              else do
                when (byte == 0x00 && not (signed prev) && shift > 0)
                  $ fail "overlong encoding"
                return $! val

    signed b = testBit b 6
