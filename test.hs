{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.ByteString as B
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Numeric.Natural
import Data.Word
import Data.Bits
import Data.List

import Data.Serialize.LEB128

newtype LargeInteger = LargeInteger Integer deriving (Show, Eq, Ord, Num)
newtype LargeNatural = LargeNatural Natural deriving (Show, Eq, Ord, Num)

instance Arbitrary LargeNatural where
    arbitrary = LargeNatural . fromWords <$> arbitrary
      where
        fromWords :: [Word8] -> Natural
        fromWords = foldl' go 0
        go :: Natural -> Word8 -> Natural
        go acc w = (acc `shiftL` 8) + fromIntegral w

instance Arbitrary LargeInteger where
    arbitrary = do
       sign <- arbitrary
       LargeNatural n <- arbitrary
       return $ LargeInteger $ (if sign then negate else id) $ fromIntegral n

-- Only change the default (slight hack)
-- Needed to find corner cases involving non-short encodings
moreTests (QuickCheckTests 100) = QuickCheckTests 100000
moreTests (QuickCheckTests n) = QuickCheckTests n

main = defaultMain $ adjustOption moreTests $
  testGroup "tests"
  [ testGroup "bad cases"
    [ testCase "empty LEB128" $
        fromLEB128 B.empty @=? Left "Failed reading: short encoding\nFrom:\tLEB128\n\n"
    , testCase "empty SLEB128" $
        fromSLEB128 B.empty @=? Left "Failed reading: short encoding\nFrom:\tSLEB128\n\n"
    ]
  , testGroup "roundtrip"
    [ testProperty "LEB128" $ \(LargeNatural i) ->
        Right i === fromLEB128 (toLEB128 i)
    , testProperty "SLEB128" $ \(LargeInteger i) ->
        Right i === fromSLEB128 (toSLEB128 i)
    ]
  , testGroup "unique rep"
    [ testProperty "LEB128" $ \(B.pack -> bs) -> case fromLEB128 bs of
        Left _ -> property ()
        Right i -> bs === toLEB128 i
    , testProperty "SLEB128" $ \(B.pack -> bs) -> case fromSLEB128 bs of
        Left _ -> property ()
        Right i -> bs === toSLEB128 i
    ]
  ]
