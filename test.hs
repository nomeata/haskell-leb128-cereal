{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.ByteString as B
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Numeric.Natural
import Data.Word
import Data.Bits
import Data.List

import Codec.LEB128

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

main = defaultMain $ testGroup "tests"
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
  ]
