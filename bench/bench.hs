{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Hashable
import Test.Tasty.Bench
import Data.Int
import Data.Word
import Data.Scientific
import qualified Data.Binary as Binary
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder.Scientific

main :: IO ()
main = defaultMain
       [ bgroup "realToFrac"
         [ bgroup "Scientific->Double"
           [ sToD "dangerouslyBig"   dangerouslyBig
           , sToD "dangerouslySmall" dangerouslySmall
           , sToD "pos"              pos
           , sToD "neg"              neg
           , sToD "int"              int
           , sToD "negInt"           negInt
           ]
         , bgroup "Double->Scientific"
           [ dToS "pos"    pos
           , dToS "neg"    neg
           , dToS "int"    int
           , dToS "negInt" negInt
           ]
         ]
       , bgroup "floor"
         [ bench "floor"        (nf (floor :: Scientific -> Integer) $! pos)
         , bench "floorDefault" (nf floorDefault                     $! pos)
         ]
       , bgroup "ceiling"
         [ bench "ceiling"        (nf (ceiling :: Scientific -> Integer) $! pos)
         , bench "ceilingDefault" (nf ceilingDefault                     $! pos)
         ]
       , bgroup "truncate"
         [ bench "truncate"        (nf (truncate :: Scientific -> Integer) $! pos)
         , bench "truncateDefault" (nf truncateDefault                     $! pos)
         ]

       , bgroup "round"
         [ bench "round"        (nf (round :: Scientific -> Integer) $! pos)
         , bench "roundDefault" (nf roundDefault                     $! pos)
         ]

       , bgroup "toDecimalDigits"
         [ bench "big" (nf toDecimalDigits $! big)
         ]

       , bgroup "fromFloatDigits"
         [ bench "pos"    $ nf (fromFloatDigits :: Double -> Scientific) pos
         , bench "neg"    $ nf (fromFloatDigits :: Double -> Scientific) neg
         , bench "int"    $ nf (fromFloatDigits :: Double -> Scientific) int
         , bench "negInt" $ nf (fromFloatDigits :: Double -> Scientific) negInt
         ]

       , bgroup "toBoundedInteger"
         [ bgroup "0"              $ benchToBoundedInteger 0
         , bgroup "dangerouslyBig" $ benchToBoundedInteger dangerouslyBig
         , bgroup "64"             $ benchToBoundedInteger 64
         ]

       , bgroup "Eq"
         [ bench name $ nf (uncurry (==)) t | (name, t) <- comparisonPairs ]

       , bgroup "Ord"
         [ bench name $ nf (uncurry compare) t | (name, t) <- comparisonPairs ]

       , bench "hash" $ nf (hash @Scientific) $! pos

       , bgroup "normalize"
         [ bench "normalized"     $ nf normalize $! pos
         , bench "trailing zeros" $ nf normalize $! scientific 1234512345000 (-8)
         ]

       , bgroup "isInteger"
         [ bench "integer"  $ nf isInteger $! scientific 1234500 (-2)
         , bench "fraction" $ nf isInteger $! pos
         ]

       , bench "multiplication" $ nf (uncurry (*)) (pos @Scientific, neg)

       , bgroup "show"
         [ bench "pos" $ nf show $! pos @Scientific
         , bench "big" $ nf show $! big
         , bench "exponent" $ nf show $! scientific 12345 100
         ]

       , bgroup "scientificBuilder"
         [ bench "pos" $ nf (B.toLazyByteString . scientificBuilder) $! pos
         , bench "big" $ nf (B.toLazyByteString . scientificBuilder) $! big
         ]

       , bench "Binary.decode" $ nf (Binary.decode @Scientific) $! Binary.encode (pos @Scientific)

       , bgroup "read"
         [ benchRead "1.5"
         , benchRead "123456789.123456789"
         , benchRead "12345678900000000000.12345678900000000000000000"
         , benchRead "12345678900000000000.12345678900000000000000000e1234"
         ]

       , bgroup "division"
         [ bench (show n ++ " / " ++ show d) $ nf (uncurry (/)) t
         | t@(n, d) <-
           [ (0.4     , 20.0)
           , (0.4e-100, 0.2e50)
           ] :: [(Scientific, Scientific)]
         ]

       ]
    where
      pos :: Fractional a => a
      pos = 12345.12345

      neg :: Fractional a => a
      neg = -pos

      int :: Fractional a => a
      int = 12345

      negInt :: Fractional a => a
      negInt = -int

      big :: Scientific
      big = read $ "0." ++ concat (replicate 20 "0123456789")

      dangerouslyBig :: Scientific
      dangerouslyBig = read "1e500"

      dangerouslySmall :: Scientific
      dangerouslySmall = read "1e-500"

      comparisonPairs :: [(String, (Scientific, Scientific))]
      comparisonPairs =
        [ ("same exponent",            (scientific 1234512345 (-5), scientific 1234512346  (-5)))
        , ("other exponent, equal",    (scientific 1234512345 (-5), scientific 12345123450 (-6)))
        , ("other exponent, unequal",  (scientific 1234512345 (-5), scientific 12345123451 (-6)))
        , ("other exponent, other sign", (scientific 1234512345 (-5), scientific (-1234512345) (-6)))
        ]

      benchRead :: String -> Benchmark
      benchRead s = bench s $ nf (read @Scientific) s

realToFracStoD :: Scientific -> Double
realToFracStoD = fromRational . toRational
{-# INLINE realToFracStoD #-}

realToFracDtoS :: Double -> Scientific
realToFracDtoS = fromRational . toRational
{-# INLINE realToFracDtoS #-}

sToD :: String -> Scientific -> Benchmark
sToD name f = bgroup name
              [ bench "toRealFloat"  . nf (realToFrac     :: Scientific -> Double) $! f
              , bench "via Rational" . nf (realToFracStoD :: Scientific -> Double) $! f
              ]

dToS :: String -> Double -> Benchmark
dToS name f = bgroup name
              [ bench "fromRealFloat"  . nf (realToFrac     :: Double -> Scientific) $! f
              , bench "via Rational"   . nf (realToFracDtoS :: Double -> Scientific) $! f
              ]

floorDefault :: Scientific -> Integer
floorDefault x = if r < 0 then n - 1 else n
                 where (n,r) = properFraction x
{-# INLINE floorDefault #-}

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > 0 then n + 1 else n
                   where (n,r) = properFraction x
{-# INLINE ceilingDefault #-}

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = properFraction x
{-# INLINE truncateDefault #-}

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = properFraction x
                     m     = if r < 0 then n - 1 else n + 1
                 in case signum (abs r - 0.5) of
                      -1 -> n
                      0  -> if even n then n else m
                      1  -> m
                      _  -> error "round default defn: Bad value"
{-# INLINE roundDefault #-}

benchToBoundedInteger :: Scientific -> [Benchmark]
benchToBoundedInteger s =
    [ bench "Int"    $ nf (toBoundedInteger :: Scientific -> Maybe Int)    s
    , bench "Int8"   $ nf (toBoundedInteger :: Scientific -> Maybe Int8)   s
    , bench "Int16"  $ nf (toBoundedInteger :: Scientific -> Maybe Int16)  s
    , bench "Int32"  $ nf (toBoundedInteger :: Scientific -> Maybe Int32)  s
    , bench "Int64"  $ nf (toBoundedInteger :: Scientific -> Maybe Int64)  s
    , bench "Word"   $ nf (toBoundedInteger :: Scientific -> Maybe Word)   s
    , bench "Word8"  $ nf (toBoundedInteger :: Scientific -> Maybe Word8)  s
    , bench "Word16" $ nf (toBoundedInteger :: Scientific -> Maybe Word16) s
    , bench "Word32" $ nf (toBoundedInteger :: Scientific -> Maybe Word32) s
    , bench "Word64" $ nf (toBoundedInteger :: Scientific -> Maybe Word64) s
    ]
