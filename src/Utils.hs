{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils
    ( roundTo
    , i2d
    , maxExpt
    , magnitude
    , toIntExponent
    , exponentOverflow
    , stripPowers
    , decimalDigits
    ) where

import GHC.Base (Int(I#), Char(C#), chr#, ord#, (+#))

import qualified Data.Primitive.Array as Primitive
import           Control.Monad.ST             (runST)

import           Data.Bits
import           Data.Char
import           Data.Maybe

roundTo :: Int -> [Int] -> (Int, [Int])
roundTo d is =
  case f d True is of
    x@(0,_) -> x
    (1,xs)  -> (1, 1:xs)
    _       -> error "roundTo: bad Value"
 where
  base = 10

  b2 = base `quot` 2

  f n _ []     = (0, replicate n 0)
  f 0 e (x:xs) | x == b2 && e && all (== 0) xs = (0, [])   -- Round to even when at exactly half the base
               | otherwise = (if x >= b2 then 1 else 0, [])
  f n _ (i:xs)
     | i' == base = (1,0:ds)
     | otherwise  = (0,i':ds)
      where
       (c,ds) = f (n-1) (even i) xs
       i'     = c + i

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i# ))

----------------------------------------------------------------------
-- Exponentiation with a cache for the most common numbers.
----------------------------------------------------------------------

-- | The same limit as in GHC.Float.
maxExpt :: Int
maxExpt = 324

expts10 :: Primitive.Array Integer
expts10 = runST $ do
    ma <- Primitive.newArray maxExpt uninitialised
    Primitive.writeArray ma 0  1
    Primitive.writeArray ma 1 10
    let go !ix
          | ix == maxExpt = Primitive.unsafeFreezeArray ma
          | otherwise = do
              Primitive.writeArray ma  ix        xx
              Primitive.writeArray ma (ix+1) (10*xx)
              go (ix+2)
          where
            xx = x * x
            x  = Primitive.indexArray expts10 half
            !half = ix `unsafeShiftR` 1
    go 2

uninitialised :: error
uninitialised = error "Data.Scientific: uninitialised element"

-- | @magnitude e == 10 ^ e@
magnitude :: Num a => Int -> a
magnitude e
    -- A negative e only comes from an overflow in the caller. The cache lookup
    -- does not check bounds, so it would read outside the array, or, for
    -- minBound, wrap around to the first element.
    | e < 0       = exponentOverflow
    | e < maxExpt = cachedPow10 e
    | otherwise   = cachedPow10 hi * 10 ^ (e - hi)
    where
      cachedPow10 = fromInteger . Primitive.indexArray expts10

      hi = maxExpt - 1

-- | Converts an exponent to an 'Int'. Raises an error if it does not fit.
toIntExponent :: Integer -> Int
toIntExponent = fromMaybe exponentOverflow . toIntegralSized

exponentOverflow :: a
exponentOverflow = error "Data.Scientific: exponent overflow"

-- | @stripPowers c p@ removes all factors of @p@ from @c@, and returns the
-- number of removed factors. Precondition: @c /= 0@.
stripPowers :: Integer -> Integer -> (Integer, Int)
stripPowers !c !p
    | r /= 0
    = (c, 0)

    -- remove factors of p*p; this speedups the normalisation by quite a bit.
    | let (c', k) = stripPowers q (p*p)
    , let (q', r') = quotRem c' p
    = if r' == 0 then (q', 2 * k + 2) else (c', 2 * k + 1)
  where
    (q, r) = quotRem c p

-- | @decimalDigits c e@ is 'Data.Scientific.toDecimalDigits' of the
-- non-negative scientific number with coefficient @c@ and exponent @e@, but
-- the returned exponent is an 'Integer', so it cannot overflow.
decimalDigits :: Integer -> Int -> ([Int], Integer)
decimalDigits 0 _ = ([0], 0)
decimalDigits c e = (ds, toInteger e + toInteger k + toInteger (length ds))
  where
    (c', k) = stripPowers c 10

    -- show for Integer is faster than repeated quotRem _ 10
    ds = map (\d -> ord d - ord '0') (show c')
