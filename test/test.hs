{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.Exception                  hiding (assert)
import           Control.Monad
import           Data.Either
import           Data.Hashable
import           Data.Int
import           Data.List
import           Data.Ratio
import           Data.Word
import           Data.Scientific                    as Scientific
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck                       (Property, (===), (.&&.))
import qualified Test.QuickCheck                    as QC
import           Test.Tasty.QuickCheck                 (testProperty)
import qualified Data.Binary                        as Binary
import qualified Data.Text.Lazy                     as TL  (unpack)
import qualified Data.Text.Lazy.Builder             as TLB (toLazyText)
import qualified Data.Text.Lazy.Builder.Scientific  as T
import           Numeric ( floatToDigits )
import           Text.Read

import qualified Data.ByteString.Lazy.Char8         as BLC8
import qualified Data.ByteString.Builder.Scientific as B
import qualified Data.ByteString.Builder            as B
import           Text.ParserCombinators.ReadP (readP_to_S)

main :: IO ()
main = testMain $ testGroup "scientific"
  [ testGroup "DoS protection"
    [ testGroup "Eq"
      [ testCase "1e1000000" $ assertBool "" $ (read "1e1000000" :: Scientific) == (read "1e1000000" :: Scientific)
      , testCase "1e1000000 ineq" $ assertBool "" $ (read "1e1000000" :: Scientific) /= (read "1e1000002" :: Scientific)

      -- this also indirectly checks that 'read' is fast enough.
      , testCase "10...0" $ assertBool "" $
          (read "1e1000000" :: Scientific) ==
          (read ('1' : replicate 1000000 '0'))
      ]
    , testGroup "Ord"
      [ testCase "compare 1234e1000000 123e1000001" $
          compare (read "1234e1000000" :: Scientific) (read "123e1000001" :: Scientific) @?= GT

      , testCase "10...0" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "0"))
              @?= EQ
      , testCase "1...1" $
          compare (read "1e1000001" :: Scientific)
                  (read ('1' : replicate 1000000 '0' ++ "1"))
              @?= LT
      ]

    , testGroup "isInteger"
        [ testCase "1e1000000" $ True @=? isInteger (read "1e1000000" :: Scientific)
        , testCase "10...0e-1" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1" :: Scientific)
        , testCase "10...0e-10...0" $ True @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-1000000" :: Scientific)
        , testCase "10...0e-20...0" $ False @=? isInteger (read $ '1' : replicate 1000000 '0' ++ "e-2000000" :: Scientific)
        ]

    , testGroup "toBoundedInteger"
        [ testCase "1e1000000" $ Nothing @=? toBoundedInteger @Int (read "1e1000000")
        , testCase "10...0e-1" $ Nothing @=? toBoundedInteger @Int (read $ '1' : replicate 1000000 '0' ++ "e-1")
        ]

    , testGroup "floatingOrInteger"
        [ testCase "1e1000000" $ Right (10 ^ (1000000 :: Int) :: Integer) @=? floatingOrInteger @Double @Integer (read "1e1000000")
        , testCase "10...0e-1" $ Right (10 ^ ( 999999 :: Int) :: Integer) @=? floatingOrInteger @Double @Integer (read $ '1' : replicate 1000000 '0' ++ "e-1")
        ]

    , testGroup "normalize"
        [ testCase "1e1000000" $ True @=? isInteger (normalize (read "1e1000000" :: Scientific))
        , testCase "10...0e-1" $ True @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-1" :: Scientific))
        , testCase "10...0e-10...0" $ True @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-1000000" :: Scientific))
        , testCase "10...0e-20...0" $ False @=? isInteger (normalize (read $ '1' : replicate 1000000 '0' ++ "e-2000000" :: Scientific))
        ]

    , testGroup "toDecimalDigits"
        [ testCase "9...9" $ do
            let (ds, n) = toDecimalDigits (read $ replicate 1000000 '9')
            (1000000,1000000) @=? (length ds, n)
        ]

    , testGroup "RealFrac"
      [ testGroup "floor"
        [ testCase "1e1000000"   $ (floor (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (floor (read "-1e-1000000" :: Scientific) :: Int) @?= (-1)
        , testCase "1e-1000000"  $ (floor (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "ceiling"
        [ testCase "1e1000000"   $ (ceiling (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (ceiling (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (ceiling (read "1e-1000000"  :: Scientific) :: Int) @?= 1
        ]
      , testGroup "round"
        [ testCase "1e1000000"   $ (round (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (round (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (round (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "truncate"
        [ testCase "1e1000000"   $ (truncate (read "1e1000000"   :: Scientific) :: Int) @?= 0
        , testCase "-1e-1000000" $ (truncate (read "-1e-1000000" :: Scientific) :: Int) @?= 0
        , testCase "1e-1000000"  $ (truncate (read "1e-1000000"  :: Scientific) :: Int) @?= 0
        ]
      , testGroup "properFracton"
        [ testCase "1e1000000"   $ properFraction (read "1e1000000" :: Scientific) @?= (0 :: Int, 0)
        , testCase "-1e-1000000" $ let s = read "-1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        , testCase "1e-1000000"  $ let s = read "1e-1000000" :: Scientific
                                   in properFraction s @?= (0 :: Int, s)
        ]
      ]
    , testGroup "toRealFloat"
      [ testCase "1e1000000"  $ assertBool "Should be infinity!" $ isInfinite $
                                  (toRealFloat (read "1e1000000" :: Scientific) :: Double)
      , testCase "1e-1000000" $ (toRealFloat (read "1e-1000000" :: Scientific) :: Double) @?= 0
      ]
      -- Without the fix, a repeating quotient computes digits until a
      -- remainder repeats, which takes up to 2305843009213693950 steps here.
    , localOption (mkTimeout 10000000) $ testGroup "Fractional"
      [ testCase "1 / 2305843009213693951" $
          assertRepeatingDecimal "1 divided by a prime other than 2 and 5 is a repeating decimal"
            (scientific 1 0 / scientific 2305843009213693951 0)
      , testCase "recip 2305843009213693951" $
          assertRepeatingDecimal "the reciprocal of a prime other than 2 and 5 is a repeating decimal"
            (recip (scientific 2305843009213693951 0))
      , testCase "fromRational (1 % 2305843009213693951)" $
          assertRepeatingDecimal "a fraction whose denominator is a prime other than 2 and 5 is a repeating decimal"
            (fromRational @Scientific (1 % 2305843009213693951))
      , testCase "1 / 1e-100000000" $
          assertEqual "the quotient must not hold the magnitude 10^100000000 in its coefficient"
            (1, 100000000) (let q = scientific 1 0 / scientific 1 (-100000000) in (coefficient q, base10Exponent q))
      ]
    ]

  , testGroup "Exponent overflow"
    [ testGroup "isInteger"
      [ testCase "1e(minBound)" $
          assertBool "a tiny positive number is not an integer"
            (not (isInteger (scientific 1 minBound)))
      , testCase "toBoundedInteger 1e(minBound)" $
          assertEqual "a tiny positive number is not an integer, so it has no Int value"
            Nothing (toBoundedInteger @Int (scientific 1 minBound))
      , testCase "floatingOrInteger 1e(minBound)" $
          assertEqual "a tiny positive number is not an integer, and it rounds to the Double 0"
            (Left 0) (floatingOrInteger @Double @Int (scientific 1 minBound))
      ]
      -- Without the fix, these compute 10^maxBound. The runtime cannot
      -- interrupt a multiplication of huge numbers, so the timeout fails the
      -- test only when the current one ends, after up to a minute.
    , localOption (mkTimeout 10000000) $ testGroup "toBoundedInteger"
      [ testCase "10e(maxBound)" $
          assertEqual "a number far above maxBound has no Int value"
            Nothing (toBoundedInteger @Int (scientific 10 maxBound))
      , testCase "-10e(maxBound)" $
          assertEqual "a number far below minBound has no Int value"
            Nothing (toBoundedInteger @Int (scientific (-10) maxBound))
      ]
    , testGroup "normalize"
      [ testCase "10e(maxBound)" $
          assertExponentOverflow "the normalized exponent maxBound + 1 does not fit in an Int"
            (normalize (scientific 10 maxBound))
      , testCase "10e(maxBound - 1)" $
          assertEqual "the normalized exponent maxBound fits in an Int"
            (1, maxBound) (let n = normalize (scientific 10 (maxBound - 1)) in (coefficient n, base10Exponent n))
      , testCase "toDecimalDigits 10e(maxBound)" $
          assertExponentOverflow "the digit exponent maxBound + 2 does not fit in an Int"
            (toDecimalDigits (scientific 10 maxBound))
      , testCase "toDecimalDigits 1e(maxBound)" $
          assertExponentOverflow "the digit exponent maxBound + 1 does not fit in an Int, although normalize succeeds"
            (toDecimalDigits (scientific 1 maxBound))
      ]
    , testGroup "hash"
      [ testCase "10e(maxBound) == 100e(maxBound - 1)" $
          assertEqual "equal numbers must have equal hashes, also when the normalized exponent does not fit in an Int"
            (hash (scientific 10 maxBound)) (hash (scientific 100 (maxBound - 1)))
      , testCase "10e(maxBound - 1) == 1e(maxBound)" $
          assertEqual "equal numbers must have equal hashes when the normalized exponent is maxBound"
            (hash (scientific 1 maxBound)) (hash (scientific 10 (maxBound - 1)))
      , testProperty "same as the hash of the normalized parts" $ \salt s ->
          let n = normalize s
          in hashWithSalt salt s === (salt `hashWithSalt` coefficient n `hashWithSalt` base10Exponent n)
      ]
    , testGroup "Formatting" $
      [ testCase "show 10e(maxBound)" $
          assertEqual "show must print the exponent maxBound + 1 without wrapping"
            "1.0e9223372036854775808" (show (scientific 10 maxBound))
      , testCase "show -10e(maxBound)" $
          assertEqual "show must print the exponent maxBound + 1 of a negative number without wrapping"
            "-1.0e9223372036854775808" (show (scientific (-10) maxBound))
        -- Without the error, this would print zeros until memory runs out,
        -- so the timeout turns a hang into a failure.
      , localOption (mkTimeout 10000000) $ testCase "Fixed 10e(maxBound)" $
          assertExponentOverflow "fixed notation would need more zeros than an Int can count"
            (length (formatScientific Scientific.Fixed Nothing (scientific 10 maxBound)))
      , localOption (mkTimeout 10000000) $ testCase "Fixed (Just 5) 1e(maxBound - 2)" $ do
          let s = scientific 1 (maxBound - 2)
          assertExponentOverflow "formatScientific: the exponent plus the decimal places overflows an Int"
            (length (formatScientific Scientific.Fixed (Just 5) s))
          assertExponentOverflow "Text builder: the exponent plus the decimal places overflows an Int"
            (length (TL.unpack (TLB.toLazyText (T.formatScientificBuilder Scientific.Fixed (Just 5) s))))
          assertExponentOverflow "ByteString builder: the exponent plus the decimal places overflows an Int"
            (length (BLC8.unpack (B.toLazyByteString (B.formatScientificBuilder Scientific.Fixed (Just 5) s))))
      ] ++
      [ testCase (name ++ " " ++ show (fmt, decs)) $
          assertEqual (name ++ " must print " ++ what ++ " without wrapping") expected (render fmt decs s)
      | (name, render) <-
          [ ("formatScientific", formatScientific)
          , ("Text builder", \fmt decs -> TL.unpack . TLB.toLazyText . T.formatScientificBuilder fmt decs)
          , ("ByteString builder", \fmt decs -> BLC8.unpack . B.toLazyByteString . B.formatScientificBuilder fmt decs)
          ]
      , (fmt, decs, s, expected, what) <-
          [ (Scientific.Generic, Nothing, scientific 10 maxBound, "1.0e9223372036854775808", "the exponent maxBound + 1")
          , (Scientific.Exponent, Nothing, scientific 10 maxBound, "1.0e9223372036854775808", "the exponent maxBound + 1")
          , (Scientific.Exponent, Just 1, scientific 999 maxBound, "1.0e9223372036854775810", "the exponent maxBound + 3 after rounding up")
          ]
      ]
    , testGroup "Num"
      [ testCase "1e(minBound) + 1e(maxBound)" $
          assertExponentOverflow "the difference of the exponents overflows an Int"
            (scientific 1 minBound + scientific 1 maxBound)
      , testCase "1e(maxBound) * 1e1" $
          assertExponentOverflow "the exponent maxBound + 1 of the product does not fit in an Int"
            (scientific 1 maxBound * scientific 1 1)
      , testCase "1e(minBound) * 1e-1" $
          assertExponentOverflow "the exponent minBound - 1 of the product does not fit in an Int"
            (scientific 1 minBound * scientific 1 (-1))
      , testCase "1e(maxBound) * 1e-1" $
          assertEqual "the exponent maxBound - 1 of the product fits in an Int"
            (scientific 1 (maxBound - 1)) (scientific 1 maxBound * scientific 1 (-1))
      , testCase "1e(maxBound - 1) * 1e1" $
          assertEqual "the exponent maxBound of the product fits in an Int"
            (1, maxBound) (let p = scientific 1 (maxBound - 1) * scientific 1 1 in (coefficient p, base10Exponent p))
      , testCase "1e(minBound + 1) * 1e-1" $
          assertEqual "the exponent minBound of the product fits in an Int"
            (1, minBound) (let p = scientific 1 (minBound + 1) * scientific 1 (-1) in (coefficient p, base10Exponent p))
      ]
    , testGroup "Parsing"
      [ testCase "0.1e-9223372036854775808" $
          assertEqual "the exponent minBound - 1 does not fit in an Int, so parsing must fail"
            Nothing (readMaybe @Scientific "0.1e-9223372036854775808")
      , testCase "1e9223372036854775808" $
          assertEqual "the exponent maxBound + 1 does not fit in an Int, so parsing must fail"
            Nothing (readMaybe @Scientific "1e9223372036854775808")
      , testCase "1e999...9" $
          assertEqual "an exponent of a million nines does not fit in an Int, so parsing must fail"
            Nothing (readMaybe @Scientific ("1e" ++ replicate 1000000 '9'))
      , testCase "1e9223372036854775807" $
          assertEqual "the exponent maxBound fits in an Int"
            (Just (scientific 1 maxBound)) (readMaybe "1e9223372036854775807")
      , testCase "1e-9223372036854775808" $
          assertEqual "the exponent minBound fits in an Int"
            (Just (scientific 1 minBound)) (readMaybe "1e-9223372036854775808")
      , testCase "0.1e9223372036854775808" $
          assertEqual "the fraction digit lowers the exponent maxBound + 1 to maxBound, which fits in an Int"
            (Just (scientific 1 maxBound)) (readMaybe "0.1e9223372036854775808")
      , testCase "read . show 10e(maxBound)" $
          assertEqual "read must parse what show prints for an exponent beyond maxBound"
            (scientific 10 maxBound) (read (show (scientific 10 maxBound)))
      , testCase "1e000...01" $
          assertEqual "the leading zeros of the exponent must not change its value"
            (Just (scientific 1 1)) (readMaybe ("1e" ++ replicate 1000000 '0' ++ "1"))
      ]
    , testGroup "Binary"
      [ testCase "exponent maxBound + 1" $
          assertBool "decoding must fail, because the exponent maxBound + 1 does not fit in an Int"
            (isLeft (decodeExponent (toInteger (maxBound @Int) + 1)))
      , testCase "exponent minBound - 1" $
          assertBool "decoding must fail, because the exponent minBound - 1 does not fit in an Int"
            (isLeft (decodeExponent (toInteger (minBound @Int) - 1)))
      , testCase "exponent maxBound" $
          assertEqual "decoding must accept the exponent maxBound"
            (Right (scientific 1 maxBound)) (decodeExponent (toInteger (maxBound @Int)))
      , testCase "exponent minBound" $
          assertEqual "decoding must accept the exponent minBound"
            (Right (scientific 1 minBound)) (decodeExponent (toInteger (minBound @Int)))
      ]
    , testGroup "Fractional"
        -- Without the fix, this computes 10^maxBound. The runtime cannot
        -- interrupt a multiplication of huge numbers, so the timeout fails
        -- the test only when the current one ends, after up to a minute.
      [ localOption (mkTimeout 10000000) $ testCase "1e(minBound) / 1e1" $
          assertExponentOverflow "the difference minBound - 1 of the exponents does not fit in an Int"
            (scientific 1 minBound / scientific 1 1)
      , testCase "1e(maxBound) / 1e-1" $
          assertExponentOverflow "the difference maxBound + 1 of the exponents does not fit in an Int"
            (scientific 1 maxBound / scientific 1 (-1))
      , testCase "1e(maxBound) / 1024e-5" $
          assertEqual "the exponent maxBound - 5 of the quotient fits in an Int, although the difference maxBound + 5 of the exponents does not"
            (9765625, maxBound - 5) (let q = scientific 1 maxBound / scientific 1024 (-5) in (coefficient q, base10Exponent q))
      , testCase "1e(minBound) / 1" $
          assertEqual "the exponent minBound of the quotient fits in an Int, although its negation does not"
            (1, minBound) (let q = scientific 1 minBound / scientific 1 0 in (coefficient q, base10Exponent q))
      ]
    , testGroup "Real"
      [ testCase "toRational 1e(minBound)" $
          assertExponentOverflow "the denominator 10^(-minBound) needs an exponent that overflows an Int"
            (toRational (scientific 1 minBound))
      ]
    ]

  , testProperty "normalization"
       (QC.forAll normalizedScientificGen    $ \s ->
            s /= 0 QC.==> abs (Scientific.coefficient s) `mod` 10 /= 0)

  , testGroup "Binary"
    [ testProperty "decode . encode == id" $ \s ->
        Binary.decode (Binary.encode s) === theSci s
    ]

  , testGroup "Parsing"
    [ testCase "reads \"\""        $ testReads ""        []
    , testCase "reads \"1.\""      $ testReads "1."      [(1.0, ".")]
    , testCase "reads \"1.2e\""    $ testReads "1.2e"    [(1.2, "e")]
    , testCase "reads \"(1.3 )\""  $ testReads "(1.3 )"  [(1.3, "")]
    , testCase "reads \"((1.3))\"" $ testReads "((1.3))" [(1.3, "")]
    , testCase "reads \" 1.3\""    $ testReads " 1.3"    [(1.3, "")]
    , testCase "read \" ( ((  -1.0e+3 ) ))\"" $ testRead " ( ((  -1.0e+3 ) ))" (-1000.0)
    , testCase "scientificP \"3\""       $ testScientificP "3"       [(3.0, "")]
    , testCase "scientificP \"3.0e2\""   $ testScientificP "3.0e2"   [(3.0, "e2"), (300.0, "")]
    , testCase "scientificP \"+3.0e+2\"" $ testScientificP "+3.0e+2" [(3.0, "e+2"), (300.0, "")]
    , testCase "scientificP \"-3.0e-2\"" $ testScientificP "-3.0e-2" [(-3.0, "e-2"), (-3.0e-2, "")]
    , testProperty "long coefficient" $
        QC.forAll (QC.scale (* 10) (QC.listOf1 (QC.elements ['0'..'9']))) $ \ds ->
          coefficient (read ds) === read @Integer ds
      -- The timeout checks the speed: a parser that takes quadratic time
      -- needed 62 s for this test, and the current one needs 0.6 s.
    , localOption (mkTimeout 10000000) $ testCase "0.999...9" $ do
        let s = read @Scientific ("0." ++ replicate 2000000 '9')
        assertEqual "the fraction digits must form the coefficient, and their count the negated exponent"
          (10 ^ (2000000 :: Int) - 1, -2000000) (coefficient s, base10Exponent s)
    ]

  , testGroup "Formatting"
    [ testProperty "read . show == id" $ \s -> read (show s) === theSci s
    , testCase "show (Just 1)"    $ testShow (Just 1)    "Just 1.0"
    , testCase "show (Just 0)"    $ testShow (Just 0)    "Just 0.0"
    , testCase "show (Just (-1))" $ testShow (Just (-1)) "Just (-1.0)"

    , testGroup "toDecimalDigits"
      [ testProperty "laws"
          (QC.forAll nonNegativeScientificGen    toDecimalDigits_laws)

      , testProperty "== Numeric.floatToDigits"
          (toDecimalDigits_eq_floatToDigits . QC.getNonNegative)
      ]

    , testGroup "Builder"
      [ testProperty "Text" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          TL.unpack (TLB.toLazyText $
                       T.formatScientificBuilder Scientific.Generic Nothing s)

      , testProperty "ByteString" $ \s ->
          formatScientific Scientific.Generic Nothing s ==
          BLC8.unpack (B.toLazyByteString $
                        B.formatScientificBuilder Scientific.Generic Nothing s)
      ]

    , testProperty "formatScientific_fromFloatDigits" $ \(d::Double) ->
        formatScientific Scientific.Generic Nothing (Scientific.fromFloatDigits d) ==
        show d

    -- , testProperty "formatScientific_realToFrac" $ \(d::Double) ->
    --     formatScientific B.Generic Nothing (realToFrac d :: Scientific) ==
    --     show d
    ]

  , testGroup "Eq"
    [ testProperty "==" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        (s1 == s2) == (toRational s1 == toRational s2)
    , testProperty "s == s" $ \(s :: Scientific) -> s == s

    , testProperty "extreme" $
        -- This was true in scientific-0.3.8.0
        -- due 'normalize' inherent limitation causing overflow
        --
        -- in scientific-0.3.9.0 this crashed.
        not (scientific 10 maxBound == scientific 1 minBound)

    ]

  , testGroup "Ord"
    [ testProperty "compare" $ \(s1 :: Scientific) (s2 :: Scientific) ->
        compare s1 s2 == compare (toRational s1) (toRational s2)

    , testProperty "extreme" $
        -- This was true in scientific-0.3.8.0
        -- due 'normalize' inherent limitation causing overflow
        --
        -- in scientific-0.3.9.0 this crashed.
        compare (scientific 10 maxBound) (scientific 1 minBound) === GT
    ]

  , testGroup "Num"
    [ testGroup "Equal to Rational"
      [ testProperty "fromInteger" $ \i -> fromInteger i === theSci (fromRational (fromInteger i))
      , testProperty "+"           $ bin (+)
      , testProperty "-"           $ bin (-)
      , testProperty "*"           $ bin (*)
      , testProperty "abs"         $ unary abs
      , testProperty "negate"      $ unary negate
      , testProperty "signum"      $ unary signum
      ]

    , testProperty "0 identity of +" $ \a -> a + 0 === theSci a
    , testProperty "1 identity of *" $ \a -> 1 * a === theSci a
    , testProperty "0 identity of *" $ \a -> 0 * a === theSci 0

    , testProperty "associativity of +"         $ \a b c -> a + (b + c) === (a + b) + theSci c
    , testProperty "commutativity of +"         $ \a b   -> a + b       === b + theSci a
    , testProperty "distributivity of * over +" $ \a b c -> a * (b + c) === a * b + a * theSci c

    , testProperty "subtracting the addition" $ \x y -> x + y - y === theSci x

    , testProperty "+ and negate" $ \x -> theSci x + negate x === 0
    , testProperty "- and negate" $ \x -> theSci x - negate x === x + x

    , testProperty "abs . negate == id"
        (QC.forAll nonNegativeScientificGen    $ \x -> abs (negate x) === theSci x)
    ]

  , testGroup "Real"
    [ testProperty "fromRational . toRational == id" $ \x ->
        (fromRational . toRational) x === theSci x
    ]

  , testGroup "Fractional"
    [ testProperty "fromRational gives the same coefficient and exponent as long division" $
        QC.forAll terminatingRationalGen $ \r ->
          let s = fromRational r
              (l, _) = fromRationalRepetendUnlimited r
          in (coefficient s, base10Exponent s) === (coefficient l, base10Exponent l)
    , testProperty "/ equal to Rational" $ \x ->
        QC.forAll terminatingDivisorGen $ \y ->
          toRational (x / y) === toRational x / toRational y
    , testProperty "recip equal to Rational" $
        QC.forAll terminatingDivisorGen $ \y ->
          toRational (recip y) === recip (toRational y)
    ]

  , testGroup "RealFrac"
    [ testGroup "Equal to Rational"
      [ testProperty "properFraction" $ \x ->
          let (n1::Integer, f1::Scientific) = properFraction x
              (n2::Integer, f2::Rational)   = properFraction (toRational x)
          in (n1 === n2) .&&. (f1 === fromRational f2)

      , testProperty "round" $ \(x::Scientific) ->
          (round x :: Integer) == round (toRational x)

      , testProperty "truncate" $ \(x::Scientific) ->
          (truncate x :: Integer) == truncate (toRational x)

      , testProperty "ceiling" $ \(x::Scientific) ->
          (ceiling x :: Integer) == ceiling (toRational x)

      , testProperty "floor" $ \(x::Scientific) ->
          (floor x :: Integer) == floor (toRational x)
      ]

    , testProperty "properFraction_laws" properFraction_laws

    , testProperty "round"    $ \s -> round    s == roundDefault    s
    , testProperty "truncate" $ \s -> truncate s == truncateDefault s
    , testProperty "ceiling"  $ \s -> ceiling  s == ceilingDefault  s
    , testProperty "floor"    $ \s -> floor    s == floorDefault    s
    ]

  , testGroup "Conversions"
    [ testProperty "fromRationalRepetend" $ \(l, r) -> r ==
        (case fromRationalRepetend (Just l) r of
          Left (s, rr) -> toRational s + rr
          Right (s, mbRepetend) ->
            case mbRepetend of
              Nothing       -> toRational s
              Just repetend -> toRationalRepetend s repetend)

    , testGroup "Float"  $ conversionsProperties (undefined :: Float)
    , testGroup "Double" $ conversionsProperties (undefined :: Double)

    , testGroup "floatingOrInteger"
      [ testProperty "correct conversion" $ \s ->
            case floatingOrInteger s :: Either Double Int of
              Left  d -> d == toRealFloat s
              Right i -> i == fromInteger (coefficient s') * 10^(base10Exponent s')
                  where
                    s' = normalize s
      , testProperty "Integer == Right" $ \(i::Integer) ->
          (floatingOrInteger (fromInteger i) :: Either Double Integer) == Right i
      , testProperty "Double == Left"
          (\(d::Double) -> genericIsFloating d QC.==>
             (floatingOrInteger (realToFrac d) :: Either Double Integer) == Left d)
      ]
    , testGroup "toBoundedInteger"
      [ testGroup "correct conversion"

        [ testCase "100e-2" $ toBoundedInteger @Int (read "100e-2") @?= Just 1
        , testProperty "Int64"       $ toBoundedIntegerConversion (undefined :: Int64)
        , testProperty "Word64"      $ toBoundedIntegerConversion (undefined :: Word64)
        , testProperty "NegativeNum" $ toBoundedIntegerConversion (undefined :: NegativeInt)
        ]
      ]
    ]
  , testGroup "toBoundedRealFloat"
    [ testCase "0 * 10^1000 == 0" $
        toBoundedRealFloat (scientific 0 1000) @?= Right (0 :: Float)
    ]
  , testGroup "toBoundedInteger"
    [ testGroup "to Int64" $
      [ testCase "succ of maxBound" $
        let i = succ . fromIntegral $ (maxBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "pred of minBound" $
        let i = pred . fromIntegral $ (minBound :: Int64)
            s = scientific i 0
        in (toBoundedInteger s :: Maybe Int64) @?= Nothing
      , testCase "0 * 10^1000 == 0" $
          toBoundedInteger (scientific 0 1000) @?= Just (0 :: Int64)
      ]
    ]
  , testGroup "Predicates"
    [ testProperty "isFloating" $ \s -> isFloating s ==      genericIsFloating s
    , testProperty "isInteger"  $ \s -> isInteger  s == not (genericIsFloating s)
    ]
  ]
  where
    assertExponentOverflow :: String -> a -> Assertion
    assertExponentOverflow preface x = do
        r <- try @ErrorCall (evaluate x)
        case r of
          Left (ErrorCall msg) -> assertEqual preface "Data.Scientific: exponent overflow" msg
          Right _ -> assertFailure (preface ++ "\nexpected the exponent overflow error, but the value was computed")

    assertRepeatingDecimal :: String -> a -> Assertion
    assertRepeatingDecimal preface x = do
        r <- try @ErrorCall (evaluate x)
        case r of
          Left (ErrorCall msg) ->
            assertBool (preface ++ "\nexpected the repeating decimal error, but got: " ++ msg)
              ("repeating decimal" `isInfixOf` msg)
          Right _ -> assertFailure (preface ++ "\nexpected the repeating decimal error, but the value was computed")

    -- The encoded number has the coefficient 1.
    decodeExponent :: Integer -> Either String Scientific
    decodeExponent e = case Binary.decodeOrFail (Binary.encode @(Integer, Integer) (1, e)) of
        Left (_, _, err) -> Left err
        Right (_, _, s)  -> Right s

-- used as type annotation
theSci :: Scientific -> Scientific
theSci = id

testMain :: TestTree -> IO ()
testMain = defaultMainWithIngredients defaultIngredients

testReads :: String -> [(Scientific, String)] -> Assertion
testReads inp out = reads inp @?= out

testRead :: String -> Scientific -> Assertion
testRead inp out = read inp @?= out

testShow :: Maybe Scientific -> String -> Assertion
testShow inp out = show inp @?= out

testScientificP :: String -> [(Scientific, String)] -> Assertion
testScientificP inp out = readP_to_S Scientific.scientificP inp @?= out

genericIsFloating :: RealFrac a => a -> Bool
genericIsFloating a = fromInteger (floor a :: Integer) /= a

toDecimalDigits_eq_floatToDigits :: Double -> Bool
toDecimalDigits_eq_floatToDigits d =
    Scientific.toDecimalDigits (Scientific.fromFloatDigits d)
      == Numeric.floatToDigits 10 d

conversionsProperties :: forall realFloat.
                         ( RealFloat    realFloat
                         , QC.Arbitrary realFloat
                         , Show         realFloat
                         )
                      => realFloat -> [TestTree]
conversionsProperties _ =
  [
    -- testProperty "fromFloatDigits_1" $ \(d :: realFloat) ->
    --   Scientific.fromFloatDigits d === realToFrac d

    -- testProperty "fromFloatDigits_2" $ \(s :: Scientific) ->
    --   Scientific.fromFloatDigits (realToFrac s :: realFloat) == s

    testProperty "toRealFloat" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . realToFrac) d == d

  , testProperty "toRealFloat . fromFloatDigits == id" $ \(d :: realFloat) ->
      (Scientific.toRealFloat . Scientific.fromFloatDigits) d == d

  -- , testProperty "fromFloatDigits . toRealFloat == id" $ \(s :: Scientific) ->
  --     Scientific.fromFloatDigits (Scientific.toRealFloat s :: realFloat) == s
  ]

toBoundedIntegerConversion
    :: forall i. (Integral i, Bounded i)
    => i -> Scientific -> Bool
toBoundedIntegerConversion _ s =
    case toBoundedInteger s :: Maybe i of
      Just i -> i == (fromIntegral $ (coefficient s') * 10^(base10Exponent s')) &&
                i >= minBound &&
                i <= maxBound
        where
          s' = normalize s
      Nothing -> isFloating s ||
                 s < fromIntegral (minBound :: i) ||
                 s > fromIntegral (maxBound :: i)

bin :: (forall a. Num a => a -> a -> a) -> Scientific -> Scientific -> Bool
bin op a b = toRational (a `op` b) == toRational a `op` toRational b

unary :: (forall a. Num a => a -> a) -> Scientific -> Bool
unary op a = toRational (op a) == op (toRational a)

toDecimalDigits_laws :: Scientific -> Bool
toDecimalDigits_laws x =
  let (ds, e) = Scientific.toDecimalDigits x

      rule1 = n >= 1
      n     = length ds

      rule2 = toRational x == coeff * 10 ^^ e
      coeff = foldr (\di a -> a / 10 + fromIntegral di) 0 (0:ds)

      rule3 = all (\di -> 0 <= di && di <= 9) ds

      rule4 | n == 1    = True
            | otherwise = null $ takeWhile (==0) $ reverse ds

  in rule1 && rule2 && rule3 && rule4

properFraction_laws :: Scientific -> Property
properFraction_laws x = fromInteger n + f === x        .&&.
                        (positive n == posX || n == 0) .&&.
                        (positive f == posX || f == 0) .&&.
                        abs f < 1
    where
      posX = positive x

      (n, f) = properFraction x :: (Integer, Scientific)

positive :: (Ord a, Num a) => a -> Bool
positive y = y >= 0

floorDefault :: Scientific -> Integer
floorDefault x = if r < 0 then n - 1 else n
                 where (n,r) = properFraction x

ceilingDefault :: Scientific -> Integer
ceilingDefault x = if r > 0 then n + 1 else n
                   where (n,r) = properFraction x

truncateDefault :: Scientific -> Integer
truncateDefault x =  m where (m,_) = properFraction x

roundDefault :: Scientific -> Integer
roundDefault x = let (n,r) = properFraction x
                     m     = if r < 0 then n - 1 else n + 1
                 in case signum (abs r - 0.5) of
                      -1 -> n
                      0  -> if even n then n else m
                      1  -> m
                      _  -> error "round default defn: Bad value"

newtype NegativeInt = NegativeInt Int
    deriving (Show, Enum, Eq, Ord, Num, Real, Integral)

instance Bounded NegativeInt where
    minBound = -100
    maxBound = -10

----------------------------------------------------------------------
-- QuickCheck instances
----------------------------------------------------------------------

instance QC.Arbitrary Scientific where
    arbitrary = QC.frequency
      [ (70, scientific <$> QC.arbitrary
                        <*> intGen)
      , (20, scientific <$> QC.arbitrary
                        <*> bigIntGen)
      , (10, scientific <$> pure 0
                        <*> bigIntGen)
      , (10, (\c e' e -> scientific (c * 10 ^ min 10 (abs e')) e) <$> QC.arbitrary <*> intGen <*> intGen)
      ]

    shrink s =
        [ scientific c e
        | (c, e) <- QC.shrink (Scientific.coefficient s, Scientific.base10Exponent s)
        ]

nonNegativeScientificGen :: QC.Gen Scientific
nonNegativeScientificGen =
    scientific <$> (QC.getNonNegative <$> QC.arbitrary)
               <*> intGen

normalizedScientificGen :: QC.Gen Scientific
normalizedScientificGen = Scientific.normalize <$> QC.arbitrary

-- The divisors of a power of 10, i.e. the denominators of terminating
-- decimals.
powerOf2And5Gen :: QC.Gen Integer
powerOf2And5Gen = (\a b -> 2 ^ a * 5 ^ b) <$> QC.choose @Int (0, 64) <*> QC.choose @Int (0, 64)

terminatingRationalGen :: QC.Gen Rational
terminatingRationalGen = (%) <$> QC.arbitrary <*> powerOf2And5Gen

-- A nonzero number whose reciprocal is a terminating decimal.
terminatingDivisorGen :: QC.Gen Scientific
terminatingDivisorGen = scientific <$> ((*) <$> QC.elements [1, -1] <*> powerOf2And5Gen) <*> intGen

bigIntGen :: QC.Gen Int
bigIntGen = QC.sized $ \size -> QC.resize (size * 1000) intGen

intGen :: QC.Gen Int
intGen = QC.arbitrary
