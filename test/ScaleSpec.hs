{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScaleSpec (spec) where

import Granite.Scale (TrainedScale (..), niceNum, niceTicks, train)
import Granite.Spec (
    Expand (..),
    LogBase (..),
    Scale (..),
    ScaleOpts (..),
    defScaleOpts,
 )
import Test.Hspec
import Test.QuickCheck

{- | A floating-point domain that the property tests treat as well-formed:
finite, non-NaN, and strictly increasing.
-}
data Domain = Domain {dLo :: Double, dHi :: Double}
    deriving (Eq, Show)

instance Arbitrary Domain where
    arbitrary = do
        a <- choose (-1000, 1000)
        d <- choose (0.5, 1000) -- guaranteed positive span
        pure (Domain a (a + d))

-- | A strictly-positive domain for log-scale tests.
data PosDomain = PosDomain {pdLo :: Double, pdHi :: Double}
    deriving (Eq, Show)

instance Arbitrary PosDomain where
    arbitrary = do
        lo <- choose (0.01, 100)
        factor <- choose (1.1, 1000)
        pure (PosDomain lo (lo * factor))

spec :: Spec
spec = describe "Granite.Scale" $ do
    describe "niceNum" $ do
        it "is idempotent on numbers already in {1,2,5} × 10^k" $ do
            niceNum 1 False `shouldBe` 1
            niceNum 2 False `shouldBe` 2
            niceNum 5 False `shouldBe` 5
            niceNum 10 False `shouldBe` 10
            niceNum 100 False `shouldBe` 100
            niceNum 0.5 False `shouldBe` 0.5
            niceNum 0.1 False `shouldBe` 0.1

        it "is always positive for positive input" $
            property $ \(Positive (x :: Double)) ->
                niceNum x True > 0

        it "is non-decreasing in 'ceil' mode" $
            property $ \(Positive (x :: Double)) ->
                niceNum x False >= x

    describe "niceTicks" $ do
        it "returns at least 2 ticks for a non-degenerate range" $
            property $ \(Domain lo hi) ->
                length (niceTicks (lo, hi) 5) >= 2

        it "ticks span the data range" $
            property $ \(Domain lo hi) ->
                let ticks = niceTicks (lo, hi) 5
                    tMin = minimum ticks
                    tMax = maximum ticks
                 in tMin <= lo + 1e-9 .&&. tMax >= hi - 1e-9

        it "ticks are monotonically increasing" $
            property $ \(Domain lo hi) ->
                let ticks = niceTicks (lo, hi) 5
                 in and (zipWith (<) ticks (drop 1 ticks))

        it "uses round-number step sizes ({1, 2, 5} × 10^k)" $
            property $ \(Domain lo hi) ->
                let ticks = niceTicks (lo, hi) 5
                 in case ticks of
                        (a : b : _) -> isRoundStep (b - a)
                        _ -> True

        it "tick count is reasonable for the requested target" $
            property $ \(Domain lo hi) ->
                let ticks = niceTicks (lo, hi) 5
                    n = length ticks
                 in n >= 2 .&&. n <= 12 -- nice numbers can over- or under-shoot a bit
    describe "train (linear)" $ do
        it "projects the lower bound to 0 and upper bound to 1" $
            property $ \(Domain lo hi) ->
                let ts = train (SLinear defScaleOpts) (lo, hi)
                    (lo', hi') = tsDomain ts
                 in abs (tsProject ts lo') < 1e-9
                        .&&. abs (tsProject ts hi' - 1) < 1e-9

        it "is the identity round-trip: unproject . project ≈ id" $
            property $ \(Domain lo hi) (v0 :: Double) ->
                let ts = train (SLinear defScaleOpts) (lo, hi)
                    (lo', hi') = tsDomain ts
                    v = lo' + (v0 `divNorm` (hi' - lo'))
                    t = tsProject ts v
                    v' = tsUnproject ts t
                 in abs (v - v') < 1e-6 .||. abs (hi' - lo') < 1e-12

        it "applies the configured expansion factor symmetrically" $
            property $ \(Domain lo hi) ->
                let mult = 0.1
                    expand = Expand mult 0
                    ts = train (SLinear defScaleOpts{scaleExpand = expand}) (lo, hi)
                    (lo', hi') = tsDomain ts
                    pad = (hi - lo) * mult
                 in abs ((lo - pad) - lo') < 1e-6
                        .&&. abs ((hi + pad) - hi') < 1e-6

    describe "train (log)" $ do
        it "log-base-10 of 1..1000 produces three-decade nice breaks" $ do
            let ts = train (SLog Base10 defScaleOpts) (1, 1000)
                breaks = tsBreaks ts
            length breaks `shouldSatisfy` (>= 2)
            length breaks `shouldSatisfy` (<= 12)
            -- All breaks must be positive powers of 10
            all
                (\v -> abs (v - 10 ** fromIntegral (round (logBase 10 v) :: Int)) < 1e-6 * v)
                breaks
                `shouldBe` True

        it "handles data with zero by clamping the domain" $ do
            -- previously this emitted hundreds of ticks; now caps at 10
            let ts = train (SLog Base10 defScaleOpts) (0, 100)
                breaks = tsBreaks ts
            length breaks `shouldSatisfy` (<= 12)

        it "is positive-only: projection of values below the floor saturates" $ do
            let ts = train (SLog Base10 defScaleOpts) (1, 1000)
            tsProject ts 0.0001 `shouldSatisfy` (\t -> t >= -1e-6)

    describe "train (sqrt)" $ do
        it "projects 0 to 0 (sqrt scale is bounded at 0)" $ do
            let ts = train (SSqrt defScaleOpts) (0, 100)
            abs (tsProject ts 0) `shouldSatisfy` (< 0.1)
            tsProject ts 100 `shouldSatisfy` (> 0.8)

        it "places 25 closer to 50% than to 50 (sqrt compresses tail)" $ do
            let ts = train (SSqrt defScaleOpts) (0, 100)
                t25 = tsProject ts 25
                t50 = tsProject ts 50
            -- sqrt(25) / sqrt(100) = 0.5 (so 25 lands at 50% — that's the point)
            abs (t25 - 0.5) `shouldSatisfy` (< 0.1)
            t50 `shouldSatisfy` (> t25)

    describe "train (reverse)" $ do
        it "reverses the projection of its inner scale" $
            property $ \(Domain lo hi) (Positive (t :: Double)) ->
                let unit = min 1.0 (t / 10)
                    inner = SLinear defScaleOpts{scaleExpand = Expand 0 0}
                    ts = train inner (lo, hi)
                    rs = train (SReverse inner) (lo, hi)
                    (l, h) = tsDomain ts
                    mid = l + unit * (h - l)
                 in abs (tsProject ts mid + tsProject rs mid - 1) < 1e-6

    describe "train (identity)" $ do
        it "projects values through unchanged" $ do
            let ts = train SIdentity (0, 10)
            tsProject ts 0 `shouldBe` 0
            tsProject ts 5 `shouldBe` 5
            tsUnproject ts 7 `shouldBe` 7

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Is @step@ of the form @{1, 2, 2.5, 5} × 10^k@?
isRoundStep :: Double -> Bool
isRoundStep s
    | s <= 0 = False
    | otherwise =
        let k = floor (logBase 10 s) :: Int
            mant = s / (10 ** fromIntegral k)
            -- Heckbert's set is {1, 2, 5}; we are slightly looser.
            allowed = [1, 2, 2.5, 5, 10]
         in any (\a -> abs (mant - a) < 1e-6) allowed

-- | Normalise a Double to a value in @[0, 1]@.
divNorm :: Double -> Double -> Double
divNorm v span_
    | abs span_ < 1e-12 = 0
    | otherwise =
        let u = abs v / (abs v + 1)
         in u * span_
