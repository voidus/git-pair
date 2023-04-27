{-# OPTIONS_GHC -Wno-orphans #-}
module GitPair.Test.Orphans where

import Test.QuickCheck (Arbitrary (arbitrary), Arbitrary1 (liftArbitrary), Gen, suchThat)
import GitPair (AppState (..), Author(..))
import Data.Text.Arbitrary ()

arbitraryTextWithoutLinebreaks :: Gen Text
arbitraryTextWithoutLinebreaks = mconcat . lines <$> arbitrary

instance Arbitrary AppState where
    arbitrary = do
        story <- liftArbitrary arbitraryTextWithoutLinebreaks
        authors <- arbitrary
        pure AppState {..}

instance Arbitrary Author where
    arbitrary = do
        -- TODO: Maybe we should check this assumption at runtime
        initials <- arbitraryTextWithoutLinebreaks `suchThat` (/= "")
        expanded <- arbitraryTextWithoutLinebreaks `suchThat` (/= "")
        pure Author {..}

