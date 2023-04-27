{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
module GitPairSpec where

import GitPair (AppState (..), Author (..), templateContents)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, (==>), Property, (===), forAll )
import GitPair.Test.Orphans ()

prop_secondAndThirdLineIsEmpty :: AppState -> Bool
prop_secondAndThirdLineIsEmpty state =
        case lines $ templateContents state of
            [] -> True
            [_] -> True
            _ : "": "" : _ -> True
            _ -> False

prop_issueInFirstLineOrEmpty :: AppState -> Bool
prop_issueInFirstLineOrEmpty state =
    case (story state, lines $ templateContents state) of
        (Nothing, []) -> True
        (Nothing, "": _) -> True
        (Just s, first_line:_) -> first_line == "#" <> s
        _ -> False


arbitraryAppStateWithSingleAuthor :: Gen AppState
arbitraryAppStateWithSingleAuthor = do
    state <- arbitrary
    author <- arbitrary
    pure $ state {authors = [author]}

arbitraryAppStateWithoutAuthor :: Gen AppState
arbitraryAppStateWithoutAuthor = do
    state <- arbitrary
    pure $ state {authors = []}
--
--
-- TODO: This is useless and should be removed
prop_showsOnlyStoryIfNoAuthors :: Property
prop_showsOnlyStoryIfNoAuthors =
    forAll arbitraryAppStateWithoutAuthor \state ->
        length (lines $ templateContents state) <= 1

prop_showsOnlyStoryIfSingleAuthors :: Property
prop_showsOnlyStoryIfSingleAuthors =
    forAll arbitraryAppStateWithSingleAuthor \state ->
        length (lines $ templateContents state) <= 1


prop_showsCoAuthoredByForMultipleAuthors :: AppState -> Property
prop_showsCoAuthoredByForMultipleAuthors state =
    length (authors state) > 1
        ==> let
                expected =
                    authors state
                    & map expanded
                    & map ("Co-authored-by: " <>)
                    & sort
            in drop 3 (lines $ templateContents state) === expected

