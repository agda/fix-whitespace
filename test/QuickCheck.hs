{-# LANGUAGE TemplateHaskell #-}

-- | Test 'dropWhileEnd1'.

module Main where

import Data.Maybe            ( isJust )

import Test.QuickCheck.All   ( allProperties )
import Test.Tasty            ( defaultMain )
import Test.Tasty.QuickCheck ( testProperties )

import Data.List.Extra.Drop  ( dropWhile1, dropWhileEnd1 )

-- | If the predicate is true for exactly one value, we can express 'dropWhileEnd1' in terms of 'dropWhile1'.
--
--   Example: predicate 'not' holds only for 'False'.
--
prop_dropWhileEnd1_Bool :: [Bool] -> Bool
prop_dropWhileEnd1_Bool xs = dropWhileEnd1 not xs == (reverse . dropWhile1 not . reverse) xs

-- | If the predicate is *not* a singleton, this relation does not hold.
--
prop_dropWhileEnd1_Maybe_Bool :: Bool
prop_dropWhileEnd1_Maybe_Bool = l /= r
  where
  xs = [Just True, Just False]
  l  = dropWhileEnd1 isJust xs                     -- Just True
  r  = (reverse . dropWhile1 isJust . reverse) xs  -- Just False

-- Pseudo Template Haskell instruction to make $allProperties work
return []

main :: IO ()
main = defaultMain $ testProperties "Tests" $allProperties
