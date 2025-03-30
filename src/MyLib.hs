module MyLib where

import Data.List

tuplize :: Int -> [a] -> [[a]]
tuplize n = takeWhile ((== n) . length) . map (take n) . tails

{-# HLINT ignore tuplize_overlapless #-}
tuplize_overlapless :: Int -> [a] -> [[a]]
tuplize_overlapless n = takeWhile ((== n) . length) . map (take n) . iterate (drop n)

