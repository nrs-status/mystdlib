module MyLib where

import Data.List
import Lens.Micro.Pro hiding (traversed)
import Numeric.Natural
import Data.Map (Map, fromList)
import Data.Char
import Control.Lens.Combinators (dropping, taking, takingWhile)
import Control.Lens.Traversal

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (<>)

tuplize :: Integral integral => integral -> [a] -> [[a]]
tuplize n = takeWhile ((== n) . fromIntegral . length) . map (take (fromIntegral n)) . tails

tuplizeIso :: Integral integral => integral -> Iso' [a] [[a]]
tuplizeIso n = iso (tuplize n) (map head)

{-# HLINT ignore tuplize_overlapless #-}
tuplize_overlapless :: Integral integral => integral -> [a] -> [[a]]
tuplize_overlapless n = takeWhile ((== n) . fromIntegral . length) . map (take $ fromIntegral n) . iterate (drop $ fromIntegral n)


-- modUsingEnumeration :: Int -> Lens [a] [(Int, a)] a (Int, a)
-- modUsingEnumeration n = lens 
--   (!! fromIntegral n)
--   (\s b -> 
--     let x = _ in
--     _)
