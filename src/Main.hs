{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens        (Iso', Reversing, Traversal', iso, ix, reversed,
                            reversing, (%=), (.=), (<&>), (^.), (^..), (^?))
import Control.Monad       (forever)
import Control.Monad.State (StateT, evalStateT, get, liftIO)
import Data.Default        (Default, def)
import Data.Maybe          (catMaybes)
import Data.Monoid         (Sum (Sum, getSum), (<>))
import Linear              (M44, V4 (V4), transpose, _w, _x, _y, _z)
import System.Console.ANSI (clearScreen)
import Text.Read           (readMaybe)

import qualified Text.PrettyPrint.Boxes as Boxes

type Board = M44 (Maybe (Sum Integer))
type Game = StateT Board IO ()

instance Default Board where
  def = V4 naught naught naught naught
    where naught = V4 Nothing Nothing Nothing Nothing

main :: IO ()
main = evalStateT loop def
  where
    loop :: Game
    loop = forever $ do
      liftIO clearScreen
      get >>= liftIO . Boxes.printBox . mkBox
      liftIO getLine >>= \case
        "h" -> rows %= merge
        "j" -> locs %= merge
        "k" -> cols %= merge
        "l" -> wors %= merge
        (parseCell -> Just (lx, ly, val)) -> lx . ly .= Just (Sum val)
        _ -> return ()

    parseCell (words -> [x, y, v]) =
        (,,) <$> parseLens x
             <*> parseLens y
             <*> readMaybe v
    parseCell _ = Nothing

    parseLens "x" = Just _x
    parseLens "y" = Just _y
    parseLens "z" = Just _z
    parseLens "w" = Just _w
    parseLens _ = Nothing

--------------------------------------------------------------------------------
-- Render
--------------------------------------------------------------------------------

class Box a where
  mkBox :: a -> Boxes.Box

instance Box Board where
  mkBox v = Boxes.vsep 1 Boxes.center1 $ v^..traverse <&> mkBox

instance Box (V4 (Maybe (Sum Integer))) where
  mkBox v = Boxes.hsep 2 Boxes.center1 $ v^..traverse <&> Boxes.text . f
    where f = maybe "X" (show . getSum)

--------------------------------------------------------------------------------
-- Logic
--------------------------------------------------------------------------------

instance Reversing (V4 a) where
  reversing v = V4 (v^._w) (v^._z) (v^._y) (v^._x)

merge :: (Eq a, Monoid a) => [a] -> [a]
merge (x:x':xs) | x == x' = (x <> x') : merge xs
merge (x:xs) = x : merge xs
merge [] = []

rows, wors, cols, locs :: Traversal' (M44 (Maybe  a)) [a]
rows = traverse . list
wors = traverse . reversed . list
cols = transposed . rows
locs = transposed . wors

transposed :: Iso' (M44 a) (M44 a)
transposed = iso transpose transpose

list :: Iso' (V4 (Maybe a)) [a]
list = iso toList fromList
  where
    toList v = reverse $ catMaybes $ foldl (flip (:)) [] v
    fromList (xs :: [a]) = V4 (xs^?ix 0) (xs^?ix 1) (xs^?ix 2) (xs^?ix 3)
