{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Criterion.Main
import Data.Foldable
import Data.Monoid
import Data.Text (Text)
import Haskell
import TextShow

---

nums :: [Int]
nums = [1 .. 1000]

str :: String
str = mconcat $ replicate 1000 "How fast is this"

tstr :: Text
tstr = mconcat $ replicate 1000 "How fast is this"

main :: IO ()
main = defaultMain
  [ bgroup "Eq"
    [ bench "list" $ nf (equalAll nums) nums
    ]
  , bgroup "Show"
    [ bench "show - list" $ nf show nums
    , bench "show - string" $ nf show str
    , bench "show - text" $ nf show tstr
    ]
  , bgroup "TextShow"
    [ bench "showt - list" $ nf showt nums
    , bench "showt - text" $ nf showt tstr
    , bench "manual" $ nf (\s -> "\"" <> s <> "\"") tstr
    ]
  , bgroup "Monoid"
    [ bench "fold - [Sum Int]" $ nf fold (map Sum nums)
    , bench "fold - [Maybe (Sum Int)]" $ nf fold (map (Just . Sum) nums)
    ]
  , bgroup "State"
    [ bench "oneGet" $ nf (runState get) (1 :: Int)
    , bench "bind" $ nf (\s -> runState s (1 :: Int)) bind
    , bench "countdown" $ nf (\s -> runState s (10000 :: Int)) countdown
    , bench "countdownT" $ nf (\s -> runStateT countdownT (10000 :: Int)) countdownT
    ]
  , bgroup "Applicative"
    [ bench "Dumb Sum" $ nf dumbSum nums
    ]
  , bgroup "IO"
    [ bench "recurseIO 1000" $ nfIO (recurseIO 1000)
    , bench "recurseIO 10000" $ nfIO (recurseIO 10000)
    , bench "recurseIO 100000" $ nfIO (recurseIO 100000)
    ]
  ]
