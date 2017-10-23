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

main :: IO ()
main = defaultMain
  [ bgroup "Eq"
    [ bench "list" $ nf (equalAll nums) nums
    ]
  , bgroup "Show"
    [ bench "show - list" $ nf show nums
    , bench "show - string" $ nf show "How fast is this"
    ]
  , bgroup "Monoid"
    [ bench "fold - [Sum Int]" $ nf fold (map Sum nums)
    , bench "fold - [Maybe (Sum Int)]" $ nf fold (map (Just . Sum) nums)
    ]
  , bgroup "TextShow"
    [ bench "showt - list" $ nf showt nums
    , bench "showt - text" $ nf showt ("How fast is this" :: Text)
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
  where nums = [1..1000] :: [Int]
