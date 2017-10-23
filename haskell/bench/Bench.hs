{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State.Strict
import Haskell
import Criterion.Main
import Data.Text (Text)
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
    [ bench "recurseIO" $ nfIO (recurseIO 1000)
    , bench "recurseIO" $ nfIO (recurseIO 10000)
    , bench "recurseIO" $ nfIO (recurseIO 100000)
    ]
  ]
  where nums = [1..1000] :: [Int]
