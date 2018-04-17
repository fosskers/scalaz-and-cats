{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (ErrorCall, catch)
import Control.Monad.State.Strict
import Control.Monad.Except
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
  -- , bgroup "Show"
  --   [ bench "show - list" $ nf show nums
  --   , bench "show - string" $ nf show str
  --   , bench "show - text" $ nf show tstr
  --   ]
  -- , bgroup "TextShow"
  --   [ bench "showt - list" $ nf showt nums
  --   , bench "showt - text" $ nf showt tstr
  --   , bench "manual" $ nf (\s -> "\"" <> s <> "\"") tstr
  --   ]
  -- , bgroup "Monoid"
  --   [ bench "fold - [Sum Int]" $ nf fold (map Sum nums)
  --   , bench "fold - [Maybe (Sum Int)]" $ nf fold (map (Just . Sum) nums)
  --   ]
  -- , bgroup "State"
  --   [ bench "oneGet" $ nf (runState get) (1 :: Int)
  --   , bench "bind" $ nf (\s -> runState s (1 :: Int)) bind
  --   , bench "countdown" $ nf (\s -> runState s (10000 :: Int)) countdown
  --   , bench "countdownT" $ nf (\s -> runStateT countdownT (10000 :: Int)) countdownT
  --   ]
  -- , bgroup "Applicative"
  --   [ bench "Dumb Sum" $ nf dumbSum nums
  --   ]
  , bgroup "IO"
    [ bench "recurseIO 1k"   $ nfIO (recurseIO 1000)
    , bench "recurseIO 10k"  $ nfIO (recurseIO 10000)
    , bench "recurseIO 100k" $ nfIO (recurseIO 100000)
    , bench "IO Exception 1k"   $ nfIO (catch (ioCountdown 1000) (\(_ :: ErrorCall) -> pure 1))
    , bench "IO Exception 10k"  $ nfIO (catch (ioCountdown 10000) (\(_ :: ErrorCall) -> pure 1))
    , bench "IO Exception 100k" $ nfIO (catch (ioCountdown 100000) (\(_ :: ErrorCall) -> pure 1))
    , bench "ExceptT IO 1k"   $ nfIO (runExceptT (ioCountdownE 1000))
    , bench "ExceptT IO 10k"  $ nfIO (runExceptT (ioCountdownE 10000))
    , bench "ExceptT IO 100k" $ nfIO (runExceptT (ioCountdownE 100000))
    ]
  ]
