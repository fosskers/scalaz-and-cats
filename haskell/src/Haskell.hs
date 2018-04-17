{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Haskell where

import Control.Exception (ErrorCall, catch)
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Foldable (fold)

---

-------
-- SHOW
-------

showOpt :: String
showOpt = show $ Just 5

showAll :: Show a => [a] -> String
showAll = show

-----
-- EQ
-----

equalOpt :: Bool
equalOpt = Just 5 == Just 6

equalAll :: Eq a => [a] -> [a] -> Bool
equalAll l0 l1 = l0 == l1

---------
-- MONOID
---------

combineAll :: Monoid a => [a] -> a
combineAll = fold

--------
-- STATE
--------

oneGet :: (Int, Int)
oneGet = runState get 1

bind :: State Int ()
bind = get >>= put . (+ 1)

countdown :: State Int Int
countdown = get >>= (\(!n) -> if n <= 0 then pure n else put (n-1) >> countdown)

-- | The base Monad can be `Either`, as we expect to be possible.
countdownT :: StateT Int (Either String) ()
countdownT = get >>= (\(!n) -> if n <= 0 then throwError "darn!" else put (n-1) >> countdownT)

-- | Peel off the layers of the stack one at a time, from the outside inward.
runCountdownT :: Either String ((), Int)
runCountdownT = runStateT countdownT 10000

--------------
-- APPLICATIVE
--------------

dumbSum :: [Int] -> Maybe Int
dumbSum [] = Just 0
dumbSum (n:ns) = (+) <$> Just n <*> dumbSum ns

---------------
-- SIDE EFFECTS
---------------

greet :: String -> IO ()
greet msg = putStrLn $ "Hi, " ++ msg ++ "!"

recurseIO :: Int -> IO Int
recurseIO 0  = pure 0
recurseIO !n = pure (n - 1) >>= recurseIO

-- | Is is not wise to manually throw exceptions in Haskell.
ioException :: IO ()
ioException = putStrLn "Step 1" >> error "oh no" >> putStrLn "Step 2"

catchingExceptions :: IO ()
catchingExceptions = catch ioException (\(_ :: ErrorCall) -> putStrLn "crap")

ioCountdown :: Int -> IO Int
ioCountdown 0  = error "oh no"
ioCountdown !n = pure (n - 1) >>= ioCountdown

ioCountdownE :: Int -> ExceptT String IO Int
ioCountdownE 0  = throwError "oh no"
ioCountdownE !n = pure (n - 1) >>= ioCountdownE

--------------
-- TYPECLASSES
--------------

data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap f (Node a ts) = Node (f a) $ map (fmap f) ts

instance Applicative Tree where
  pure a = Node a []

  Node f fs <*> k@(Node a bs) = Node (f a) $ map (f <$>) bs ++ map (<*> k) fs

instance Monad Tree where
  return = pure

  Node a as >>= f = Node b $ bs ++ map (>>= f) as
    where Node b bs = f a

--------
-- STUBS
--------

-- How well does type inference work with `undefined` from Haskell and `???` from Scala when it comes to typeclass instances?
