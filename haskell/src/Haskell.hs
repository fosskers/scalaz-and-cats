module Haskell where

import Control.Monad.Except
import Control.Monad.State.Strict

---

--------
-- STATE
--------

oneGet :: (Int, Int)
oneGet = runState get 1

bind :: State Int ()
bind = get >>= put . (+ 1)

countdown :: State Int Int
countdown = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> countdown)

-- | The base Monad can be `Either`, as we expect to be possible.
countdownT :: StateT Int (Either String) ()
countdownT = get >>= (\n -> if n <= 0 then throwError "darn!" else put (n-1) >> countdownT)

-- | Peel off the layers of the stack one at a time, from the outside inward.
runCountdownT :: Either String ((), Int)
runCountdownT = runStateT countdownT 10000

--------------
-- APPLICATIVE
--------------

dumbSum :: [Int] -> Maybe Int
dumbSum [] = Just 0
dumbSum (n:ns) = (+) <$> Just n <*> dumbSum ns

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
