module Haskell where

import Control.Monad.Except
import Control.Monad.State.Lazy

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
