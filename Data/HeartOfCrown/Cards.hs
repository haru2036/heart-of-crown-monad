module Data.HeartOfCrown.Cards where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.HeartOfCrown.Types

farm :: Card
farm c = do 
  return (c + 1)

city :: Card
city c = do 
  return (c + 2)

stop :: Card
stop c = do
  fld <- (lift get)
  left c

alchemist :: Card
alchemist c = do
  fld <- lift get
  lift $ put $ drawCards 2 fld
  return c

dummyField = Field 0 [] [] [] 

zeroCoins = 0 :: Coins

runActionPhase = runState . runEitherT
