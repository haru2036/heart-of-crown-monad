import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class

data Field = Field {
                    inheritanceRights :: Int,
                    hand :: Cards,
                    deck :: Cards,
                    trash :: Cards
                   }

type Cards = [Card]

type Card = Coins -> StateT Field (Either (Coins, Field)) Coins

type Coins = Int

type ActionPhaseT = StateT Field (Either (Coins, Field)) Coins

data ActionPhaseO a = Running Field a | Finished Field a

farm :: Card
farm c = do
  return (c + 1)

alchemist :: Card
alchemist c = do
  f <- get
  return c

stop :: Card
stop c = do
  fld <- get
  lift $ Left (c, fld)

dummyField = Field 0 [] [] [] 

testRun = runStateT (farm 0 >>= stop >>= farm) dummyField

zeroCoins = 0 :: Coins

printEither (Right (c, _)) = print ("Right " ++ show c)
printEither (Left (c, _)) = print ("Left " ++ show c)
