import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

type ActionPhase = EitherT Coins (State Field) Coins

data Field = Field {
                    inheritanceRights :: Int,
                    hand :: Cards,
                    deck :: Cards,
                    trash :: Cards
                   }

type Cards = [Card]

type Card = Coins -> ActionPhase

type Coins = Int

dummyField = Field 0 [] [] [] 

zeroCoins = 0 :: Coins

farm :: Card
farm c = do 
  return (c + 1)


stop :: Card
stop c = do
  fld <- (lift get)
  return c


printEither (Right (c, _)) = print ("Right " ++ show c)
printEither (Left (c, _)) = print ("Left " ++ show c)
