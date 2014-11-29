import Control.Monad.Trans.State.Lazy

data Field = Field {
                    inheritanceRights :: Int 
                    hand :: Cards
                    deck :: Cards
                   }

type Cards = [Card]

type Card a = a -> Either a a

type TActionPhase a = StateT Field (Either a) a

data OActionPhase a = Running Field a | Finished Field a
