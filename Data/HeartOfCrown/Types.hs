{-# LANGUAGE TemplateHaskell #-}
module Data.HeartOfCrown.Types 
(ActionPhase
,Field(Field)
,Coins
,Card
,Cards
,drawCards 
,drawCard
,setHands
)where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Either
import Control.Lens

type ActionPhase = EitherT Coins (State Field) Coins

data Field = Field {
                    _inheritanceRights :: Int,
                    _hand :: Cards,
                    _deck :: Cards,
                    _trash :: Cards
                   } 


type Cards = [Card]

type Card = Coins -> ActionPhase

type Coins = Int

makeLenses ''Field

addHands :: Field -> Cards -> Field 
addHands f cds = setHands f ((f ^. hand) ++ cds)

setHands :: Field -> Cards -> Field
setHands f cds = f & hand .~ cds

drawCard :: Field -> Field
drawCard f = let card = head (f^.deck)
                 deckTail = tail (f^.deck)
                 newHand = card : (f^.hand) 
             in (f&hand .~ newHand)&deck .~ deckTail

drawCards :: Int -> Field -> Field 
drawCards count f
  | count > 0 = drawCard $ drawCards (count - 1) f
  | count == 0 = f
  | count < 0 = error "count should be > 0"
