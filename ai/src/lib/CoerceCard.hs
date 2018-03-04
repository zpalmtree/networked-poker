module CoerceCard
(
    cardToEval,
    evalToCard
)
where

import Control.Lens ((^.))

import qualified Data.Poker as P 
    (Card, Rank(..), Suit(..), mkCard, cardRank, cardSuit)

import Types (Card(..), Value(..), Suit(..))
import Lenses (value, suit)

cardToEval :: Card -> P.Card
cardToEval c = P.mkCard (valueToRank $ c^.value) (suitToEvalSuit $ c^.suit)

valueToRank :: Value -> P.Rank
valueToRank = toEnum . fromEnum

rankToValue :: P.Rank -> Value
rankToValue = toEnum . fromEnum

suitToEvalSuit :: Suit -> P.Suit
suitToEvalSuit Heart = P.Hearts
suitToEvalSuit Diamond = P.Diamonds
suitToEvalSuit Spade = P.Spades
suitToEvalSuit Club = P.Clubs

evalSuitToSuit :: P.Suit -> Suit
evalSuitToSuit P.Hearts = Heart
evalSuitToSuit P.Diamonds = Diamond
evalSuitToSuit P.Spades = Spade
evalSuitToSuit P.Clubs = Club

evalToCard :: P.Card -> Card
evalToCard c = Card (rankToValue $ P.cardRank c) (evalSuitToSuit $ P.cardSuit c)
