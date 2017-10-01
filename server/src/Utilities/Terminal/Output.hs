module Utilities.Terminal.Output
(
    turnCard,
    riverCard,
    playerCards,
    potWinners,
    printHand
)
where

import Text.Printf (printf)
import Data.Maybe (fromJust)
import Control.Lens ((^.))
import Safe (at, headNote, initNote, lastNote)

import Types (Player, Pot)
import Lenses (name, cards, pot, uuid, handInfo, handValue)

import Output.Terminal.OutputMessages
    (turnMsg, riverMsg, fullSet, card, dealt, hasCards, multiWinnerMsg,
     singleWinnerMsg, playerMsg, playerHand)

turnCard :: [String] -> String
turnCard = turnOrRiver True

riverCard :: [String] -> String
riverCard = turnOrRiver False

turnOrRiver :: Bool -> [String] -> String
turnOrRiver turn xs = start ++ totalCards xs
    where start = printf msg (lastNote "in turnOrRiver!" xs)
          msg | turn = turnMsg
              | otherwise = riverMsg

totalCards :: [String] -> String
totalCards xs = printf (fullSet filler) (lastNote "in turnOrRiver!" xs)
    where filler = concatMap printCard (initNote "in totalCards!" xs)

printCard :: String -> String
printCard = printf card

playerCards :: [Player] -> String
playerCards players = dealt ++ dealtCards
    where dealtCards = concatMap printCards players

printCards :: Player -> String
printCards p = printf hasCards (p^.name) card1 card2
    where card1 = show $ (p^.cards) `at` 0
          card2 = show $ (p^.cards) `at` 1

potWinners :: (Pot, [Player]) -> String
potWinners (pot', players')
    | length players' == 1 = singleWinner pot' 
                             (headNote "in potWinners!" players')
    | otherwise = multiWinners pot' players'

multiWinners :: Pot -> [Player] -> String
multiWinners pot' players' = printf (multiWinnerMsg middle) (pot'^.pot)
    where middle = printWinners players'

singleWinner :: Pot -> Player -> String
singleWinner pot' p = printf singleWinnerMsg (p^.name) (pot'^.pot)

printWinners :: [Player] -> String
printWinners players = start ++ end
    where start = concatMap (printWinner True) 
                            (initNote "in printWinners!" players)
          end = printWinner False (lastNote "in printWinners!" players)

printWinner :: Bool -> Player -> String
printWinner final player = printf (playerMsg final) (player^.name)

printHand :: Player -> String
printHand p = printf playerHand (p^.name) value card1 card2
    where value = show $ fromJust (p^.handInfo)^.handValue
          card1 = show $ (p^.cards) `at` 0
          card2 = show $ (p^.cards) `at` 1
