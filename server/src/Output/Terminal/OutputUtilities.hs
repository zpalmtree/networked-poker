module Output.Terminal.OutputUtilities
(
    playerNum,
    turnCard,
    riverCard,
    playerCards,
    potWinners,
    printHand
)
where

import Types
import Output.Terminal.OutputMessages
import Lenses (name, cards, pot, num, handInfo, handValue)

import Text.Printf
import Data.Maybe
import Control.Lens hiding (Fold)

turnCard :: [String] -> String
turnCard = turnOrRiver True

riverCard :: [String] -> String
riverCard = turnOrRiver False

turnOrRiver :: Bool -> [String] -> String
turnOrRiver turn xs = start ++ totalCards xs
    where start = printf msg (last xs)
          msg | turn = turnMsg
              | otherwise = riverMsg

totalCards :: [String] -> String
totalCards xs = printf (fullSet filler) (last xs)
    where filler = concatMap printCard (init xs)

printCard :: String -> String
printCard = printf card

playerCards :: [Player] -> String
playerCards players = dealt ++ dealtCards
    where dealtCards = concatMap printCards players

{-# ANN printCards "HLint: ignore Use head" #-}
printCards :: Player -> String
printCards p = printf hasCards (playerNum p) (p^.name) card1 card2
    where card1 = show $ (p^.cards) !! 0
          card2 = show $ (p^.cards) !! 1

potWinners :: (Pot, [Player]) -> String
potWinners (pot', players')
    | length players' == 1 = singleWinner pot' (head players')
    | otherwise = multiWinners pot' players'

multiWinners :: Pot -> [Player] -> String
multiWinners pot' players' = printf (multiWinnerMsg middle) (pot'^.pot)
    where middle = printWinners players'

singleWinner :: Pot -> Player -> String
singleWinner pot' p = printf singleWinnerMsg (playerNum p) (p^.name) (pot'^.pot)

printWinners :: [Player] -> String
printWinners players = start ++ end
    where start = concatMap (printWinner True) (init players)
          end = printWinner False (last players)

printWinner :: Bool -> Player -> String
printWinner final player = printf (playerMsg final) (playerNum player) 
                                  (player^.name)

--0 indexed
playerNum :: Player -> Int
playerNum p = p^.num + 1

{-# ANN printHand "HLint: ignore Use head" #-}
printHand :: Player -> String
printHand p = printf playerHand (playerNum p) (p^.name) value card1 card2
    where value = show $ fromJust (p^.handInfo)^.handValue
          card1 = show $ (p^.cards) !! 0
          card2 = show $ (p^.cards) !! 1
