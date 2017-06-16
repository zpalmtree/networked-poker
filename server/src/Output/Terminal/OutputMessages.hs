module Output.Terminal.OutputMessages
(
    actionFold,
    actionCheck,
    actionCall,
    actionRaise,
    actionAllIn,
    playersTurn,
    flopCards,
    turnCard,
    riverCard,
    playerCards,
    winner,
    potWinners,
    playerNum
)
where

import Types

import Text.Printf
import Control.Lens

actionFold :: String
actionFold = "Player %d, %s, folded."

actionCheck :: String
actionCheck = "Player %d, %s, checked."

actionCall :: String
actionCall = "Player %d, %s, called a bet of %d."

actionRaise :: String
actionRaise = "Player %d, %s, raised the bet from %d to %d."

actionAllIn :: String
actionAllIn = "Player %d, %s, went all in for a total bet of %d."

playersTurn :: String
playersTurn = "It is player %d's turn. (%s)."

flopCards :: String
flopCards = "The flop has been revealed, the cards are the %s, the %s, and \
            \the %s."

turnCard :: [String] -> String
turnCard = turnOrRiver True

riverCard :: [String] -> String
riverCard = turnOrRiver False

turnOrRiver :: Bool -> [String] -> String
turnOrRiver turn xs = start ++ totalCards xs
    where start = printf msg (last xs)
          msg = if turn then turnMsg else riverMsg
          turnMsg = "The turn card has been revealed, it is the %s. "
          riverMsg = "The river card has been revealed, it is the %s. "

totalCards :: [String] -> String
totalCards xs = start ++ filler ++ end 
    where start = "The full set of cards on the table are now "
          filler = concatMap printCard (init xs)
          end = printf "and the %s." (last xs)

printCard :: String -> String
printCard = printf "the %s, "

playerCards :: [Player] -> String
playerCards players' = "The cards have been dealt. " ++ dealt
    where dealt = concatMap printCards players'

{-# ANN printCards "HLint: ignore Use head" #-}
printCards :: Player -> String
printCards p = printf " Player %d, %s has the %s and the %s."
                      (playerNum p) (p^.name) card1 card2
    where card1 = show $ (p^.cards) !! 0
          card2 = show $ (p^.cards) !! 1

winner :: String
winner = "Player %d, %s, won the hand, and gained %d chips!"

potWinners :: (Pot, [Player]) -> String
potWinners (pot', players')
    | length players' == 1 = singleWinner pot' (head players')
    | otherwise = multiWinners pot' players'

multiWinners :: Pot -> [Player] -> String
multiWinners pot' players' = printf (start ++ middle ++ end) (pot'^.pot)
    where start = "The pot of %d chips was won by "
          middle = printWinners players'
          end = " and they will share the winnings!"

singleWinner :: Pot -> Player -> String
singleWinner pot' p = printf msg (playerNum p) (p^.name) (pot'^.pot)
    where msg = "The pot of %d chips was won by player %d, %s, and they \
                \gained %d chips!"

printWinners :: [Player] -> String
printWinners players' = start ++ end
    where start = concatMap (printWinner True) (init players')
          end = printWinner False (last players')


printWinner :: Bool -> Player -> String
printWinner final player = printf fixed (playerNum player) (player^.name)
    where msg = "player %d, %s"
          fixed = if final then msg else msg ++ ", "

--0 indexed
playerNum :: Player -> Int
playerNum p = p^.num + 1
