module Output.Terminal.OutputMessages
(
    actionFold,
    actionCheck,
    actionCall,
    actionRaise,
    actionAllIn,
    playersTurn,
    flopCards,
    turnMsg,
    riverMsg,
    fullSet,
    card,
    dealt,
    hasCards,
    winner,
    multiWinnerMsg,
    singleWinnerMsg,
    playerMsg
)
where

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

turnMsg :: String
turnMsg = "The turn card has been revealed, it is the %s. "

riverMsg :: String
riverMsg = "The river card has been revealed, it is the %s. "

fullSet :: String -> String
fullSet middle = start ++ middle ++ end
    where start = "The full set of cards on the table are now "
          end = "and the %s."

card :: String
card = "the %s, "

dealt :: String
dealt = "The cards have been dealt. "

hasCards :: String
hasCards = " Player %d, %s, has the %s and the %s."

winner :: String
winner = "Player %d, %s, won the hand, and gained %d chips!"

multiWinnerMsg :: String -> String
multiWinnerMsg middle = start ++ middle ++ end
    where start = "The pot of %d chips was won by "
          end = " and they will share the winnings!"

singleWinnerMsg :: String
singleWinnerMsg = "The pot of %d chips was won by player %d, %s, and they \
                  \gained %d chips!"

playerMsg :: Bool -> String
playerMsg final = if final then msg else msg ++ ", "
    where msg = "player %d, %s"
