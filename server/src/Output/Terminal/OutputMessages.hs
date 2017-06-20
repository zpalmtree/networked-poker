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
    playerMsg,
    totalWinner,
    playerRemoved,
    playerHand,
    roundNumberMsg
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
playerMsg final
    | final = msg
    | otherwise = msg ++ ", "
    where msg = "player %d, %s"

totalWinner :: String
totalWinner = "Game over! The winner was player %d, %s, who ended the game \
              \with %d chips!"

playerRemoved :: String
playerRemoved = "Thanks for playing player %d, %s, unfortunately you've run \
                \out of chips! Come back soon!"

playerHand :: String
playerHand = "Player %d, %s, has a %s, their cards are the %s and the %s."

roundNumberMsg :: String
roundNumberMsg = "\nRound number %d:\n"
