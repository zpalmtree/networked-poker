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
    roundNumberMsg,
    smallBlind,
    bigBlind
)
where

actionFold :: String
actionFold
    = "%s folded."

actionCheck :: String
actionCheck
    = "%s checked."

actionCall :: String
actionCall
    = "%s called a bet of %d."

actionRaise :: String
actionRaise
    = "%s raised the bet from %d to %d."

actionAllIn :: String
actionAllIn
    = "%s went all in for a total bet of %d."

playersTurn :: String
playersTurn
    = "It is %s's turn."

flopCards :: String
flopCards
    = "The flop has been revealed, the cards are the %s, the %s, and the %s."

turnMsg :: String
turnMsg
    = "The turn card has been revealed, it is the %s. "

riverMsg :: String
riverMsg
    = "The river card has been revealed, it is the %s. "

fullSet :: String -> String
fullSet middle
    = "The full set of cards on the table are now " ++ middle ++ "and the %s." 

card :: String
card 
    = "the %s, "

dealt :: String
dealt 
    = "The cards have been dealt."

hasCards :: String
hasCards 
    = " %s has the %s and the %s."

winner :: String
winner 
    = "%s won the hand, and gained %d chips!"

multiWinnerMsg :: String -> String
multiWinnerMsg middle
    = "The pot of %d chips was won by " ++ middle ++ " and they will " ++
      "share the winnings!"

singleWinnerMsg :: String
singleWinnerMsg
    = "%s won the pot of %d chips!"

playerMsg :: Bool -> String
playerMsg final
    | final = msg
    | otherwise = msg ++ ", "
    where msg = "%s"

totalWinner :: String
totalWinner
    = "Game over! The winner was %s, who ended the game with %d chips!"

playerRemoved :: String
playerRemoved
    = "Thanks for playing %s, unfortunately you've run out of " ++
      "chips! Come back soon!"

playerHand :: String
playerHand
    = "%s, has a %s, their cards are the %s and the %s."

roundNumberMsg :: String
roundNumberMsg
    = "\nRound number %d:\n"

smallBlind :: String
smallBlind
    = "%s posted a small blind of %d."

bigBlind :: String
bigBlind
    = "%s posted a big blind of %d."
