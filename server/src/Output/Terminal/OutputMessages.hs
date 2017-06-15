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
    riverCard
)
where

import Text.Printf

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
