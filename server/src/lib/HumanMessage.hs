module HumanMessage
(
    humanHandValues,
    humanAction
)
where

import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Text (Text, pack)
import Control.Lens ((^.))
import Control.Monad.Trans.State (get)

import Utilities.Player (getCurrentPlayer)
import Types (Player, GameStateT, Action(..))
import Lenses (handInfo, handValue, name, bets, currentBet, bet, chips)

humanHandValues :: [Player] -> [Text]
humanHandValues = map humanHandValues'

humanHandValues' :: Player -> Text
humanHandValues' p = pack $ printf "%s has a %s" 
                                   (upperFirstLetter (p^.name))
                                   (show $ fromJust (p^.handInfo)^.handValue)

humanAction :: Action Int -> GameStateT [Text]
humanAction a = do
    s <- get
    p <- getCurrentPlayer

    let msg = case a of
            SmallBlind -> printf "%s posted the small blind" 
                                $ upperFirstLetter (p^.name)

            BigBlind -> printf "%s posted the big blind"
                                $ upperFirstLetter (p^.name)

            Fold -> printf "%s folded" $ upperFirstLetter (p^.name)
            Check -> printf "%s checked" $ upperFirstLetter (p^.name)
            Call -> printf "%s called a bet of %d" (upperFirstLetter (p^.name))
                                                   (s^.bets.currentBet)

            Raise n -> printf "%s raised the bet from %d to %d"
                            (upperFirstLetter (p^.name)) (s^.bets.currentBet) n

            AllIn -> printf "%s went all in for a total bet of %d"
                            (upperFirstLetter (p^.name)) (p^.bet + p^.chips)
                                                        
    return [pack msg]

upperFirstLetter :: String -> String
upperFirstLetter [] = []
upperFirstLetter (x:xs) = toUpper x : xs
