module HumanMessage
(
    humanHandValues,
    humanAction,
    humanNewChips,
    humanNewShuffle
)
where

import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Data.Text (Text, pack)
import Control.Lens ((^.))
import Control.Monad.Trans.State (get)
import Safe (headNote)

import Utilities.Player (getCurrentPlayer)
import Types (Player, GameStateT, Action(..))

import Lenses 
    (handInfo, handValue, name, bets, currentBet, bet, chips, shuffleType)

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
            Call -> printf "%s called a bet of %d chips" 
                           (upperFirstLetter (p^.name))
                           (s^.bets.currentBet)

            Raise n -> printf "%s raised the bet from %d chips to %d chips"
                            (upperFirstLetter (p^.name)) (s^.bets.currentBet) n

            AllIn -> printf "%s went all in for a total bet of %d chips"
                            (upperFirstLetter (p^.name)) (p^.bet + p^.chips)
                                                        
    return [pack msg]

upperFirstLetter :: String -> String
upperFirstLetter [] = []
upperFirstLetter (x:xs) = toUpper x : xs

humanNewChips :: [(Int, [Player])] -> [Text]
humanNewChips = concatMap (map pack . potWinners)

potWinners :: (Int, [Player]) -> [String]
potWinners (pot', players')
    | length players' == 1 = [singleWinner pot' 
                             (headNote "in potWinners!" players')
                             "won"]
    | otherwise = multiWinners pot' players'

singleWinner :: Int -> Player -> String -> String
singleWinner n p verb = printf msg (upperFirstLetter $ p^.name) verb n 
    where msg = "%s %s the pot of %d chips"

multiWinners :: Int -> [Player] -> [String]
multiWinners n = map (\x -> singleWinner n x "shared")

humanNewShuffle :: GameStateT [Text]
humanNewShuffle = do
    s <- get
    return $ [pack $ "Switched to using " ++ show (s^.shuffleType) ++ " shuffle"]
