module Output.Terminal.Output
(
    outputAction,
    outputPlayerTurn,
    outputFlop,
    outputTurn,
    outputRiver,
    outputPlayerCards,
    outputWinner,
    outputWinners,
    outputGameOver,
    outputPlayersRemoved,
    outputHandValues
)
where

import Types
import Output.Terminal.OutputMessages
import PlayerUtilities

import Text.Printf
import Data.Function
import Data.List
import Data.Maybe
import Control.Lens hiding (Fold)

outputAction :: Game -> Action Int -> IO ()
outputAction game action = case action of
    Fold -> putStrLn $ printf actionFold num' playerName
    Check -> putStrLn $ printf actionCheck num' playerName
    Call -> putStrLn $ printf actionCall num' playerName bet'
    Raise a -> putStrLn $ printf actionRaise num' playerName bet' (bet' + a)
    AllIn -> putStrLn $ printf actionAllIn num' playerName chips'
    where num' = playerNum $ getCurrentPlayer game
          playerName = getCurrentPlayer game^.name
          bet' = game^.bets.currentBet
          chips' = getCurrentPlayer game^.chips + getCurrentPlayer game^.bet

outputPlayerTurn :: Game -> IO ()
outputPlayerTurn game = putStrLn $ printf playersTurn num' playerName
    where playerName = getCurrentPlayer game^.name
          num' = playerNum $ getCurrentPlayer game

{-# ANN outputFlop "HLint: ignore Use head" #-}
outputFlop :: Game -> IO ()
outputFlop game = putStrLn $ printf flopCards card1 card2 card3
    where cards' = game^.cardInfo.tableCards
          card1 = show $ cards' !! 0
          card2 = show $ cards' !! 1
          card3 = show $ cards' !! 2

outputTurn :: Game -> IO ()
outputTurn game = putStrLn . turnCard $ map show cards'
    where cards' = game^.cardInfo.tableCards

outputRiver :: Game -> IO ()
outputRiver game = putStrLn . riverCard $ map show cards'
    where cards' = game^.cardInfo.tableCards

outputPlayerCards :: Game -> IO ()
outputPlayerCards game = putStrLn $ playerCards players'
    where players' = game^..playerInfo.players.traversed

--have to reimplement gatherchips because betting imports this module
outputWinner :: Game -> Player -> IO ()
outputWinner game p = putStrLn $ printf winner (playerNum p) (p^.name) chips'
    where chips' = sum (game^..bets.pots.traversed.pot)
                 + sum (game^..playerInfo.players.traversed.bet)

outputWinners :: Game -> [(Pot, [Player])] -> IO ()
outputWinners _ = mapM_ (putStrLn . potWinners)

outputGameOver :: Game -> IO ()
outputGameOver game = putStrLn msg
    where winner' = maximumBy (compare `on` (^.chips)) players'
          players' = game^.playerInfo.players
          msg = printf totalWinner num' name' chips'
          name' = winner'^.name
          num' = playerNum winner'
          chips' = winner'^.chips

outputPlayersRemoved :: Game -> Maybe [Player] -> IO ()
outputPlayersRemoved _ = mapM_ (mapM_ $ putStrLn . helper)
    where helper p = printf playerRemoved (playerNum p) (p^.name)

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
playerCards players' = dealt ++ dealtCards
    where dealtCards = concatMap printCards players'

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
printWinners players' = start ++ end
    where start = concatMap (printWinner True) (init players')
          end = printWinner False (last players')

printWinner :: Bool -> Player -> String
printWinner final player = printf (playerMsg final) (playerNum player) 
                                  (player^.name)

--0 indexed
playerNum :: Player -> Int
playerNum p = p^.num + 1

outputHandValues :: Game -> IO ()
outputHandValues game = mapM_ (putStrLn . printHand) inPlayers
    where inPlayers = filter (^.inPlay) (game^.playerInfo.players)

{-# ANN printHand "HLint: ignore Use head" #-}
printHand :: Player -> String
printHand p = printf playerHand (playerNum p) (p^.name) value' card1 card2
    where value' = show . fromJust $ p^.handValue
          card1 = show $ (p^.cards) !! 0
          card2 = show $ (p^.cards) !! 1
