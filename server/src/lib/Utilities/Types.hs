module Utilities.Types
(
    mkCGame,
    mkGame
)
where

import Control.Monad.Trans.State (get)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))
import Data.IORef (newIORef)
import Safe (at)

import Utilities.Card (fullDeck)
import DrawCard (getInitFunc, getRNGFunc)

import Types 
    (GameStateT, ClientGame(..), Game(..), PlayerQueue(..), Cards(..), 
     CBets(..), Stage(..), Player, CPlayer(..), Bets(..), ShuffleType(..),
     Deck(..), RandomIndexDeck(..), DrawAlgorithm(..), RandomSource(..))

import Lenses 
    (stage, tableCards, bets, playerQueue, players, dealer, name, uuid, chips,
     inPlay, canReRaise, madeInitialBet, allIn, bet, cardInfo, currentBet,
     smallBlindSize, bigBlindSize, minimumRaise, shuffleType, algorithm,
     randomSource)
    
mkCGame :: GameStateT ClientGame
mkCGame = do
    s <- get

    let cgame = ClientGame players' dealer' currentPlayer (s^.stage) 
                           (s^.cardInfo.tableCards) cbets'

        dealer' = ((s^.playerQueue.players) `at` (s^.playerQueue.dealer))^.uuid

        currentPlayer = head (s^.playerQueue.players)^.uuid

        players' = map cP (s^.playerQueue.players)

        -- have to fill in cards later, can't tell everyone what everyones
        -- cards are, same with isMe
        cP p = CPlayer (p^.name) (p^.uuid) (p^.chips) [] (p^.inPlay)
                       (p^.allIn) (p^.bet) (p^.madeInitialBet) Nothing
                       (p^.canReRaise) False

        cbets' = CBets 0 (s^.bets.currentBet) (s^.bets.smallBlindSize) 
                         (s^.bets.bigBlindSize) (s^.bets.minimumRaise)

    return cgame

mkGame :: [Player] -> MVar [Player] -> ShuffleType -> IO Game
mkGame players' playerChan shuffleType' = do
    shuffleType'' <- newIORef shuffleType'

    let initFunc = getInitFunc (shuffleType'^.algorithm)
        rngFunc = getRNGFunc (shuffleType'^.randomSource)

    x <- initFunc rngFunc

    let cards' = Cards [] x

    return $ Game playerChan pq PreFlop cards' False bets' False 1 
             shuffleType' shuffleType''

    where pq = PlayerQueue players' (length players' - 1)
          bets' = Bets [] 0 smallBlind bigBlind bigBlind
          smallBlind = 10
          bigBlind = smallBlind * 2
