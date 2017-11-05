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
import Utilities.Card (fullDeck)

import Types 
    (GameStateT, ClientGame(..), Game(..), PlayerQueue(..), Cards(..), 
     CBets(..), Stage(..), Player, CPlayer(..), Bets(..), ShuffleType(..),
     Deck(..), RandomIndexDeck(..), DrawAlgorithm(..), RandomSource(..))

import Lenses 
    (stage, tableCards, bets, playerQueue, players, dealer, name, uuid, chips,
     inPlay, canReRaise, madeInitialBet, allIn, bet, cardInfo, currentBet,
     smallBlindSize, bigBlindSize, minimumRaise)
    
mkCGame :: GameStateT ClientGame
mkCGame = do
    s <- get

    let cgame = ClientGame players' dealer' currentPlayer (s^.stage) 
                           (s^.cardInfo.tableCards) cbets'

        dealer' = ((s^.playerQueue.players) !! (s^.playerQueue.dealer))^.uuid

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

mkGame :: [Player] -> MVar [Player] -> IO Game
mkGame players' playerChan = do
    let shuffleType' = ShuffleType RandomIndex LEucyer
        cards' = Cards [] (IsRandomIndex $ RandomIndexDeck fullDeck)

    shuffleType'' <- newIORef shuffleType'

    return $ Game playerChan pq PreFlop cards' False bets' False 1 
             shuffleType' shuffleType''

    where pq = PlayerQueue players' (length players' - 1)
          bets' = Bets [] 0 smallBlind bigBlind bigBlind
          smallBlind = 10
          bigBlind = smallBlind * 2
