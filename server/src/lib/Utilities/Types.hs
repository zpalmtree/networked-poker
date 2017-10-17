module Utilities.Types
(
    mkCGame,
    mkGame
)
where

import Control.Monad.Trans.State (get)
import Control.Concurrent.MVar (MVar)
import Control.Lens ((^.))

import Types 
    (GameStateT, ClientGame(..), Game(..), PlayerQueue(..),
     Cards(..), Bets(..), Stage(..), Player, Card(..), Value(..), Suit(..),
     CPlayer(..))

import Lenses 
    (stage, tableCards, bets, playerQueue, players, dealer, name, uuid, chips,
     inPlay, canReRaise, madeInitialBet, allIn, bet, cardInfo)
    
mkCGame :: GameStateT ClientGame
mkCGame = do
    s <- get

    let cgame = ClientGame players' dealer' currentPlayer (s^.stage) 
                           (s^.cardInfo.tableCards) (s^.bets)

        dealer' = ((s^.playerQueue.players) !! (s^.playerQueue.dealer))^.uuid

        currentPlayer = head (s^.playerQueue.players)^.uuid

        players' = map cP (s^.playerQueue.players)

        -- have to fill in cards later, can't tell everyone what everyones
        -- cards are, same with isMe
        cP p = CPlayer (p^.name) (p^.uuid) (p^.chips) [] (p^.inPlay)
                       (p^.allIn) (p^.bet) (p^.madeInitialBet) Nothing
                       (p^.canReRaise) False

    return cgame

mkGame :: [Player] -> MVar [Player] -> Game
mkGame players' playerChan = game'
    where pq = PlayerQueue players' 0
          cards' = Cards [] fullDeck
          bets' = Bets [] 0 smallBlind bigBlind bigBlind
          smallBlind = 10
          bigBlind = smallBlind * 2
          game' = Game playerChan pq PreFlop cards' False bets' False 1
          -- redefine to prevent import loop
          fullDeck = [Card value suit | value <- [Two .. Ace],
                                        suit  <- [Heart .. Diamond]]
