module Utilities.ClientTypes
(
    makeCGame
)
where

import Control.Lens ((^.))

import Types (ClientGame, ClientPlayerQueue, ClientPlayer, Card)

import ClientTypes 
    (CGame(..), CPlayerQueue(..), CPlayer(..), CCards(..), PSignals, CSignals)

import Lenses 
    (clientStage, clientBets, clientPlayerQueue, clientCommunityCards,
     clientPlayers, clientDealer, clientName, clientUUID, clientChips,
     clientInPlay, clientAllIn, clientBet, clientMadeInitialBet,
     clientHandValue, clientCanReRaise, clientIsMe, clientCards)

makeCGame :: ClientGame -> [PSignals] -> CSignals -> CGame
makeCGame c pSigs cSigs = CGame pq (c^.clientStage) cc (c^.clientBets)
    where pq = makeCPlayerQueue (c^.clientPlayerQueue) pSigs
          cc = makeCCards (c^.clientCommunityCards) cSigs

makeCPlayerQueue :: ClientPlayerQueue -> [PSignals] -> CPlayerQueue
makeCPlayerQueue pq sigs = CPlayerQueue cp (pq^.clientDealer)
    where cp = makeCPlayers (pq^.clientPlayers) sigs

makeCPlayers :: [ClientPlayer] -> [PSignals] -> [CPlayer]
makeCPlayers = zipWith makeCPlayer

makeCPlayer :: ClientPlayer -> PSignals -> CPlayer
makeCPlayer p
    = CPlayer (p^.clientName) (p^.clientUUID) (p^.clientChips) (p^.clientCards)
              (p^.clientInPlay) (p^.clientAllIn) (p^.clientBet)
              (p^.clientMadeInitialBet) (p^.clientHandValue) 
              (p^.clientCanReRaise) (p^.clientIsMe)

makeCCards :: [Card] -> CSignals -> CCards
makeCCards = CCards
