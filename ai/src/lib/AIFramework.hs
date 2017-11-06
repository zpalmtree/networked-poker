module AIFramework
(
    runAI
)
where

import Control.Monad.Trans.State (evalStateT)

import ClientFramework (establishConnection, ioLoop)
import AITypes (AIGameStateT)

import HandleMessage 
    (handleAction, handleNewCards, handleMyCards, handleNewChips,
     handlePlayersRemoved, handleBadInput, handleGatherChips, handleResetRound,
     handleNextState, handleMinRaise, handleGameOver)

import Types 
    (Message(..), Action(..), CardMsg(..), DealtCardsMsg(..), NewChipsMsg(..),
     PlayersRemovedMsg(..), InputMsg(..), MinRaiseMsg(..))

runAI :: String -> ([Action Int] -> AIGameStateT (Maybe (Action Int))) -> IO ()
runAI name handleFunc = do
    maybeGame <- establishConnection name
    case maybeGame of
        Left err -> error $
            "Couldn't connect to server. Did you start it?\n" ++ show err

        Right (game, socket) -> 
            evalStateT (ioLoop socket (filterUnneeded handleFunc)) game

filterUnneeded :: ([Action Int] -> AIGameStateT (Maybe (Action Int))) 
                 -> Message -> AIGameStateT (Maybe (Action Int))
filterUnneeded aiFunction msg = case msg of
    MIsInput (InputMsg m) -> aiFunction m

    MIsAction m -> def $ handleAction m
    MIsCard (CardMsg m) -> def $ handleNewCards m
    MIsDealt (DealtCardsMsg m) -> def $ handleMyCards m
    MIsNewChips (NewChipsMsg m) -> def $ handleNewChips m
    MIsPlayersRemoved (PlayersRemovedMsg m) -> def $ handlePlayersRemoved m
    MIsBadInput -> def handleBadInput
    MIsGatherChips -> def handleGatherChips
    MIsResetRound -> def handleResetRound
    MIsNextState -> def handleNextState
    MIsMinRaise (MinRaiseMsg m) -> def $ handleMinRaise m
    MIsGameOver -> def handleGameOver

    MIsPlayerTurn _ -> noop
    MIsCardReveal _ -> noop
    MIsTextMsg _ -> noop

    MIsInitialGame _ -> error "Unexpected initialGame in filterUnneeded!"

    where def f = f >> return Nothing
          noop = return Nothing
