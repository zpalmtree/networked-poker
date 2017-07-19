{-# LANGUAGE CPP #-}

module CardUtilities
(
    drawCard,
    hearts,
    clubs,
    diamonds,
    spades,
    fullDeck,
    revealFlop,
    revealTurn,
    revealRiver,
    getSevenCards,
    dealCards
)
where

import Types (Game, Player, Card(..), Value, Suit(..), State(..))
import Lenses (playerInfo, cards, numPlayers, players, cardInfo, deck,
               tableCards, state)
#ifdef DEBUG
import Output.Terminal.Output (outputPlayerCards)
#else
import Output.Network.Output (outputPlayerCards)
#endif
import System.Random (getStdRandom, randomR)
import Control.Lens

dealCards :: Game -> IO Game
dealCards game = do
    newGame <- dealCards' game [] 0
    outputPlayerCards newGame
    return newGame

dealCards' :: Game -> [Player] -> Int -> IO Game
dealCards' game newPlayers n
    | n == numPlayers' = return $ game & playerInfo.players .~ newPlayers
    | otherwise = do
        (newGame, newCards) <- drawPlayerCards game
        let newPlayer = player & cards .~ newCards
        dealCards' newGame (newPlayers ++ [newPlayer]) (n+1)
    where numPlayers' = game^.playerInfo.numPlayers
          player = game^.playerInfo.players ^?! ix n

getRandomCard :: [Card] -> IO ([Card], Card)
getRandomCard [] = error "Can't take a card from empty deck"
getRandomCard deck' = do
   cardNum <- getStdRandom $ randomR (0, length deck' - 1)
   let card = deck' !! cardNum
   let fixedDeck = deleteNth (cardNum + 1) deck'
   return (fixedDeck, card)

drawCard :: Game -> IO Game
drawCard game = do
    (newDeck, card) <- getRandomCard (game^.cardInfo.deck)
    return $ game & cardInfo.deck .~ newDeck
                  & cardInfo.tableCards .~ addCard tableCards' card
    where tableCards' = game^.cardInfo.tableCards

drawPlayerCards :: Game -> IO (Game, [Card])
drawPlayerCards game = do
    (newDeck, card1) <- getRandomCard (game^.cardInfo.deck)
    (newDeck', card2) <- getRandomCard newDeck
    return (game & cardInfo.deck .~ newDeck', [card1, card2])

addCard :: [Card] -> Card -> [Card]
addCard tableCards' card = tableCards' ++ [card]

revealFlop :: Game -> IO Game
revealFlop game = (& state .~ Flop) <$> 
                  (drawCard =<< drawCard =<< drawCard game)

revealTurn :: Game -> IO Game
revealTurn game = (& state .~ Turn) <$> drawCard game

revealRiver :: Game -> IO Game
revealRiver game = (& state .~ River) <$> drawCard game

deleteNth :: Int -> [a] -> [a]
deleteNth n xs
    | n < 0 = error "Can't remove negative index"
    | n == 0 = init xs
    | otherwise = take (n-1) xs ++ drop n xs

fullDeck :: [Card]
fullDeck = [Card value suit | value <- [minBound :: Value .. maxBound],
                              suit <- [minBound :: Suit .. maxBound]]

hearts :: [Card]
hearts = [Card value Heart | value <- [minBound :: Value .. maxBound]]

clubs :: [Card]
clubs = [Card value Club | value <- [minBound :: Value .. maxBound]]

diamonds :: [Card]
diamonds = [Card value Diamond | value <- [minBound :: Value .. maxBound]]

spades :: [Card]
spades = [Card value Spade | value <- [minBound :: Value .. maxBound]]

getSevenCards :: IO [Card]
getSevenCards = getSevenCards' 7 fullDeck

getSevenCards' :: Int -> [Card] -> IO [Card]
getSevenCards' 0 _ = return []
getSevenCards' n deck' = do
   (newDeck, card) <- getRandomCard deck'
   rest <- getSevenCards' (n-1) newDeck
   return $ card : rest
