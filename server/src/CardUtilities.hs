module CardUtilities
(
    drawCard,
    hearts,
    clubs,
    diamond,
    spade,
    fullDeck,
    revealFlop,
    revealTurn,
    revealRiver
)
where

import System.Random
import Control.Lens
import Types

getRandomCard :: [Card] -> IO ([Card], Card)
getRandomCard [] = error "Can't take a card from empty deck"
getRandomCard deck' = do
   cardNum <- getStdRandom (randomR (0, length deck' - 1)) 
   let card = deck' !! cardNum
   let fixedDeck = deleteNth (cardNum+1) deck'
   return (fixedDeck, card)

drawCard :: Game -> IO Game
drawCard game = do
    (newDeck, card) <- getRandomCard (game^.cardInfo.deck)
    return $ game & cardInfo.deck .~ newDeck
                  & cardInfo.tableCards .~ addCard tableCards' card
    where tableCards' = game^.cardInfo.tableCards

addCard :: Maybe [Card] -> Card -> Maybe [Card]
addCard Nothing card = Just [card]
addCard tableCards' card = fmap (++ [card]) tableCards'

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
fullDeck = [Card value' suit' | value' <- [minBound :: Value .. maxBound],
                                suit' <- [minBound :: Suit .. maxBound]]

hearts :: [Card]
hearts = [Card value' Heart | value' <- [minBound :: Value .. maxBound]]

clubs :: [Card]
clubs = [Card value' Club | value' <- [minBound :: Value .. maxBound]]

diamond :: [Card]
diamond = [Card value' Diamond | value' <- [minBound :: Value .. maxBound]]

spade :: [Card]
spade = [Card value' Spade | value' <- [minBound :: Value .. maxBound]]
