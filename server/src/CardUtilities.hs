module CardUtilities
(
    takeCard,
    hearts,
    clubs,
    diamond,
    spade,
    fullDeck
)
where

import System.Random
import Types

takeCard :: [Card] -> IO ([Card], Card)
takeCard [] = error "Can't take a card from empty deck"
takeCard deck' = do
   cardNum <- getStdRandom (randomR (0, length deck' - 1)) 
   let card = deck' !! cardNum
   let fixedDeck = deleteNth (cardNum+1) deck'
   return (fixedDeck, card)

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
