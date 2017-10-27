module TestDrawCard
(
    testKnuth,
    testRandomIndex
)
where

import Types (Card)
import Utilities.Card (fullDeck)
import DrawCard (Drawable, KnuthShuffle, RandomIndex, initDeck, draw)

testKnuth :: IO [Card]
testKnuth = do
    original <- initDeck :: IO KnuthShuffle
    genCard original $ length fullDeck

testRandomIndex :: IO [Card]
testRandomIndex = do
    original <- initDeck :: IO RandomIndex
    genCard original $ length fullDeck

genCard :: (Drawable a) => a -> Int -> IO [Card]
genCard _ 0 = return []
genCard a n = do
    (new, card) <- draw a
    cards <- genCard new (n-1)
    return $ card : cards
