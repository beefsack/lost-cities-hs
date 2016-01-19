module LostCities
    ( Game
    ) where

import           Control.Monad.Random.Class (MonadRandom)
import           Control.Monad.Trans.Except as Ex
import           System.Random.Shuffle      (shuffleM)

data Rank = X | Value Int deriving Show
data Suit = Yellow | Blue | White | Green | Red deriving (Show, Enum)
type Card = (Suit, Rank)
type Deck = [Card]

numXPerSuit :: Int
numXPerSuit = 3
minRankValue :: Int
minRankValue = 2
maxRankValue :: Int
maxRankValue = 10

cardRanks :: [Rank]
cardRanks = (take numXPerSuit $ repeat X) ++ map Value [minRankValue..maxRankValue]

suitCards :: Suit -> Deck
suitCards suit = map (\rank -> (suit, rank)) cardRanks

initialDeck :: Deck
initialDeck = concat $ map suitCards [Yellow ..]

data Game = Game { rnd     :: Int
                 , players :: [String]
                 , deck    :: Deck
                 } deriving Show

numRnds :: Int
numRnds = 3

new :: MonadRandom m => [String] -> Ex.ExceptT String m Game
new ps@[_, _] = do
    shuffledDeck <- shuffleM initialDeck
    return Game { players = ps
                , rnd = 1
                , deck = shuffledDeck
                }
    Ex.except
new _ = Ex.throwE "Lost Cities requires two players"

nextRound :: Game -> Either String Game
nextRound g
    | rnd g < numRnds = Right g { rnd = (rnd g + 1) }
    | otherwise       = Left "It is the end of the game already"
