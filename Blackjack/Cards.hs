{- REPRESENTATION CONVENTION: Each dataconstructor represents a card from a deck of cards
   REPRESENTATION INVARIANT: ??  ... requirements on elements of the datatype that the code preserves at all times ...
 -}
module Cards where

import Control.Exception
import System.Random
import Control.Monad
import Data.Array.IO

data Rank = A1 |Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jk | Q | K | A  deriving (Show,Eq,Enum)

-- REPRESENTATION CONVENTION: Each dataconstructor represents one of the suits from a deck of cards --
data Suit = Diamond | Clove | Heart | Spade deriving (Show,Eq,Enum)
                       


cardsize :: Card -> Int 
cardsize (Card _ Jk)    = 10
cardsize (Card _ Q)     = 10
cardsize (Card _ K)     = 10
cardsize (Card _ A)     = 11 
cardsize a1@(Card _ r)  = (fromEnum r) +1 

data Card = Card Suit Rank deriving (Show, Eq)

type Deck = [Card]

type Hand = [Card]

type Bet = Double


deck :: Deck
deck = [(Card suit rank) | rank <- [(Two)..(A)], suit <- [(Diamond)..(Spade)]]

shuffledDeck :: IO(Deck)
shuffledDeck = shuff deck

dealCard :: Hand -> Deck -> (Hand,Deck)
dealCard hand deck = (((head deck):hand), tail deck)

firstDeal :: [a] -> [a] -> [a] -> [[a]]
firstDeal p x (a:b:c:d:f) = [(a:c:p), (d:b:x), f] 

valueHand :: Hand -> Int
valueHand h = foldl (\x y -> x + (cardsize  y)) 0 h


---function shuff is taken from https://wiki.haskell.org/Random_shuffle--

shuff :: Deck -> IO Deck
shuff xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

convertAce :: Hand -> Hand
convertAce ((a@(Card s A)):xs) = (Card s A1) :xs
convertAce xs                  = convertAce xs

checkAce :: Hand -> Bool 
checkAce [] = False  
checkAce ((Card _ A):xs) =  True  
checkAce((Card _ _):xs) = checkAce xs 

eqCards :: Hand -> Bool
eqCards (x:y:xs) = if cardsize x == cardsize y then True else False

printCard [] = []
printCard ((Card a b):xs) = (show a) ++ " " ++ (show b) ++ "  " ++ printCard xs

printPlayer :: Hand -> Hand -> IO()
printPlayer a (b:c) = do
  putStrLn $ "Your hand is " ++ (printCard a) ++ " Which is "++ (show (valueHand a))
  putStrLn $ "Dealers hand is " ++ (printCard [b]) ++" Which is " ++ (show (valueHand [b]))

printDealer :: Hand -> Hand -> IO()
printDealer a b = do
  putStrLn $ "Your hand is " ++ (printCard a) ++ " Which is "++ (show (valueHand a))
  putStrLn $ "Dealers hand is " ++ (printCard b) ++ "Which is " ++ (show (valueHand b))
  putStrLn ""

printSplit :: Hand -> Hand -> Hand -> IO()
printSplit a b c = do
  putStrLn $ "Your first hand is " ++ (printCard a) ++ " Which is "++ (show (valueHand a))
  putStrLn $ "Your second hand is " ++ (printCard b) ++ " Which is "++ (show (valueHand b))
  putStrLn $ "Dealers hand is " ++ (printCard c) ++ " Which is "++ (show (valueHand c))


blackJackDealer :: Hand -> Hand -> Bool
blackJackDealer player dealer
  |(length player) == 2 && (valueHand player) == (valueHand dealer) && (length dealer == 2) = False
  |(length player) == 2 = True

