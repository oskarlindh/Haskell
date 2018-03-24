import Prelude hiding(catch)
import Test.HUnit
import Cards
import Text.Read
 

{--Main
PURPOSE: To start the program and give the player his/hers initial betting sum
PRE:True
POST:-
SIDE EFFECTS: running start with a double with the value of 100 
--}

main :: IO()
main = do
  putStrLn "Welcome To Blackjack"
  start 100.0


 {--playersTurn deck player dealer
   PURPOSE: Allowing the player to interact with the game and play the hand he has been dealt.
   PRE: Two non empty hands (the players and the dealers) and the a non empty deck.
   POST: The players hand when he has finnished playing and the deck without the cards (if any) the player has been dealt. 
   SIDE EFFECTS: Reads an input from the keyboard prints a string  and/or the players hand and the deck (see Post) --}

playersTurn :: Deck -> Hand -> Hand -> IO (Hand,Deck)
playersTurn deck player dealer = do
  putStrLn ""
  printPlayer player dealer  
  if valueHand player > 21 && checkAce player == True then playersTurn deck (convertAce player) dealer
   else if valueHand player > 21  then do putStrLn "Sorry You Got Over 21"
                                          return (player, deck)
   else if valueHand player == 21 then do putStrLn "Congratulations, You Got 21"
                                          return (player, deck)
   else do putStrLn "Hit or Stand"
           answear <- getLine
           if answear == "Hit" || answear == "hit" then playersTurn (snd (dealCard player deck)) (fst (dealCard player deck)) dealer
           else if answear == "Stand" || answear == "stand"  then return (player,deck)
           else do putStrLn "Invalid answear"
                   playersTurn deck player dealer

{--dealersTurn deck dealer
PURPOSE: Making the dealer draw cards untill the prerequirment is met
PRE:A non empty deck and a non empty hand
POST:Possibly a shorter deck with cards moved to dealer
EXAMPLES:[Card Diamond A,Card Heart Jk,Card Clove Eight,Card Heart A] [Card Clove Four,Card Diamond Six] =
         ([Card Diamond A,Card Clove Four,Card Diamond Six],[Card Heart Jk,Card Clove Eight,Card Heart A])
--}

dealersTurn :: Deck -> Hand -> (Hand,Deck)
dealersTurn deck dealer =
  if valueHand dealer > 21 && checkAce dealer == True then dealersTurn deck (convertAce dealer) 
   else if valueHand dealer > 16  then (dealer, deck)
   else dealersTurn (snd (dealCard dealer deck)) (fst (dealCard dealer deck)) 

{--singleHand Deck player dealer betMax bet
PURPOSE: Playing out a single hand
PRE:a non empty deck
POST: Runs functions depending on the dealers and players hands, prints a string (see Side Effects)
SIDE EFFECTS: Prints a strings depending on the players and dealers hands. Runs askQuit with a new betMax 
--}


singleHand :: Deck -> Hand -> Hand -> Bet -> Bet -> IO()
singleHand deck player dealer betMax bet = do
  playersHand <- playersTurn deck player dealer
  if valueHand (fst(playersHand)) >= 22 then do putStrLn "You busted"
                                                let betMax1 = (betMax - bet)
                                                askQuit (betMax1)
     else do let dealersHand = dealersTurn (snd(playersHand)) dealer
             if valueHand (fst(dealersHand)) > 21 
                then do putStrLn ""
                        printDealer (fst(playersHand)) (fst (dealersHand))
                        putStrLn "Dealer busted, Congratulations You Win"
                        let betMax1 = (betMax + bet)
                        askQuit (betMax1)
                else do putStrLn ""
                        printDealer (fst(playersHand)) (fst (dealersHand))
                        print (fst(winner (fst(playersHand)) (fst (dealersHand)) betMax bet))
                        askQuit (snd(winner (fst(playersHand)) (fst (dealersHand)) betMax bet))

winner :: Hand -> Hand -> Bet -> Bet -> (String,Bet)
winner player dealer betMax bet | valueHand dealer == 21 && length dealer == 2 =("They Dealer Got Blackjack. You Lost!", (betMax - bet))
                                | valueHand player > valueHand dealer = ("You Win", (betMax + bet)) 
                                | valueHand player < valueHand dealer = ("Sorry Dealer Wins ", (betMax - bet))
                                | valueHand player == valueHand dealer = ("Its a Tie", betMax) 

{--askQuit betMax
PURPOSE: To check if the player and if the player wants to continue playing and if the player 
         depending on the remaining betable amount can. 
PRE: True
POST: see side effects
SIDE EFFECTS: Reads an input from the keyboard and either terminates the program or runs start with betMax.
--}

askQuit :: Bet -> IO()
askQuit betMax = do
  putStrLn ""
  putStrLn $ "You Now Got " ++ (show betMax)
  putStrLn ""
  if betMax <= 0 then do putStrLn "You're Out Of Money, You Lost!"
                         return()
     else do putStrLn "Would You Like To Continue Playing? Yes/No?"
             go <-getAnswear
             if go == "Yes" || go == "yes" then start betMax
                else do putStrLn $ "You Stop Playing With " ++ (show betMax)
                        putStrLn "Thank You For Playing"
                        return()

{--getBet betMax
PURPOSE: To get the player to decide the amount he/she wishes to bet
PRE:betMax > 0
POST: a double bigger than 0 but not bigger than maxBet depending on the players input (se sideeffects)
SIDE EFFECTS:If Puts out a string and reads an input from the keyboard.
--}


getBet :: Bet -> IO(Bet)
getBet betMax = do
  putStrLn $ "You Got " ++ (show betMax)
  putStrLn "How Much Would You Like To Bet?"
  bet <- getNum :: IO (Maybe Bet)
  case bet of
    Nothing -> do
      putStrLn "Not A Number, Try Again"
      getBet betMax
    Just bet' -> do
         let bet1 = checkJust bet
                   where checkJust (Just a) = a 
         if bet1 > 0 && bet1 <= betMax then return (bet1)
            else do putStrLn "Invalid Bet"
                    getBet betMax

{--TODO fix type declaration--}
{--getNum :: (Num a, Read a) => IO (Maybe Double)--}
getNum = do
  num <- getLine
  let a = readMaybe num :: Maybe Double
  return (a)

{--doubleDown deck player betMax bet
PURPOSE:playing out a hand if double down was chosen
PRE:a non empty deck
POST: a printed string (see Side Effects), a new betMax 
SIDE EFFECTS: Prints a Strings depending on the players and dealers hands. Running askQuit with a new betMax
EXAMPLES:
--}

doubleDown :: Deck -> Hand -> Hand -> Bet -> Bet -> IO()
doubleDown cardDeck player dealer betMax bet  = do
   let hand = dealCard player cardDeck
   if valueHand (fst(hand)) >= 22 
      then do putStrLn "You busted"
              let betMax1 = (betMax - bet)
              askQuit (betMax1)
      else do let dealersHand = dealersTurn (snd(hand)) dealer
              if valueHand (fst(dealersHand)) > 21 
                 then do putStrLn ""
                         printDealer (fst(hand)) (fst (dealersHand))
                         putStrLn "Dealer busted, Congratulations You Win"
                         let betMax1 = (betMax + bet)
                         askQuit (betMax1)
                 else do putStrLn ""
                         printDealer (fst(hand)) (fst (dealersHand))
                         print (fst(winner (fst(hand)) (fst (dealersHand)) betMax bet))
                         askQuit (snd(winner (fst(hand)) (fst (dealersHand)) betMax bet))
  


{--start betMax
PURPOSE:To get the players bet, dealing put cards and  check if the player is able to split and if he/she would like to
PRE:betMax > 0
POST: Runs either play or playSplit with betMax
SIDE EFFECTS: Depending on the players hands puts out a string.
EXAMPLES:
--}

start :: Bet -> IO()
start betMax = do
  putStrLn ""
  bet <- getBet betMax
  cardDeck <- shuffledDeck
  let deal = firstDeal [] [] cardDeck in checkSplit (deal!!2) (deal!!0) (deal!!1) betMax bet
    where checkSplit deck player dealer betMax bet = 
               if eqCards player == True && betMax >= (2*bet) 
                  then do printPlayer player dealer
                          putStrLn "Would You Like To Split? Yes or No?"
                          answear <- getLine
                          if answear == "Yes" || answear == "yes" then playSplit deck (player!!0) (player!!1) dealer betMax bet
                             else do play deck player dealer betMax bet
                  else play deck player dealer betMax bet
{--
play cardDeck player dealer
PURPOSE: checking if the player got blackjack and asking if the player wants to play double down
PRE: Two non empty hands (the players and the dealers) and the a non empty deck.
POST: Runs functions depeding on the players hand or puts out a string (se Sideeffects) 
SIDE EFFECTS: Puts out a strings depending on the players and the dealers hands.
--}
play :: Deck -> Hand -> Hand -> Bet -> Bet -> IO()
play cardDeck player dealer betMax bet = 
  if (valueHand player) == 21 && blackJackDealer player dealer == True 
   then do printDealer player dealer
           putStrLn "Congratulations, You Got Blackjack"
           let betMax1 = (betMax + (1.5 * bet))
           askQuit (betMax1)
   else if (valueHand player) == 21 && blackJackDealer player dealer == False
     then do printDealer player dealer
             putStrLn "You Got Blackjack But So Did The Dealer, Bad Luck. It's A Tie"
             askQuit (betMax)
     else do putStrLn ""
             printPlayer player dealer
             putStrLn "Surrender? Yes/No"
             answear1 <- getAnswear
             if answear1 == "Yes" || answear1 == "yes" then do let betMax1 = (betMax - (0.5*bet))
                                                               putStrLn "Chicken"
                                                               askQuit (betMax1) 
             else if betMax >= (2*bet) 
                     then do putStrLn "Would You Like To Double Down, Yes Or No?"
                             answear <- getAnswear
                             if answear == "Yes" || answear == "yes" then doubleDown cardDeck player dealer betMax (2*bet)
                                else do singleHand cardDeck player dealer betMax bet
                     else singleHand cardDeck player dealer betMax bet

getAnswear :: IO(String)
getAnswear = do
  answear <- getLine
  if answear == "Yes" || answear == "yes" || answear == "no" || answear == "No" then return (answear)
     else do putStrLn "Invalid Answear" 
             getAnswear
{--
playSplit deck hand1 hand2 dealer 
PURPOSE: Playing blackjack with two player hands  
PRE: A non empty hand, a non empty deck
POST: Runs askQuit with betmax  and (se side effects) 
SIDE EFFECTS: Strings put out depending on the players and dealers hands 
EXAMPLES:
--}


playSplit :: Deck -> Card -> Card -> Hand -> Bet -> Bet -> IO()
playSplit deck hand1 hand2 dealer betMax bet = do
  printSplit [hand1] [hand2] dealer
  putStrLn ""
  let player1 = dealCard [hand1] deck
  let player2 = dealCard [hand2] (snd(player1))
  putStrLn "Play Your First Hand"
  playersHand1 <- playersTurn (snd (player2)) (fst (player1)) dealer 
  putStrLn "Play Your Second Hand"
  playersHand2 <- playersTurn (snd (playersHand1)) (fst (player2)) dealer
  if valueHand(fst playersHand1) >= 22 && valueHand(fst playersHand2) >= 22 
     then do putStrLn ""
             putStrLn "Both Your Hands Busted, You Lost"
             let betMax1 = betMax - 2*bet
             askQuit betMax1
     else do let dealersHand = dealersTurn (snd playersHand2) dealer
             putStrLn ""
             printSplit (fst playersHand1) (fst playersHand2) (fst dealersHand)
             putStrLn ""
             if valueHand dealer == 21 && length dealer == 2 then do putStrLn ""
                                                                     putStrLn "Sorry The Dealer Got Blackjack, You Lost"
                                                                     askQuit betMax
                else do let a = (splitWinner (fst dealersHand) (fst playersHand1) betMax bet)
                        putStrLn $ "Your First Hand " ++ (fst(a))
                        putStrLn ""
                        putStrLn $ "Your Second Hand " ++ (fst(splitWinner (fst dealersHand) (fst playersHand2) (snd(a)) bet))
                        askQuit (snd(splitWinner (fst dealersHand) (fst playersHand2) (snd(a)) bet))


splitWinner :: Hand -> Hand -> Bet -> Bet -> (String, Bet)
splitWinner dealer hand betMax bet
   |valueHand hand > 21 = ("Busted", (betMax - bet))
   |valueHand hand == valueHand dealer = ("Tied With The Dealer", betMax)
   |valueHand hand > valueHand dealer = ("Won Against The Dealer", (betMax + bet))
   |valueHand hand < valueHand dealer = ("Lost Against The Dealer", (betMax - bet))


-------------------------------------------------------------------------------------------------------------------------
-- Test Cases
-------------------------------------------------------------------------------------------------------------------------

testDeck = [Card Spade Two,Card Diamond Q,Card Heart Q,Card Spade Three,Card Diamond Five,Card Heart A,Card Spade Five,Card Heart Four,Card Clove Ten,Card Heart Three,Card Clove Q,Card Diamond A,Card Clove Jk,Card Clove Five,Card Heart Five,Card Clove Six,Card Clove Nine,Card Heart Seven,Card Heart Jk,Card Diamond Four,Card Clove Two,Card Spade Eight,Card Clove K,Card Diamond Seven,Card Spade Nine,Card Heart Eight,Card Clove A,Card Spade K,Card Diamond Nine,Card Heart Ten,Card Diamond Six,Card Clove Seven,Card Spade Jk,Card Spade Q,Card Spade A,Card Diamond Ten,Card Diamond Three,Card Clove Eight,Card Heart Two,Card Clove Three,Card Heart Six,Card Diamond Two,Card Clove Four,Card Heart Nine,Card Spade Ten,Card Diamond K,Card Heart K,Card Diamond Jk,Card Diamond Eight,Card Spade Six,Card Spade Four,Card Spade Seven]



test1 = TestCase $ do
          deck <- shuffledDeck
          assertEqual "ShuffledDeck" (380) (valueHand deck)

test2 = TestCase $ assertEqual "dealCard"
          ([Card Spade Two,Card Diamond Q,Card Heart Q],tail testDeck) (dealCard ([Card Diamond Q,Card Heart Q]) testDeck) 

test3 = TestCase $ assertEqual "valueHand"
          (17) (valueHand [Card Diamond Six,Card Diamond A])

test4 = TestCase $ assertEqual "convertAce"
           ([Card Diamond A1,Card Heart A]) (convertAce ([Card Diamond A,Card Heart A]))

test5 = TestCase $ assertEqual "checkAce"
           (True) (checkAce [Card Diamond A,Card Heart A])

test6 = TestCase $ assertEqual "blackJackDealer"
          (False) (blackJackDealer ([Card Diamond A,Card Clove K]) ([Card Heart A,Card Heart K]))

test7 = TestCase $ assertEqual "eqCards"
          (True) (eqCards ([Card Diamond K,Card Clove Ten])) 
                 
runTests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7]

{--
PURPOSE:
PRE:
POST: 
SIDE EFFECTS:
EXAMPLES:
--}






