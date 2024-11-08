module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import System.Random
import Ngram

type Score = Int

data Hand = One | Two deriving (Eq, Ord, Enum, Bounded)

instance Show Hand where
  show One = "1"
  show Two = "2"

data GameType = CPU (NGram Hand) | TwoPlayer deriving Show

data Parity = Odd | Even deriving (Show, Enum, Eq)

data Settings = Settings { gameType :: GameType, playerParity :: Parity }

data GameState = GameState { settings :: Settings
                           , p1Score  :: Score
                           , p2Score  :: Score }

type Game = StateT GameState IO

randomOfTwo :: a -> a -> IO a
randomOfTwo choice1 choice2 = do
  choice <- randomRIO (0 :: Int, 1)
  if choice == 0 then return choice1 else return choice2

oppositeParity :: Parity -> Parity
oppositeParity Odd = Even
oppositeParity Even = Odd

toInt :: Hand -> Integer
toInt One = 1
toInt Two = 2

sumHands :: Hand -> Hand -> Parity
sumHands p1Hand p2Hand =
  if even (toInt p1Hand + toInt p2Hand) then Even else Odd

askGameMode :: IO GameType
askGameMode = do
  putStrLn "1. Computer Game\n2. Human"
  x <- getChar
  case x of 
    '1' -> return (CPU $ ngram 2 3)
    '2' -> return TwoPlayer
    _ -> putStrLn "Choose 1 or 2." >> askGameMode
  
setupSettings :: IO Settings
setupSettings = do
  gameMode <- askGameMode
  parity <- randomOfTwo Odd Even
  putStrLn $ "\nPlayer 1 is " ++ show parity 
          ++ " and player 2 is " ++ show (oppositeParity parity)

  return Settings { gameType = gameMode, playerParity = parity }

setupGame :: IO GameState
setupGame = do
  settings' <- setupSettings
  return GameState { settings = settings', p1Score = 0, p2Score = 0 }

getHumanThrow :: IO Hand
getHumanThrow = do 
  x <- read <$> fmap (:[]) getChar 
  case (x :: Int) of 
    1 -> return One
    2 -> return Two
    _ -> putStrLn "\nPlease choose either 1 or 2 fingers." >> getHumanThrow

getP2Throw :: Settings -> IO Hand
getP2Throw Settings { gameType = TwoPlayer } = getHumanThrow
getP2Throw Settings { gameType = CPU ng, playerParity = parity }= do
  let next' = predictNext ng 
  putStrLn $ "\nPredicted: " ++ show next' ++ " with a chance of " ++ show (chanceOf next' ng)
  return $ case (next', parity) of 
         (One, Odd) -> One
         (One, Even) -> Two
         (Two, Odd) -> Two
         (Two, Even) -> One

throwHands :: Settings -> IO (Hand, Parity)
throwHands sets = do
  putStrLn "\nChoose either 1 or 2 fingers." 

  putStr "P1: "
  p1Throw <- getHumanThrow 

  p2Throw <- getP2Throw sets
  putStr $ "\nP2: " ++ show p2Throw

  return (p1Throw, sumHands p1Throw p2Throw)

updateScore :: Game Hand
updateScore = do
  gs@GameState{ settings = sets, p1Score = s1, p2Score = s2 } <- get

  (hand, parity) <- liftIO $ throwHands sets

  let newGs = 
        if playerParity sets == parity 
        then gs { p1Score = s1 + 1 }
        else gs { p2Score = s2 + 1 }

  put newGs

  return hand
  
updateNGram :: Hand -> Game ()
updateNGram hand = do 
  gs@GameState{ settings = sets@Settings { gameType = gametype } } <- get 

  case gametype of  
    TwoPlayer -> put gs
    CPU ng -> do
      let ng' = update hand ng
      put gs { settings = sets { gameType = CPU ng' } }

runGame :: Game ()
runGame = do
  GameState { p1Score = s1, p2Score = p2 } <- get
  liftIO $ putStrLn $ "\ncurrent scores: P1: " ++ show s1 ++ " P2:  " ++ show p2
  updateScore >>= updateNGram >> runGame

main :: IO ()
main = setupGame >>= runStateT runGame >> return ()
