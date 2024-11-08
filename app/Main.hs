module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import System.Random
import Ngram
import System.IO

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
sumHands p1Hand p2Hand = if even (toInt p1Hand + toInt p2Hand) then Even else Odd

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
getP2Throw Settings { gameType = CPU ng, playerParity = parity }= 
  return $ case (predictNext ng, parity) of 
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

showScore :: (Show a1, Show a2) => a1 -> a2 -> IO ()
showScore s1 s2 = putStrLn $ "\ncurrent scores: P1: " ++ show s1 ++ " P2:  " ++ show s2

updateScore :: Game Hand
updateScore = do
  gs@GameState{ settings = sets, p1Score = s1, p2Score = s2 } <- get

  (p1Hand, parity) <- liftIO $ throwHands sets

  let p1Win = playerParity sets == parity 

  liftIO . putStrLn $ if p1Win then "\nPlayer 1 wins!" else "\nPlayer 2 wins!"

  put $ if p1Win then gs { p1Score = s1 + 1 } else gs { p2Score = s2 + 1 }

  return p1Hand
  
updateNGram :: Hand -> Game ()
updateNGram p1Hand = do 
  gs@GameState{ settings = sets@Settings { gameType = gametype } } <- get 

  put $ case gametype of  
        TwoPlayer -> gs
        CPU ng -> gs { settings = sets { gameType = CPU (update p1Hand ng) } }

runGame :: Game ()
runGame = do
  GameState { p1Score = s1, p2Score = s2 } <- get

  liftIO $ showScore s1 s2

  updateScore >>= updateNGram >> runGame

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  setupGame >>= runStateT runGame >> return ()
