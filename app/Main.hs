module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy
import System.Random

type Score = Int

data GameType = CPU | TwoPlayer deriving (Show, Enum)

data Parity = Odd | Even deriving (Show, Enum, Eq)

data Hand = One | Two deriving (Enum)

instance Show Hand where
  show One = "1"
  show Two = "2"

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
    '1' -> return CPU
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

getP2Throw :: GameType -> IO Hand
getP2Throw TwoPlayer = getHumanThrow
getP2Throw CPU = randomOfTwo One Two

throwHands :: GameType -> IO Parity
throwHands gametype = do
  putStrLn "\nChoose either 1 or 2 fingers." 

  putStr "P1: "
  p1Throw <- getHumanThrow 

  p2Throw <- getP2Throw gametype
  putStr $ "\nP2: " ++ show p2Throw

  return $ sumHands p1Throw p2Throw

runGame :: Game ()
runGame = do
  game@(GameState { settings = Settings { gameType = type', 
                                          playerParity = pParity }, 
                    p1Score = s1, 
                    p2Score = p2 }) <- get

  liftIO $ putStrLn $ "\ncurrent scores: P1: " ++ show s1 ++ " P2:  " ++ show p2
  parity <- liftIO $ throwHands type'

  let newGameState = 
        if pParity == parity 
        then game { p1Score = s1 + 1 }
        else game { p2Score = p2 + 1 }

  put newGameState

  runGame

main :: IO ()
main = setupGame >>= runStateT runGame >> return ()
