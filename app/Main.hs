module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy ( evalStateT, get, modify, put, StateT )
import System.Random ( randomRIO )
import Ngram ( NGram, ngram, update, predictNext )
import System.IO ( hSetBuffering, hSetEcho, stdin, stdout, BufferMode(NoBuffering) )
import Control.Monad (when)

type Score = Int

data Hand = One | Two deriving (Eq, Ord, Enum, Bounded)

data GameType = CPU (NGram Hand) | TwoPlayer deriving (Show)

data Parity = Odd | Even deriving (Show, Enum, Eq)

data Settings = Settings { gameType :: GameType, playerParity :: Parity }

data GameState = GameState { settings :: Settings, p1Score  :: Score, p2Score  :: Score }

type Game = StateT GameState IO

instance Show Hand where
  show One = "1"
  show Two = "2"

---------- Utilities ---------- 
isTwoPlayer :: GameType -> Bool
isTwoPlayer TwoPlayer = True
isTwoPlayer (CPU _) = False

randomOfTwo :: a -> a -> IO a
randomOfTwo choice1 choice2 = do
  choice <- randomRIO (0 :: Int, 1)
  pure $ if choice == 0 then choice1 else choice2

oppositeParity :: Parity -> Parity
oppositeParity Odd = Even
oppositeParity Even = Odd

toInt :: Hand -> Integer
toInt One = 1
toInt Two = 2

sumHands :: Hand -> Hand -> Parity
sumHands p1Hand p2Hand = if even (toInt p1Hand + toInt p2Hand) then Even else Odd

---------- Set Up ---------- 
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
  putStrLn $ "\nPlayer 1 is " ++ show parity ++ " and player 2 is " ++ show (oppositeParity parity)
  pure Settings { gameType = gameMode, playerParity = parity }

setupGame :: IO GameState
setupGame = do
  settings' <- setupSettings
  pure GameState { settings = settings', p1Score = 0, p2Score = 0 }

---------- Input ---------- 
getHumanThrow :: IO Hand
getHumanThrow = do 
  x <- read <$> fmap (:[]) getChar 
  case (x :: Int) of 
    1 -> return One
    2 -> return Two
    _ -> putStrLn "\nPlease choose either 1 or 2 fingers." >> getHumanThrow

getP2Throw :: Settings -> IO Hand
getP2Throw Settings { gameType = TwoPlayer } = getHumanThrow 
getP2Throw Settings { gameType = CPU ng, playerParity = parity } = pure $
  case (predictNext ng, parity) of 
   (One, Odd) -> One
   (One, Even) -> Two
   (Two, Odd) -> Two
   (Two, Even) -> One

---------- Core ---------- 
throwHands :: Settings -> IO (Hand, Parity)
throwHands sets = do
  hSetEcho stdin False
  putStr "Player 1 - Enter either 1 or 2: "
  p1Throw <- getHumanThrow 

  when (isTwoPlayer $ gameType sets) $ 
    putStr "\nPlayer 2 - Enter either 1 or 2: "

  p2Throw <- getP2Throw sets
  putStrLn $ "\nP1: " ++ show p1Throw ++ "\nP2: " ++ show p2Throw
  hSetEcho stdin True

  return (p1Throw, sumHands p1Throw p2Throw)

showScore :: Score -> Score -> IO ()
showScore s1 s2 = putStrLn $ "\ncurrent scores: P1: " ++ show s1 ++ " P2:  " ++ show s2

updateScore :: Game Hand
updateScore = do
  gs@GameState{ settings = sets, p1Score = s1, p2Score = s2 } <- get
  (p1Hand, parity) <- liftIO $ throwHands sets

  let p1Win = playerParity sets == parity 
  liftIO . putStrLn $ if p1Win then "\nPlayer 1 wins!" else "\nPlayer 2 wins!"
  put $ if p1Win then gs { p1Score = s1 + 1 } else gs { p2Score = s2 + 1 }

  pure p1Hand
  
updateNGram :: Hand -> Game ()
updateNGram p1Hand = modify $ \gs -> case settings gs of  
  Settings { gameType = CPU ng } ->
    gs { settings = (settings gs) { gameType = CPU (update p1Hand ng) } }
  _ -> gs

runGame :: Game ()
runGame = do
  GameState { p1Score = s1, p2Score = s2 } <- get
  liftIO $ showScore s1 s2
  updateScore >>= updateNGram >> runGame

main :: IO ()
main = do hSetBuffering stdout NoBuffering 
          hSetBuffering stdin  NoBuffering 
          setupGame >>= evalStateT runGame 
