module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy ( evalStateT, get, modify, put, StateT )
import System.Random ( randomRIO )
import Ngram ( NGram, ngram, update, predictNext )
import System.IO ( hSetBuffering, hSetEcho, stdin, stdout, BufferMode(NoBuffering) )
import Control.Monad (when)
import Text.Read (readMaybe)

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

initialState :: Settings -> GameState
initialState sets = GameState { settings = sets, p1Score = 0, p2Score = 0 }

updateGameType :: Hand -> GameType -> GameType
updateGameType p1Hand (CPU ng) = CPU (update p1Hand ng)
updateGameType _ gt = gt

winningHand :: Hand -> Parity -> Hand
winningHand One Odd = One
winningHand One Even = Two
winningHand Two Odd = Two
winningHand Two Even = One

congratulate :: Bool -> String
congratulate p1Win = if p1Win then "\nPlayer 1 wins!" else "\nPlayer 2 wins!"

incScore :: GameState -> Bool -> GameState
incScore gs p1Win = if p1Win 
  then gs { p1Score = p1Score gs + 1 } 
  else gs { p2Score = p2Score gs + 1 }

showParity :: Parity -> String
showParity p = "\nPlayer 1 is " ++ show p ++ " and player 2 is " ++ show (oppositeParity p)

showScore :: Score -> Score -> String
showScore s1 s2 = "\ncurrent scores: P1: " ++ show s1 ++ " P2:  " ++ show s2

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
  putStrLn (showParity parity)
  pure Settings { gameType = gameMode, playerParity = parity }

setupGame :: IO GameState
setupGame = initialState <$> setupSettings

---------- Input ---------- 
liftPrint :: String -> Game ()
liftPrint = liftIO . putStrLn

getHumanThrow :: IO Hand
getHumanThrow = do
  x <- readMaybe <$> fmap pure getChar :: IO (Maybe Int)
  case x of
    Just 1 -> return One
    Just 2 -> return Two
    _ -> putStr "\nPlease choose either 1 or 2 fingers: " >> getHumanThrow

getCpuThrow :: NGram Hand -> Parity -> Hand
getCpuThrow ng = winningHand (predictNext ng) 

getP2Throw :: Settings -> IO Hand
getP2Throw Settings { gameType = TwoPlayer } = getHumanThrow
getP2Throw Settings { gameType = CPU ng, playerParity = p } = pure $ getCpuThrow ng p

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

updateScore :: Game Hand
updateScore = do
  gs@GameState{ settings = sets } <- get
  (p1Hand, parity) <- liftIO $ throwHands sets
  let p1Win = playerParity sets == parity
  liftPrint $ congratulate p1Win
  put (incScore gs p1Win)
  pure p1Hand

updateNGram :: Hand -> Game ()
updateNGram p1Hand = modify $ \gs@GameState { settings = sets } -> 
  gs { settings = sets { gameType = updateGameType p1Hand (gameType sets) } }

runGame :: Game ()
runGame = do
  GameState { p1Score = s1, p2Score = s2 } <- get
  liftPrint $ showScore s1 s2
  updateScore >>= updateNGram >> runGame

setBuffering :: IO ()
setBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

main :: IO ()
main = setBuffering >> setupGame >>= evalStateT runGame
