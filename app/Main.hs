module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy ( evalStateT, get, modify, put, StateT )
import Ngram ( NGram, ngram, predictNext )
import System.IO ( hSetBuffering, hSetEcho, stdin, stdout, BufferMode(NoBuffering) )
import Control.Monad (when)
import Text.Read (readMaybe)
import GameState
import System.Random (randomRIO)

type Game = StateT GameState IO

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
  parity <- randomRIO (Odd, Even)
  putStrLn (showParity parity)
  pure Settings { gameType = gameMode, playerParity = parity }

setupGame :: IO GameState
setupGame = initialState <$> setupSettings

---------- IO ---------- 
setBuffering :: IO ()
setBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering

withNoEcho :: IO a -> IO a
withNoEcho action = do
  hSetEcho stdin False
  result <- action
  hSetEcho stdin True
  return result

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
throwHands s@Settings {gameType = gt} = do
  p1Throw <- withNoEcho $ do
    putStr "Player 1 - Enter either 1 or 2: "
    getHumanThrow

  p2Throw <- withNoEcho $ do
    when (isTwoPlayer gt) $ putStr "\nPlayer 2 - Enter either 1 or 2: "
    getP2Throw s

  putStrLn $ "\nP1: " ++ show p1Throw ++ "\nP2: " ++ show p2Throw
  return (p1Throw, sumHands p1Throw p2Throw)

updateScore :: Game Hand
updateScore = do
  gs@GameState{ settings = s } <- get
  (p1Hand, parity) <- liftIO $ throwHands s
  let p1Win = playerParity s == parity
  liftPrint $ congratulate p1Win
  put (incScore gs p1Win)
  pure p1Hand

updateNGram :: Hand -> Game ()
updateNGram p1Hand = modify $ \gs@GameState { settings = s@Settings{gameType = gt} } -> 
  gs { settings = s { gameType = updateGameType p1Hand gt } }

runGame :: Game ()
runGame = do
  GameState { p1Score = s1, p2Score = s2 } <- get
  liftPrint $ showScore s1 s2
  updateScore >>= updateNGram >> runGame

main :: IO ()
main = setBuffering >> setupGame >>= evalStateT runGame
