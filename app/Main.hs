module Main where
import Control.Monad.Trans.State.Lazy (StateT)

type Score = Int

data GameType = CPU | TwoPlayer

data Parity = Odd | Even

data Hand = One | Two

data Settings = 
  Settings { 
    gameType :: GameType 
  , playerParity :: Parity 
  }

data GameState = 
  GameState { 
    settings :: Settings
  , p1Score :: Score  
  , p2Score :: Score
  , p1lastHand :: Hand
  , p2lastHand :: Hand
  } 

type Game = StateT GameState IO 


main :: IO ()
main = putStrLn "Hello, Haskell!"
