module GameState where
import Ngram
import System.Random

type Score = Int

data Hand = One | Two deriving (Eq, Ord, Enum, Bounded)

data GameType = CPU (NGram Hand) | TwoPlayer

data Parity = Odd | Even deriving (Show, Enum, Eq)

data Settings = Settings { gameType :: GameType, playerParity :: Parity }

data GameState = GameState { settings :: Settings, p1Score  :: Score, p2Score  :: Score }

instance Random Parity where
  randomR (a, b) g = let (val, g') = randomR (fromEnum a, fromEnum b) g 
                      in (toEnum val, g')
  random = randomR (Odd, Even)

instance Show Hand where
  show One = "1"
  show Two = "2"

---------- Utilities ---------- 
isTwoPlayer :: GameType -> Bool
isTwoPlayer TwoPlayer = True
isTwoPlayer (CPU _) = False

oppositeParity :: Parity -> Parity
oppositeParity Odd = Even
oppositeParity Even = Odd

toInt :: Hand -> Integer
toInt One = 1
toInt Two = 2

toHand :: Integer -> Maybe Hand
toHand 1 = Just One
toHand 2 = Just Two
toHand _ = Nothing

sumHands :: Hand -> Hand -> Parity
sumHands p1Hand p2Hand = if even (toInt p1Hand + toInt p2Hand) then Even else Odd

player1Wins :: Settings -> Parity -> Bool
player1Wins s parity = playerParity s == parity

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

