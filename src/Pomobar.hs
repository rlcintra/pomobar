{-# LANGUAGE OverloadedStrings #-}

module Pomobar where

import Control.Concurrent
import Control.Monad
import Data.Int (Int16)
import Data.Time.Clock
import DBus.Client
import Text.Printf (printf)
import System.IO (hSetBuffering, stdout, BufferMode(..))

data TimerState = TimerState {
  status        :: TimerStatus,
  duration      :: Int, -- in seconds
  started       :: UTCTime,
  refreshThread :: MVar ThreadId
}

data TimerStatus = Running | Paused | Terminated deriving Eq

data Timer = Timer (MVar TimerState) ColourConfig

type Colour = String

data ColourConfig = ColourConfig {
  runningFgColour     :: Maybe Colour,
  pausedFgColour      :: Maybe Colour,
  terminatingFgColour :: Maybe Colour,
  terminatedFgColour  :: Maybe Colour,
  terminatedBg1Colour :: Maybe Colour,
  terminatedBg2Colour :: Maybe Colour,
  terminatedBgDelay   :: Maybe Int

}

defaultColorConfig :: ColourConfig
defaultColorConfig = ColourConfig
                       (Just "green")
                       (Just "#4682B4")
                       (Just "orange")
                       (Just "white")
                       (Just "darkred")
                       Nothing -- (Just "yellow")
                       (Just 500000)         -- 0.5 seconds

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  timer <- newTimer defaultColorConfig
  startDBus timer
  putStrLn "Pb"
  waitForever

newTimer :: ColourConfig -> IO Timer
newTimer colourConfig = do
  thread <- newEmptyMVar
  now <- getCurrentTime
  state <- newMVar (TimerState Terminated 0 now thread)
  return $ Timer state colourConfig

startTimer :: Timer -> Int -> IO ()
startTimer timer@(Timer mvarState _) dur = do
  state <- takeMVar mvarState
  tryTakeMVar (refreshThread state) >>= tryKillThread
  rtID <- forkIO $ timerRefreshThread timer
  putMVar (refreshThread state) rtID
  now <- getCurrentTime
  putMVar mvarState $ TimerState Running (fromIntegral dur) now (refreshThread state)
  where tryKillThread (Just threadId) = killThread threadId
        tryKillThread Nothing = return ()

pauseTimer :: Timer -> IO ()
pauseTimer (Timer mvarState colourConfig) = do
  state <- takeMVar mvarState
  case (status state) of
    Running -> do
      now <- getCurrentTime
      takeMVar (refreshThread state) >>= killThread
      let remaining = calculateRemaining now state
      let newState = state { status = Paused, duration = remaining }
      putStrLn $ formatOutput remaining (status newState) colourConfig
      putMVar mvarState newState
    _ -> putMVar mvarState state

-- |Add minutes to the timer.
timerAdd :: Timer -> Int -> IO ()
timerAdd timer@(Timer mvarState colourConfig) x = do
  state <- takeMVar mvarState
  now <- getCurrentTime
  let diffSec = x * 60
  let remaining = calculateRemaining now state
  let newRemaining = remaining + diffSec
  if newRemaining < 0 && ((status state) /= Terminated)
    then putMVar mvarState state
    else do
      let newState = state { duration = (duration state) + diffSec }
      case (status newState) of
        Running    -> do
          takeMVar (refreshThread newState) >>= killThread
          rtID <- forkIO $ timerRefreshThread timer
          putMVar (refreshThread newState) rtID
          putMVar mvarState newState
        Paused     -> do
          putStrLn $ formatOutput newRemaining Paused colourConfig
          putMVar mvarState newState
        Terminated -> do
          putMVar mvarState newState
          startTimer timer diffSec

resumeTimer :: Timer -> IO ()
resumeTimer timer@(Timer mvarState _) = do
  state <- readMVar mvarState
  case (status state) of
    Paused -> startTimer timer (duration state)
    _      -> return ()

terminateTimer :: Timer -> IO ()
terminateTimer (Timer mvarState colourConfig) = do
  state <- takeMVar mvarState
  putMVar mvarState $ state { status = Terminated }
  forM_ [0,(-1)..(-10)] blink
  where blink x = do
          putStrLn $ formatOutput x Terminated colourConfig
          threadDelay $ delay $ terminatedBgDelay colourConfig
        delay Nothing  = 0
        delay (Just x) = x

timerRefreshThread :: Timer -> IO ()
timerRefreshThread timer@(Timer mvarState colourConfig) = do
  state <- readMVar mvarState
  now <- getCurrentTime
  let durDiff = calculateRemaining now state
  if durDiff <= 0
    then terminateTimer timer
    else do putStrLn $ formatOutput durDiff (status state) colourConfig
            threadDelay $ delay durDiff
            timerRefreshThread timer
            where delay durDiff
                    | durDiff <= 60 = 1000000
                    | otherwise     = ((durDiff `rem` 60) + 1) * 1000000

formatOutput :: Int -> TimerStatus -> ColourConfig -> String
formatOutput x s c = xmobarString (printf "%02d" number) (colour s) (bgColour s) where
  number :: Int
  number
    | x >= 60   = floor (fromIntegral x / 60)
    | x < 0     = 0
    | otherwise = x
  colour Paused = pausedFgColour c
  colour Running
    | x >= 60   = runningFgColour c
    | otherwise = terminatingFgColour c
  colour Terminated = terminatedFgColour c
  bgColour Terminated = if x `rem` 2 == 0 then terminatedBg1Colour c else terminatedBg2Colour c
  bgColour _          = Nothing

waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

calculateRemaining :: UTCTime -> TimerState -> Int
calculateRemaining time state = (duration state) - round (diffUTCTime time (started state))

startDBus :: Timer -> IO ()
startDBus timer@(Timer mvarState _) = do
  client <- connectSession
  _ <- requestName client "org.pomobar" []
  timerSwitchState <- newMVar 0
  export client "/org/pomobar"
    [
      autoMethod "org.Pomobar" "startTimer" dbusStart,
      autoMethod "org.Pomobar" "pauseResumeTimer" dbusPauseResume,
      autoMethod "org.Pomobar" "timerAddMin" dbusTimerAdd,
      autoMethod "org.Pomobar" "startTimerSwitch" (dbusStartTimerSwitch timerSwitchState)
    ]
  where dbusStart :: Int16 -> IO ()
        dbusStart durationMin = startTimer timer $ fromIntegral durationMin * 60
        dbusPauseResume = do
          state <- readMVar mvarState
          case (status state) of
            Running -> pauseTimer timer
            Paused  -> resumeTimer timer
            _       -> return ()
        dbusTimerAdd :: Int16 -> IO()
        dbusTimerAdd = timerAdd timer . fromIntegral
        dbusStartTimerSwitch :: MVar Int -> [Int16] -> IO ()
        dbusStartTimerSwitch switchState xs = do
          now <- getCurrentTime
          state <- readMVar mvarState
          i <- if (diffUTCTime now (started state)) > 1.0
                 then swapMVar switchState 0 >> return 0
                 else modifyMVar switchState (\x -> return (x+1,x+1))
          startTimer timer $ (60 *) $ fromIntegral $ (cycle xs) !! i

xmobarString :: String -> Maybe String -> Maybe String -> String
xmobarString s Nothing _ = s
xmobarString s (Just fg) bg = "<fc=" ++ fg ++ stringBg bg ++ ">" ++ s ++ "</fc>"
  where stringBg Nothing  = ""
        stringBg (Just c) = "," ++ c
