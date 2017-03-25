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
  status :: TimerStatus,
  duration :: Int, -- in seconds
  started :: UTCTime,
  refreshThread :: MVar ThreadId
}

data TimerStatus = Running | Paused | Terminated

data Timer = Timer (MVar TimerState)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  timer <- newTimer
  startDBus timer
  putStrLn "Pb"
  waitForever

newTimer :: IO Timer
newTimer = do
  thread <- newEmptyMVar
  state <- newMVar (TimerState Terminated 0 undefined thread)
  return $ Timer state

startTimer :: Timer -> Int -> IO ()
startTimer timer@(Timer mvarState) dur = do
  state <- takeMVar mvarState
  tryTakeMVar (refreshThread state) >>= tryKillThread
  rtID <- forkIO $ timerRefreshThread timer
  putMVar (refreshThread state) rtID
  now <- getCurrentTime
  putMVar mvarState $ TimerState Running (fromIntegral dur) now (refreshThread state)
  where tryKillThread (Just threadId) = killThread threadId
        tryKillThread Nothing = return ()

pauseTimer :: Timer -> IO ()
pauseTimer (Timer mvarState) = do
  state <- takeMVar mvarState
  case (status state) of
    Running -> do
      now <- getCurrentTime
      takeMVar (refreshThread state) >>= killThread
      let remaining = calculateRemaining now state
      let newState = state { status = Paused, duration = remaining }
      putStrLn $ prettify remaining (status newState)
      putMVar mvarState newState
    _ -> putMVar mvarState state

resumeTimer :: Timer -> IO ()
resumeTimer timer@(Timer mvarState) = do
  state <- readMVar mvarState
  case (status state) of
    Paused -> startTimer timer (duration state)
    _      -> return ()

terminateTimer :: Timer -> IO ()
terminateTimer (Timer mvarState) = do
  state <- takeMVar mvarState
  putMVar mvarState $ state { status = Terminated }
  putStrLn "<fc=white,darkred> 0 </fc>"
  -- TODO make it "blink"

timerRefreshThread :: Timer -> IO ()
timerRefreshThread timer@(Timer mvarState) = do
  state <- readMVar mvarState
  now <- getCurrentTime
  let durDiff = calculateRemaining now state
  if durDiff <= 0
    then terminateTimer timer
    else do putStrLn $ prettify durDiff (status state)
            threadDelay $ delay durDiff
            timerRefreshThread timer
            where delay durDiff
                    | durDiff <= 60 = 1000000
                    | otherwise     = ((durDiff `rem` 60) + 1) * 1000000

prettify :: Int -> TimerStatus -> String
prettify x s = "<fc=" ++ (color s) ++ ">" ++ (printf "%02d" number) ++ "</fc>" where 
  number :: Int
  number
    | x >= 60   = floor (fromIntegral x / 60)
    | otherwise = x
  color Paused = "#4682B4" -- light blue
  color _
    | x >= 60   = "green"
    | otherwise = "orange"

waitForever :: IO ()
waitForever = forever $ threadDelay maxBound

calculateRemaining :: UTCTime -> TimerState -> Int
calculateRemaining time state = (duration state) - round (diffUTCTime time (started state))

startDBus :: Timer -> IO ()
startDBus timer@(Timer mvarState) = do
  client <- connectSession
  _ <- requestName client "org.pomobar" []
  export client "/org/pomobar"
    [
      autoMethod "org.Pomobar" "startTimer" dbusStart,
      autoMethod "org.Pomobar" "pauseResumeTimer" dbusPauseResume
    ]
  where dbusStart :: Int16 -> IO ()
        dbusStart durationMin = startTimer timer $ fromIntegral durationMin * 60
        dbusPauseResume = do
          state <- readMVar mvarState
          case (status state) of
            Running -> pauseTimer timer
            Paused  -> resumeTimer timer
            _       -> return ()
