module Main where

import Pomobar
import Options.Applicative
import Data.Semigroup ((<>))

parseColours :: Parser TimerConfig
parseColours = TimerConfig
             <$> option cReader (long "runningFgColour"
                                   <> (value $ runningFgColour defaultTimerConfig)
                                   <> help "FG running colour")
             <*> option cReader (long "pausedFgColour"
                                   <> (value $ pausedFgColour defaultTimerConfig)
                                   <> help "FG paused colour")
             <*> option cReader (long "terminatingFgColour"
                                   <> (value $ terminatingFgColour defaultTimerConfig)
                                   <> help "FG terminating colour")
             <*> option cReader (long "terminatedFg1Colour"
                                   <> (value $ terminatedFg1Colour defaultTimerConfig)
                                   <> help "FG1 terminated colour")
             <*> option cReader (long "terminatedBg1Colour"
                                   <> (value $ terminatedBg1Colour defaultTimerConfig)
                                   <> help "BG1 terminated colour")
             <*> option cReader (long "terminatedFg2Colour"
                                   <> (value $ terminatedFg2Colour defaultTimerConfig)
                                   <> help "FG2 terminated colour")
             <*> option cReader (long "terminatedBg2Colour"
                                   <> (value $ terminatedBg2Colour defaultTimerConfig)
                                   <> help "BG2 terminated colour")
             <*> option auto (long "terminatedBlinkRate"
                                   <> (value $ terminatedBgDelay defaultTimerConfig)
                                   <> help "Terminated blink rate in nanoseconds")
             <*> option sReader (long "terminatedShellCmd"
                                   <> (value $ terminatedShellCmd defaultTimerConfig)
                                   <> help "Shell command to be executed and timer terminates")
             where cReader = eitherReader (\s -> if s == "" then Right Nothing else Right (Just s))
                   sReader = eitherReader (\s -> if s == "" then Right Nothing else Right (Just s))



main :: IO ()
main = initialise =<< execParser opts
       where opts = info (parseColours <**> helper)
                      (fullDesc
                       <> progDesc "Creates a pomodoro timer."
                       <> header "pomobar - A customisable pomodoro timer for Xmonad") 
