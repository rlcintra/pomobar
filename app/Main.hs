module Main where

import Pomobar
import Options.Applicative
import Data.Semigroup ((<>))

parseColours :: Parser ColourConfig
parseColours = ColourConfig
             <$> option reader (long "runningFgColour"
                                   <> (value $ runningFgColour defaultColourConfig)
                                   <> help "FG running colour")
             <*> option reader (long "pausedFgColour"
                                   <> (value $ pausedFgColour defaultColourConfig)
                                   <> help "FG paused colour")
             <*> option reader (long "terminatingFgColour"
                                   <> (value $ terminatingFgColour defaultColourConfig)
                                   <> help "FG terminating colour")
             <*> option reader (long "terminatedFg1Colour"
                                   <> (value $ terminatedFg1Colour defaultColourConfig)
                                   <> help "FG1 terminated colour")
             <*> option reader (long "terminatedBg1Colour"
                                   <> (value $ terminatedBg1Colour defaultColourConfig)
                                   <> help "BG1 terminated colour")
             <*> option reader (long "terminatedFg2Colour"
                                   <> (value $ terminatedFg2Colour defaultColourConfig)
                                   <> help "FG2 terminated colour")
             <*> option reader (long "terminatedBg2Colour"
                                   <> (value $ terminatedBg2Colour defaultColourConfig)
                                   <> help "BG2 terminated colour")
             <*> option auto (long "terminatedBlinkRate"
                                   <> (value $ terminatedBgDelay defaultColourConfig)
                                   <> help "Terminated blink rate in nanoseconds")
             where reader = eitherReader (\s -> if s == "" then Right Nothing else Right (Just s))

main :: IO ()
main = do
  config <- execParser opts
  initialise $ sanatiseColourConfig config
  where
    opts = info (parseColours <**> helper)
      ( fullDesc
     <> progDesc "Creates a pomodoro timer."
     <> header "pomobar - A customisable pomodoro timer for Xmonad" )

sanatiseColourConfig :: ColourConfig -> ColourConfig
sanatiseColourConfig (ColourConfig c1 c2 c3 c4 c5 c6 c7 d) =
  ColourConfig (f c1) (f c2) (f c3) (f c4) (f c5) (f c6) (f c7) d
  where f (Just "") = Nothing
        f c         = c
