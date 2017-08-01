module Main where

import           Control.Applicative
import           Control.Exception
import           Graphics.UI.Gtk
import           System.Exit
import           System.IO
import           System.Random

fileContents :: IO [String]
fileContents = do   comps <- try $ readFile "src/compliments.txt" :: IO (Either IOException String)
                    case comps of
                        Left except    -> putStrLn "The compliments file was not found. Please install compliments.txt" >> exitFailure
                        Right contents -> return $ lines contents

roll :: Int -> IO Int
roll n = getStdRandom (randomR (0,n-1))

generateCompliment :: IO String
generateCompliment = do comps <- fileContents
                        ranInt <- roll $ length comps
                        return $ "❤ " ++ (comps !! ranInt) ++ " ❤"

buttonPress :: (ButtonClass o, LabelClass l) => o -> l -> IO ()
buttonPress _ l = do  getComp <- generateCompliment
                      labelSetText l getComp

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    initGUI
    window <- windowNew
    label <- labelNew (Just "Press the button to compliment")
    button <- buttonNewWithLabel "Compliment!"
    vbox    <- vBoxNew False 0

    widgetModifyBg window StateNormal (Color 65535 0 65535)
    widgetModifyFg button StateNormal (Color 65535 0 0)

    boxPackStart vbox label PackRepel 0
    boxPackStart vbox button PackRepel 0
    containerAdd window vbox

    windowSetDefaultIconFromFile "src/heart.ico"
    set window [windowTitle := "Complimentor" , windowDefaultWidth := 200, windowDefaultHeight := 100, containerBorderWidth := 10]
    windowSetPosition window WinPosCenterAlways
    on window objectDestroy mainQuit
    on button buttonActivated (buttonPress button label)

    widgetShowAll window
    mainGUI
