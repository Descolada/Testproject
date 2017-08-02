{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Exception
import qualified Data.Text           as T
import           Data.Text.IO
import           "gtk3" Graphics.UI.Gtk
import           System.Exit
import           System.IO
import           System.Random

fileContents :: IO [String]
fileContents = do   comps <- try $ Data.Text.IO.readFile "src/compliments.txt" :: IO (Either IOException T.Text)
                    case comps of
                        Left except    -> exitFailure
                        Right contents -> return $ map T.unpack $ T.lines contents

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
