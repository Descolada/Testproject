{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           "gtk3" Graphics.UI.Gtk
import           System.Exit
import           System.IO
import           System.IO.Encoding
import           System.Random

type WidgetSize = Int

winHeight = 100 :: WidgetSize
winWidth = 200 :: WidgetSize

fileContents :: IO [String]
fileContents = do   comps <- try $ readFileWithEncoding utf8 "src/compliments.txt" :: IO (Either IOException String)
                    case comps of
                        Left except    -> exitFailure
                        Right contents -> return $ lines contents

roll :: Int -> IO Int
roll n = getStdRandom (randomR (0,n-1))

generateCompliment :: IO String
generateCompliment = do comps <- fileContents
                        ranInt <- roll $ length comps
                        return $ "❤ " ++ (comps !! ranInt) ++ " ❤"

buttonPress :: (WindowClass w, ButtonClass o, LabelClass l) => w -> o -> l -> IO ()
buttonPress w _ l = do  getComp <- generateCompliment
                        windowResize w winWidth winHeight
                        labelSetText l getComp

main :: IO ()
main = do
    initGUI
    window <- windowNew
    label <- labelNew (Just "Vajuta nupule et saada kompliment")
    button <- buttonNewWithLabel "Uus kompliment!"
    vbox    <- vBoxNew False 0

    widgetModifyBg window StateNormal (Color 65535 0 65535)
    widgetModifyFg button StateNormal (Color 65535 0 0)

    boxPackStart vbox label PackRepel 0
    boxPackStart vbox button PackRepel 0
    containerAdd window vbox

    windowSetDefaultIconFromFile "src/heart.ico"
    set window [windowTitle := "Complimentor" , windowDefaultWidth := winWidth, windowDefaultHeight := winHeight, containerBorderWidth := 10]
    windowSetPosition window WinPosCenterAlways
    on window objectDestroy mainQuit
    on button buttonActivated (buttonPress window button label)

    widgetShowAll window
    mainGUI
