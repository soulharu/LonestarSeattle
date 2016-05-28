{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Text.Julius
import Control.Monad.Logger (runStdoutLoggingT)

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           toWidget $ $(luciusFile "templates/home.lucius")
           toWidgetHead [hamlet|
             <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js">
           |]
           $(whamletFile "templates/home.hamlet")
           toWidgetHead $ $(juliusFile "templates/home.julius")

main :: IO ()
main = warp 8080 HelloWorld
