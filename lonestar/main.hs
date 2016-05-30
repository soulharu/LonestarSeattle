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
/denied ErroR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
           toWidget $ $(luciusFile "templates/home.lucius")
           toWidgetHead [hamlet|
             <script src="//ajax.googleapis.com/ajax/libs/jquery/2.1.0/jquery.min.js">
             <link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/css?family=Orbitron">
           |]
           $(whamletFile "templates/home.hamlet")
           toWidgetHead $ $(juliusFile "templates/home.julius")
           
getErroR :: Handler Html
getErroR = defaultLayout $ do
           toWidget $ $(luciusFile "templates/erro.lucius")
           toWidgetHead [hamlet|
             <link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/css?family=Orbitron">
           |]
           $(whamletFile "templates/erro.hamlet")

main :: IO ()
main = warp 8080 HelloWorld
