{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Text.Julius
import Control.Monad.Logger (runStdoutLoggingT)

data HelloWorld = HelloWorld{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Policiais json
   login Text
   senha Text
   nome Text
   idade Int
   raca Text
   sinn Int
   sinntype Text
   matrixid Int
   patente Text
   
   deriving Show
|]


mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/denied ErroR GET
/singup CadastroR GET
|]

instance Yesod HelloWorld

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage HelloWorld FormMessage where
    renderMessage _ _ = defaultFormMessage
    
formPoliciais :: Form Policiais
formPoliciais = renderDivs $ Policiais <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing <*>
           areq textField "Nome: " Nothing <*>
           areq intField "Idade: " Nothing <*>
           areq textField "Raça: " Nothing <*>
           areq intField "SIN: " Nothing <*>
           areq textField "SIN Type: " Nothing <*>
           areq intField "Matix ID: " Nothing <*>
           areq textField "Patente: " Nothing

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
           
getCadastroR :: Handler Html
getCadastroR = do
           (widget, enctype) <- generateFormPost formPoliciais
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{CadastroR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]
           

connStr = "dbname=d3asuujt2vg6o1 host=ec2-54-163-226-48.compute-1.amazonaws.com user=isonzxoxadmqir password=wpDkE8ysUDGhWNfHoBZoCzx5CT port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (HelloWorld pool)
