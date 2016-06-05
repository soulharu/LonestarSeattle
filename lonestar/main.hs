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
   sinn Double
   sinntype Text
   matrixid Double
   patente Text
   deriving Show
   
Crimes json
   artigo Int
   nome Text sqltype=varchar(30)
   descricao Text
   deriving Show

Bioware json
   nome Text sqltype=varchar(40)
   descricao Text
   deriving Show

Cyberware json
   nome Text sqltype=varchar(40)
   descricao Text
   deriving Show

Runners json
   nome Text  sqltype=varchar(50)
   alias Text  sqltype=varchar(30)
   idade Int
   raca Text  sqltype=varchar (15)
   sinn Double
   sinntype Text  sqltype=varchar(20)
   magiclicenses Double
   fireweaponslicenses Double
   lastseen Text
   deriving Show
   
RunCyber
   runid RunnersId
   cyberid CyberwareId
   UniqueRunCyber runid cyberid

RunBio
   runid RunnersId
   bioid BiowareId
   UniqueRunBio runid bioid

RunCrime
   runid RunnersId
   crimeid CrimesId
   UniqueRunCrime runid crimeid

|]


mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/denied ErroR GET
/singup CadastroR GET POST
/login LoginR GET POST
/adm AdminR GET
/profile/#PoliciaisId PerfilR GET
/leave LogoutR GET
/bioreg BioregR GET POST
/cyreg CyberregR GET POST
/crireg CrimeregR GET POST
|]

instance Yesod HelloWorld


instance YesodPersist HelloWorld where
   type YesodPersistBackend HelloWorld = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

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
           areq doubleField "SIN: " Nothing <*>
           areq textField "SIN Type: " Nothing <*>
           areq doubleField "Matix ID: " Nothing <*>
           areq textField "Patente: " Nothing
           
formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formCrime :: Form Crimes
formCrime = renderDivs $ Crimes <$>
            areq intField "Artigo: " Nothing <*>
            areq textField "Nome: " Nothing <*>
            areq textField "Descrição: " Nothing

formBio :: Form Bioware
formBio = renderDivs $ Bioware <$>
            areq textField "Nome: " Nothing <*>
            areq textField "Descrição: " Nothing

formCyber :: Form Cyberware
formCyber = renderDivs $ Cyberware <$>
            areq textField "Nome: " Nothing <*>
            areq textField "Descrição: " Nothing



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
           
postCadastroR :: Handler Html
postCadastroR = do
           ((result, _), _) <- runFormPost formPoliciais
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ do
           toWidget $ $(luciusFile "templates/login.lucius")
           toWidgetHead [hamlet|
             <link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/css?family=Orbitron">
           |]
           $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [PoliciaisLogin ==. login, PoliciaisSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)


getAdminR :: Handler Html
getAdminR = defaultLayout $ do
           toWidget $ $(luciusFile "templates/admin.lucius")
           toWidgetHead [hamlet|
             <link rel="stylesheet" type="text/css" href="https://fonts.googleapis.com/css?family=Orbitron">
           |]
           $(whamletFile "templates/admin.hamlet")
           
getPerfilR :: PoliciaisId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> flw mlk doido!
     |]

getBioregR :: Handler Html
getBioregR = do
           (widget, enctype) <- generateFormPost formBio
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{BioregR}>
                     ^{widget}
                     <input type="submit" value="Cadastrar">
           |]

postBioregR :: Handler Html
postBioregR = do
            ((result,_),_) <- runFormPost formBio
            case result of
                FormSuccess bio -> (runDB $ insert bio) >> defaultLayout [whamlet|<h1> Bioware inserida!|]
                _ -> redirect BioregR

getCyberregR :: Handler Html
getCyberregR = do
           (widget, enctype) <- generateFormPost formCyber
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{CyberregR}>
                     ^{widget}
                     <input type="submit" value="Cadastrar">
           |]
           
postCyberregR :: Handler Html
postCyberregR = do
            ((result,_),_) <- runFormPost formCyber
            case result of
                FormSuccess cyber -> (runDB $ insert cyber) >> defaultLayout [whamlet|<h1> Cyberware inserida!|]
                _ -> redirect CyberregR
                
getCrimeregR :: Handler Html
getCrimeregR = do
           (widget, enctype) <- generateFormPost formCrime
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{CrimeregR}>
                     ^{widget}
                     <input type="submit" value="Cadastrar">
           |]

postCrimeregR :: Handler Html
postCrimeregR = do
            ((result,_),_) <- runFormPost formCrime
            case result of
                FormSuccess cri -> (runDB $ insert cri) >> defaultLayout [whamlet|<h1> Crime/Infração inserida!|]
                _ -> redirect CrimeregR


connStr = "dbname=d3asuujt2vg6o1 host=ec2-54-163-226-48.compute-1.amazonaws.com user=isonzxoxadmqir password=wpDkE8ysUDGhWNfHoBZoCzx5CT port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (HelloWorld pool)
