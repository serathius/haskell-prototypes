module Main where
 
import           Yesod

data YesodDemo = YesodDemo

mkYesod "YesodDemo" [parseRoutes|
/              HomeR      GET
/hello         HelloR     GET POST
/hello/#String HelloNameR GET
|]

instance Yesod YesodDemo

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello there|]

getHelloR :: Handler String 
getHelloR = return "Hello world"

postHelloR :: Handler String
postHelloR = return "Hello postman"

getHelloNameR :: String -> Handler String
getHelloNameR name = return $ "Hello " ++ name


main :: IO ()
main = do
  warp 3000 $ YesodDemo