import Network.HTTP.Types (status200, status302)
import Network.Wai (Response, responseLBS)
import Network.Wai.Internal (ResponseReceived)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Predicate (true)
import Network.Wai.Routing (Routes, continue, param, get, post, prepare, route, Continue)
import Data.String (fromString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString (ByteString)

main :: IO ()
main = run 8000 (route (prepare app))

app :: Routes a IO ()
app = do
    post "/hello" (respond200 "Hello Postman!") true
    get "/hello" (respond200 "Hello World!") true
    get "/hello/:name" (respondTemplateByteString greetPerson) $ param "name"
    get "/" (redirectTo "/hello") true
  where
    greetPerson :: Lazy.ByteString -> Lazy.ByteString
    greetPerson name = mconcat ["Hello ", name, "!"]

respond200 :: Lazy.ByteString -> a -> Continue IO -> IO ResponseReceived
respond200 text = continue . const $ writeTextResponse text

respondTemplateByteString :: Monad m => (t -> Lazy.ByteString) -> t -> Continue m -> m ResponseReceived
respondTemplateByteString f = continue $ \value -> writeTextResponse $ f value

respondTemplateShow :: (Monad m, Show a) => (t -> a) -> t -> Continue m -> m ResponseReceived
respondTemplateShow f = continue $ \s -> writeTextResponse . fromString . show $ f s

redirectTo :: Monad m => ByteString -> b -> Continue m -> m ResponseReceived
redirectTo path = continue . const $ (return . responseLBS status302 [("Location", path)]) ""

writeTextResponse :: Monad m => Lazy.ByteString -> m Response
writeTextResponse = return . responseLBS status200 []
