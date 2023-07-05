module EulerHS.Extra.Regex where

import Data.String.Conversions hiding ((<>))
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import Prelude
import qualified Control.Exception as CE
import qualified Data.ByteString as BT
import qualified Data.Maybe as DM
import qualified Data.Text as T
import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Text.Regex.PCRE.Light.Char8 as TRPLC

data RegExException = RegExException T.Text
    deriving Show

instance CE.Exception RegExException

regex' :: BT.ByteString -> Either T.Text PCRE.Regex
regex' re = 
    case PCRE.compileM formatRe [] of
        Right val -> Right val
        Left err -> CE.throw $ RegExException $ (T.pack $ err) <> " " <> (T.pack $ show re)
    where
        formatRe = encodeUtf8 $ T.replace "[^]" "[^-]" (decodeUtf8 re)

regex :: T.Text -> Either T.Text PCRE.Regex
regex str = regex' (encodeUtf8 $ T.replace "[^]" "[^-]" str)

replace :: PCRE.Regex -> SBS -> T.Text -> T.Text
replace cRegex to str = PCRE.gsub cRegex to str

test :: PCRE.Regex -> T.Text -> Bool
test r str = 
    case TRPLC.match r (T.unpack str) [] of
        Nothing -> False
        DM.Just _ -> True

match :: PCRE.Regex -> T.Text -> Maybe [Maybe T.Text]
match r str = 
    case (TRPLC.match r (T.unpack str) []) of
        Nothing -> Nothing
        DM.Just val -> DM.Just $ fmap (DM.Just . T.pack) $ val