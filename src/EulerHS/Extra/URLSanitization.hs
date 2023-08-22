{- |
Module      :  EulerHS.Extra.URLSanitization
Copyright   :  (C) Juspay Technologies Pvt Ltd 2019-2022
License     :  Apache 2.0 (see the file LICENSE)
Maintainer  :  opensource@juspay.in
Stability   :  experimental
Portability :  non-portable
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module EulerHS.Extra.URLSanitization where

import Data.Maybe (fromMaybe)
import Prelude
import qualified Data.HashSet as HS
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as DAT
import Test.Tasty.HUnit ((@?=))


sanitizeMetricURL :: Text.Text -> Maybe (HS.HashSet Text.Text) -> IO Text.Text
sanitizeMetricURL url allowedKeywords = do
    splitByQuestion <- runParserForURLPathAndQueryParams url
    let queryParamsCheck =
            case splitByQuestion of
                DAT.Done left right -> [right,(Text.drop 1 $ left)]
                _ -> [url]
        urlPath = Text.splitOn ("/" :: Text.Text) (lookupNthElement queryParamsCheck 0)
        maskedPath = map (mask) urlPath
    return $ (Text.intercalate ("/" :: Text.Text) maskedPath) <> (if length queryParamsCheck > 1 then ("?" :: Text.Text) <> (sanitizeQP $ lookupNthElement queryParamsCheck 1) else ("" :: Text.Text))
    where
        sanitizeQP :: Text.Text -> Text.Text
        sanitizeQP a = Text.drop 1 $ foldl (\acc x -> acc <> ("&" :: Text.Text) <> (splitByEq x)) ("" :: Text.Text) $ Text.splitOn ("&" :: Text.Text) a

        splitByEq :: Text.Text -> Text.Text
        splitByEq x = 
            let k = Text.splitOn ("=" :: Text.Text) x
                fstEle = lookupNthElement k 0
                sndEle = lookupNthElement k 1
            in if fstEle /= "" then fstEle <> (if checkIfAllowed fstEle then if sndEle /= "" then ("=" :: Text.Text) <> sndEle else ("" :: Text.Text) else ("=" :: Text.Text) <> ("#masked" :: Text.Text)) else ("" :: Text.Text)
        
        lookupNthElement :: [Text.Text] -> Int -> Text.Text
        lookupNthElement xs k =
                case drop k xs of
                    x:_ -> x
                    [] -> ("" :: Text.Text)
        
        runParserForURLPathAndQueryParams :: Text.Text -> IO (DAT.Result Text.Text)
        runParserForURLPathAndQueryParams str = return $ DAT.parse parseURLPath str

        parseURLPath :: DAT.Parser Text.Text
        parseURLPath = DAT.takeTill (=='?')

        mask :: Text.Text -> Text.Text
        mask "" = ""
        mask x = if checkIfAllowed x then x else "#masked"

        checkIfAllowed :: Text.Text -> Bool
        checkIfAllowed x = HS.member x $ fromMaybe getKeywords allowedKeywords 
        
        -- These are moved from euler-ps
        getKeywords :: HS.HashSet Text.Text
        getKeywords = HS.fromList []



sanitizeMetricURL' :: Text.Text -> Maybe (HS.HashSet Text.Text) -> Text.Text
sanitizeMetricURL' url allowedKeywords = do
    let queryParamsCheck = Text.splitOn ("?" :: Text.Text) url
        urlPath = Text.splitOn ("/" :: Text.Text) (lookupNthElement queryParamsCheck 0)
        maskedPath = map (mask) urlPath
    (Text.intercalate ("/" :: Text.Text) maskedPath) <> (if length queryParamsCheck > 1 then ("?" :: Text.Text) <> (sanitizeQP $ lookupNthElement queryParamsCheck 1) else ("" :: Text.Text))
    where
        sanitizeQP :: Text.Text -> Text.Text
        sanitizeQP a = Text.drop 1 $ foldl (\acc x -> acc <> ("&" :: Text.Text) <> (splitByEq x)) ("" :: Text.Text) $ Text.splitOn ("&" :: Text.Text) a

        splitByEq :: Text.Text -> Text.Text
        splitByEq x = 
            let k = Text.splitOn ("=" :: Text.Text) x
                fstEle = lookupNthElement k 0
                sndEle = lookupNthElement k 1
            in if fstEle /= "" then fstEle <> (if checkIfAllowed fstEle then if sndEle /= "" then ("=" :: Text.Text) <> sndEle else ("" :: Text.Text) else ("=" :: Text.Text) <> ("#masked" :: Text.Text)) else ("" :: Text.Text)
        
        lookupNthElement :: [Text.Text] -> Int -> Text.Text
        lookupNthElement xs k =
                case drop k xs of
                    x:_ -> x
                    [] -> ("" :: Text.Text)

        mask :: Text.Text -> Text.Text
        mask "" = ""
        mask x = if checkIfAllowed x then x else "#masked"

        checkIfAllowed :: Text.Text -> Bool
        checkIfAllowed x = HS.member x $ fromMaybe getKeywords allowedKeywords 
        
        -- These are moved from euler-ps
        getKeywords :: HS.HashSet Text.Text
        getKeywords = HS.fromList []

unit_sanitizeMetricURL :: IO [Text.Text]
unit_sanitizeMetricURL = g getURLListInput getResultList
    where
        g :: [Text.Text] -> [Text.Text] -> IO [Text.Text]
        g (x:xs) (y:ys) = do
            r <- (sanitizeMetricURL x Nothing)
            s <- g xs ys
            r @?= y
            pure $ [r] ++ s
        g _ _ = pure []

        getURLListInput :: [Text.Text]
        getURLListInput =   [
                    ("/v1/payments/#id/otp/submit" :: Text.Text)
                    ,  ("/v1/payments/#id/otp/resend" :: Text.Text)
                    ,  ("/v1/payments/#paymentId" :: Text.Text)
                    ,  ("/v1/orders/#order_id/payments" :: Text.Text)
                    ,  ("/api/v1/fetchBinDetail?mid=#mid&orderId=#orderId" :: Text.Text)
                    ,  ("/api/v1/processTransaction?mid=#mid&orderId=#orderId" :: Text.Text)
                    ,  ("/api/v1/initiateTransaction?mid=#mid&orderId=#orderId" :: Text.Text)
                    ]
        
        getResultList :: [Text.Text]
        getResultList = [ 
                        ("/v1/payments/#masked/otp/submit" :: Text.Text)
                            ,   ("/v1/payments/#masked/otp/resend" :: Text.Text)
                            ,   ("/v1/payments/#masked" :: Text.Text)
                            ,   ("/v1/orders/#masked/payments" :: Text.Text)
                            ,   ("/api/v1/fetchBinDetail?mid=#masked&orderId=#masked" :: Text.Text)
                            ,   ("/api/v1/processTransaction?mid=#masked&orderId=#masked" :: Text.Text)
                            ,   ("/api/v1/initiateTransaction?mid=#masked&orderId=#masked" :: Text.Text)
                    ]