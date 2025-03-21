{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
module EulerHS.Extra.URLSanitization where

import Data.Maybe (fromMaybe)
import Prelude
import qualified Data.HashSet as HS
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as DAT
import Test.Tasty.HUnit ((@?=))

{-

To sanitize/remove PII from URL

sanitizeMetricURL :: Text.Text -> Maybe (HS.HashSet Text.Text) -> IO Text.Text

This function requires a URL (Text) , list of whitelisting keywords (Maybe HashSet) and returns sanitized URL (Text)

> sanitizeMetricURL "/v1/payments/#id/otp/submit" $ Just $ HS.fromList ["v1","payments","otp","submit"]
"/v1/payments/#masked/otp/submit"

> sanitizeMetricURL "/v1/payments/#id/otp/submit" $ Just $ HS.fromList ["payments","otp","submit"]
"/#masked/payments/#masked/otp/submit"

There is a default whitelisting keywords list which will be used when Nothing is passed

> sanitizeMetricURL "/v1/payments/#id/otp/submit" $ Nothing
"/v1/payments/#masked/otp/submit"

-}

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
        getKeywords = HS.fromList ["http","https","internal", "txns", "cmpi-jwt", "cmpi-authenticate", "cmpi-lookup", "risk", "notify", "authorize", "vcolite", "partner", "v1", "accounts", "txnmetadata", "idtokens", "payout", "merchant", "orders", "v3", "transaction", "status", "api", "transactions", "v2", "pay", "webhooks", "txn-status", "v4", "gateways", "transaction_by_order_id", "payments", "refund", "customers", "tokens", "fund_accounts", "validations", "refunds", "otp", "submit", "resend", "theia", "fetchBinDetail", "processTransaction", "initiateTransaction", "order", "subscription", "renew", "create", "sale", "payment", "execute", "billing-agreements", "agreements", "agreement-tokens", "transaction-contexts", "lazypay", "v0", "enquiry", "apbnative", "partners", "authRequest", "authToken", "p1", "profile", "delink", "account", "debit", "checkout", "captures", "cancel", "capture", "confirmed", "marketplaceorderid", "deliveryupdate", "returned", "marketplace", "b2b", "customer", "creditLimit", "m1", "CONSENT_TOKEN", "balance", "rest", "version", "55", "3DSecureId", "57", "61", "pgsi", "recurring", "v1_1", "mandates", "upi", "validate", "MerchantAPI", "UPI", "CollectPay", "TransactionStatus", "CallbackStatus", "Refund", "PaymentGateway", "merchant2.pg", "euler", "start", "vies", "eligbility", "deenroll", "init-enrollment", "response", "gateway-response", "reg", "authenticate", "eligibility", "void", "ecr", "mandate", "proxy", "(", "registrationTokens", "query", "verify", "agencies", "vpa", "reset", "switch", "batch", "gateway", "scheme", "priorityLogic", "ingressRules", "migrateMgpm", "apiKeys", "user", "chargebacks", "issuerRoute", "isinRoute", "cardBrandRoute", "rms", "gatewayOutage", "feature", "gatewayHealth", "approve", "cards", "custom", "paymentmethods", "oauth", "callback", "finish", "wallets", "topup", "topup-response", "paresSubmit", "show-surcharge", "deviceInformation", "consent", "vco_response", "fulfillment", "response-amazonpay", "topup-response-amazonpay", "sign-and-encrypt-amazonpay-payload", "sync", "credit", "schedule", "queue", "psp", "card", "update_meta", "bank-accounts", "list", "offers", "apply", "ec-post-txn-worker", "syncWalletTopupStatus", "resetOutageScore"]


-- TODO: Move to attoparsec 
-- NOTE: Added this pure function for EPNG which should be moved to IO, once dev in epng starts

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
        getKeywords = HS.fromList ["http","https","internal", "txns", "cmpi-jwt", "cmpi-authenticate", "cmpi-lookup", "risk", "notify", "authorize", "vcolite", "partner", "v1", "accounts", "txnmetadata", "idtokens", "payout", "merchant", "orders", "v3", "transaction", "status", "api", "transactions", "v2", "pay", "webhooks", "txn-status", "v4", "gateways", "transaction_by_order_id", "payments", "refund", "customers", "tokens", "fund_accounts", "validations", "refunds", "otp", "submit", "resend", "theia", "fetchBinDetail", "processTransaction", "initiateTransaction", "order", "subscription", "renew", "create", "sale", "payment", "execute", "billing-agreements", "agreements", "agreement-tokens", "transaction-contexts", "lazypay", "v0", "enquiry", "apbnative", "partners", "authRequest", "authToken", "p1", "profile", "delink", "account", "debit", "checkout", "captures", "cancel", "capture", "confirmed", "marketplaceorderid", "deliveryupdate", "returned", "marketplace", "b2b", "customer", "creditLimit", "m1", "CONSENT_TOKEN", "balance", "rest", "version", "55", "3DSecureId", "57", "61", "pgsi", "recurring", "v1_1", "mandates", "upi", "validate", "MerchantAPI", "UPI", "CollectPay", "TransactionStatus", "CallbackStatus", "Refund", "PaymentGateway", "merchant2.pg", "euler", "start", "vies", "eligbility", "deenroll", "init-enrollment", "response", "gateway-response", "reg", "authenticate", "eligibility", "void", "ecr", "mandate", "proxy", "(", "registrationTokens", "query", "verify", "agencies", "vpa", "reset", "switch", "batch", "gateway", "scheme", "priorityLogic", "ingressRules", "migrateMgpm", "apiKeys", "user", "chargebacks", "issuerRoute", "isinRoute", "cardBrandRoute", "rms", "gatewayOutage", "feature", "gatewayHealth", "approve", "cards", "custom", "paymentmethods", "oauth", "callback", "finish", "wallets", "topup", "topup-response", "paresSubmit", "show-surcharge", "deviceInformation", "consent", "vco_response", "fulfillment", "response-amazonpay", "topup-response-amazonpay", "sign-and-encrypt-amazonpay-payload", "sync", "credit", "schedule", "queue", "psp", "card", "update_meta", "bank-accounts", "list", "offers", "apply", "ec-post-txn-worker", "syncWalletTopupStatus", "resetOutageScore"]

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
                    ,  ("/theia/api/v1/fetchBinDetail?mid=#mid&orderId=#orderId" :: Text.Text)
                    ,  ("theia/api/v1/processTransaction?mid=#mid&orderId=#orderId" :: Text.Text)
                    ,  ("theia/api/v1/initiateTransaction?mid=#mid&orderId=#orderId" :: Text.Text)
                    ]
        
        getResultList :: [Text.Text]
        getResultList = [ 
                        ("/v1/payments/#masked/otp/submit" :: Text.Text)
                            ,   ("/v1/payments/#masked/otp/resend" :: Text.Text)
                            ,   ("/v1/payments/#masked" :: Text.Text)
                            ,   ("/v1/orders/#masked/payments" :: Text.Text)
                            ,   ("/theia/api/v1/fetchBinDetail?mid=#masked&orderId=#masked" :: Text.Text)
                            ,   ("theia/api/v1/processTransaction?mid=#masked&orderId=#masked" :: Text.Text)
                            ,   ("theia/api/v1/initiateTransaction?mid=#masked&orderId=#masked" :: Text.Text)
                    ]