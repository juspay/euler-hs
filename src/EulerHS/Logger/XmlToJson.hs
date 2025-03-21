{-# OPTIONS_GHC -Wno-error=overlapping-patterns -Wno-error=orphans -Wno-error=incomplete-patterns -Wno-error=unused-matches#-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module EulerHS.Logger.XmlToJson where

import EulerHS.Prelude hiding (Key)
import Data.Aeson
import Data.List (head)
import Xmlbf
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as AK
import qualified Data.Vector as V
import qualified Data.Text as T
import EulerHS.Common (hashMapToKeyMap)

instance FromXml Value where
  fromXml = pChildren >>= convertChildrenToValue

convertChildrenToValue :: Monad m => [Xmlbf.Node] ->  ParserT m Value
convertChildrenToValue nodes =
  if length nodes == 1
    then convertChildrenToValue (getC $ head nodes)
    else fmap Object . foldM f KM.empty $ nodes
  where
    getC (Element name attrs chNodes) = chNodes
    getC _ = []
    f :: Monad m => Object -> Node -> ParserT m Object
    f mainMap (Element name attrs chNodes) =
      ( \curr ->
          case curr of
            Object currE -> KM.insert (AK.fromText name) (Object $ KM.union (String <$> (hashMapToKeyMap attrs)) currE) mainMap
            String y ->
              if KM.null (hashMapToKeyMap attrs)
                then KM.insert (AK.fromText name) (String y) mainMap
                else KM.insert (AK.fromText name) (Object $ KM.insert (AK.fromText name) (String y) (String <$> (hashMapToKeyMap attrs))) mainMap
            z -> (String <$> (hashMapToKeyMap attrs))
      )
        <$> (convertToValue chNodes)
    f m e = pure m
    f _ e = error $ T.pack $ "Shouldn't get the text, expected a Nested tag" ++ show nodes

-- Shims the missing update function for KeyMap
-- Works similar to previously used HashMap.update function
-- see: https://hackage.haskell.org/package/unordered-containers-0.2.20/docs/Data-HashMap-Internal.html#v:update
-- Logic (from docs):-
--     The expression (update f k map) updates the value x at k (if it is in the map).
--     If (f x) is Nothing, the element is deleted.
--     If it is (Just y), the key k is bound to the new value y.
akmUpdate :: (a -> Maybe a) -> KM.Key -> KM.KeyMap a -> KM.KeyMap a
akmUpdate f k km = case KM.lookup k km of
  Nothing -> km
  Just val -> case f val of
    Nothing -> KM.delete k km
    _ -> KM.insert k val km

convertToValue :: Monad m1 => [Xmlbf.Node] ->  ParserT m1 Value
convertToValue nodes =
  if length nodes == 1
    then case head nodes of
      Element name attrsn ns -> fmap Object . foldM f KM.empty $ nodes
      Text n -> return $ String n
    else fmap Object . foldM f KM.empty $ nodes
  where
    f m (Element name attrs ns) =
      if KM.null (hashMapToKeyMap attrs) && null ns
        then return KM.empty
        else
          ( \x -> case x of
              Object y -> case KM.lookup (AK.fromText name) m of
                Nothing -> KM.insert (AK.fromText name) (Object $ KM.union (String <$> (hashMapToKeyMap attrs)) y) m
                Just _ ->
                  akmUpdate
                    ( \old -> Just $ case old of
                        Array oldAr -> Array $ V.fromList $ (V.toList oldAr) ++ [Object (KM.union (String <$> (hashMapToKeyMap attrs)) y)]
                        Object oldO -> Array $ V.fromList $ (Object oldO) : [Object (KM.union (String <$> (hashMapToKeyMap attrs)) y)]
                    )
                    (AK.fromText name)
                    m
              String y -> case KM.lookup (AK.fromText name) m of
                Nothing -> if KM.null (hashMapToKeyMap attrs)
                              then KM.insert (AK.fromText name) (String y) m
                              else KM.insert (AK.fromText name) (Object $ KM.insert (AK.fromText name) (String y) (String <$> (hashMapToKeyMap attrs))) m
                Just _ ->
                  akmUpdate
                    ( \old -> Just $ case old of
                        Array oldAr -> Array $ V.fromList $ (V.toList oldAr) ++ [Object (KM.insert (AK.fromText name) (String y) (String <$> (hashMapToKeyMap attrs)))]
                        Object oldO -> Array $ V.fromList $ (Object oldO) : [Object (KM.insert (AK.fromText name) (String y) (String <$> (hashMapToKeyMap attrs)))]
                    )
                    (AK.fromText name)
                    m
              z -> case KM.lookup (AK.fromText name) m of
                Nothing -> KM.insert (AK.fromText name) (Object $ String <$> (hashMapToKeyMap attrs)) m
                Just _ ->
                  akmUpdate
                    ( \old -> Just $ case old of
                        Array oldAr -> Array $ V.fromList $ (V.toList oldAr) ++ [Object (String <$> (hashMapToKeyMap attrs))]
                        Object oldO -> Array $ V.fromList $ (Object oldO) : [Object (String <$> (hashMapToKeyMap attrs))]
                    )
                    (AK.fromText name)
                    m
          )
          <$> (convertToValue ns)
    f m e = pure m

