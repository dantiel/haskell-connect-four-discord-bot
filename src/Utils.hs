module Utils where

--------------------------------------------------------------------------------

-- | Basically fmap for Maybe.
onJust :: (a -> b) -> Maybe a -> Maybe b
onJust f (Just x) = Just (f x)
onJust _ Nothing = Nothing


-- | Join for Maybe.
joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just (Just x)) = Just x
joinMaybe _               = Nothing

