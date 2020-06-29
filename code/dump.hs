module Dump where

rand a b =


--(first (Just . mapMaybe (decode' . Aeson.encode . Aeson.decode) )) --(traceShowId . decode' . fromStrict . BS.takeWhile ( (==). c2w '\n'))))
-- parser :: BS.ByteString -> (Maybe [CategorizedLog], BS.ByteString)
-- parser =
--     (fromMaybe (mempty, mempty)
--     DLE.unsnoc
--     <<< BS.split ((fromIntegral <<< fromEnum) '\n'))

-- transform :: DA.Object -> Maybe CategorizedLog
-- transform x = (decode' . fromStrict) (traceShow "log" x)


  -- transform x = traceShow x Nothing
    -- >=>  (decode' . fromStrict)

-- lineStream'
--   :: (MonadAsync m, MonadMask m, MonadError e m)
--   => Socket -> SerialT m CategorizedLog
-- lineStream' sock =
--   SP.mapM ( pure . fromJust . decode'. fromStrict)
--   $ SP.fromList =<< fst <$>
--     SP.iterateM
--       (\(_, buf) ->
--         (fromMaybe (mempty, mempty)
--         <<< DLE.unsnoc
--         <<< BS.split ((fromIntegral <<< fromEnum) '\n')
--         <<< (buf <>))
--           <$> liftIO . recv sock $ 4096)
--       (pure (mempty, mempty))
