{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
--------------------------------------------------------------------------------
module Database where

--------------------------------------------------------------------------------
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Logger                  (runStderrLoggingT,
                                                        NoLoggingT)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource.Internal (ResourceT)
import qualified Data.ByteString.Char8            as B
import           Data.Char                             (chr)
import           Data.Semigroup                        (Semigroup((<>)))
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text                        as T
import           Data.Time
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Prelude                        hiding ((-))
--------------------------------------------------------------------------------
import           Config
import           Utils
--------------------------------------------------------------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Player
  email String
  gameState String Maybe
  difficulty String Maybe
  PrimaryU email
  UniqueU email
  deriving Show Eq Read
|]


--------------------------------------------------------------------------------

bsToString :: B.ByteString -> String
bsToString bs = Prelude.map (chr . fromEnum) . B.unpack $ bs


connStrFromEnv :: EnvConfig -> IO ConnectionString
connStrFromEnv env = do
  host  <- B.pack <$> envPostgresHost env
  db    <- B.pack <$> envPostgresDb env
  user  <- B.pack <$> envPostgresUser env
  port  <- B.pack <$> envPostgresPort env
  pw    <- B.pack <$> envPostgresPw env
  let _host = B.pack "host="
      _db   = B.pack " dbname="
      _user = B.pack " user="
      _pw   = B.pack " password="
      _port = B.pack " port="
  pure $
    _host <> host <> _db <> db <> _user <> user <> _pw <> pw <> _port <> port


getValueFromEntity :: Entity t -> t
getValueFromEntity (Entity _ v) = v


type IsBackend b = (BaseBackend b ~ SqlBackend, IsPersistBackend b)
type Backend b c = ReaderT b (NoLoggingT (ResourceT IO)) c


withDatabase :: backend ~ SqlBackend => Backend backend a
             -> IO a
withDatabase r = do
  _connStr <- connStrFromEnv globalEnvConfig
  runStderrLoggingT  $ withPostgresqlPool _connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ r

--------------------------------------------------------------------------------


getPlayerByEmail :: Unique Player
               -> Backend SqlBackend (Maybe (Entity Player))
               -- -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe (Entity Player))
getPlayerByEmail = getBy


--------------------------------------------------------------------------------

getPlayerIdByEmail :: String -> IO (Maybe (Key Player))
getPlayerIdByEmail email = withDatabase $ do
  runMigration migrateAll
  onJust (entityKey) <$> getPlayerByEmail (UniqueU email)



--------------------------------------------------------------------------------

createPlayer :: String -> IO ()
createPlayer email = withDatabase $ do
  runMigration migrateAll
  insert_ $ Player email Nothing Nothing


getPlayerGameState :: String -> IO (Maybe String)
getPlayerGameState email = withDatabase $ do
  player <- getPlayerByEmail (UniqueU email)
  let maybeGameState = playerGameState . getValueFromEntity <$> player
  pure $ joinMaybe maybeGameState
  

setPlayerGameState :: String -> Maybe String -> IO ()
setPlayerGameState email gameState = withDatabase $ do
  runMigration migrateAll
  userEntity <- getPlayerByEmail $ UniqueU email
  case userEntity of
    Nothing -> error "Player not found"
    Just ue -> updateEntity ue [PlayerGameState =. gameState]


updateEntity :: (PersistEntityBackend record ~ BaseBackend backend,
                 PersistEntity record, MonadIO m, PersistStoreWrite backend)
             => Entity record
             -> [Update record]
             -> ReaderT backend m ()
updateEntity e = update (entityKey e)

mainDB :: IO ()
mainDB = withDatabase $ do
      runMigration migrateAll

