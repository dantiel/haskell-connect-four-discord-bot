{-# LANGUAGE CPP                      #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Data.Either
import           Data.Maybe               (fromMaybe)
import Control.Monad (when, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.Split

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
import           System.Environment                        (getEnv, lookupEnv)
--------------------------------------------------------------------------------
import           FourWins
import           Database
import           Config
import           Utils

#ifdef DOT_ENV
import           Configuration.Dotenv
--------------------------------------------------------------------------------


loadDotEnv :: IO [(String, String)]
loadDotEnv = do loadFile Configuration.Dotenv.defaultConfig
#endif

#ifdef DOT_ENV
initEnv = loadDotEnv
#else
initEnv = print "didn't load environment."
#endif

main :: IO ()
main = do 

    initEnv

    envPort <- lookupEnv "PORT"
    envHost <- lookupEnv "HOST"
    envToken <- lookupEnv "TOKEN"
    
    mainDB
    
    connectFourBot (fromMaybe "3000" envPort) (fromMaybe "0.0.0.0" envHost) (fromMaybe "unset" envToken)

-- | Replies "pong" to every message that starts with "ping"
connectFourBot :: String -> String -> String -> IO ()
connectFourBot port host token = do

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = T.pack token
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^(6 :: Int))
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do _ <- restCall $ R.CreateMessage (channelId c) "Ahoy scurvy dog come across play four wins against me and walk th' plank if ye dare. `!newgame` t' start a game, `!move <X>` whar _`<X>`_ be th' column number whar ye want t' place yer godforsaken peg. Enter `!show` t' yield ye current mess."
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> when (not (fromBot m) && isCommand m) $ do
        when (isCommandNewGame m) $ do
            let uid = show . userId . messageAuthor $ m
            maybePlayer <- liftIO $ getPlayerIdByEmail uid
            when (Nothing == maybePlayer) . liftIO . createPlayer $ uid
            let gameState = ([], [])
                gameStateEncoded = encodeGameState gameState
            liftIO . setPlayerGameState uid $ Just gameStateEncoded
            let msg = T.pack $  "```" ++ drawGameState gameState ++ "```"
            _ <- restCall (R.CreateMessage (messageChannel m) msg)
            pure ()
        when (isCommandMove m) $ do
            _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
            -- threadDelay (4 * 10^(6 :: Int))
            let columnMove = (read . head . tail . splitOn "!move " . T.unpack . messageText $ m)
            let uid = show . userId . messageAuthor $ m
            gameStateEncoded <- liftIO $ getPlayerGameState uid
            let gameState = decodeGameState (fromMaybe (error "no game found") gameStateEncoded)
                eitherNewGameState = getNextGameState gameState columnMove 
                newGameState = fromRight gameState eitherNewGameState
            liftIO . setPlayerGameState uid . Just . encodeGameState $ newGameState 
            let msg= T.pack $ "```" ++ drawMyturn eitherNewGameState ++ "```"
            
            _ <- restCall (R.CreateMessage (messageChannel m) msg)
            pure ()
        when (isCommandShow m) $ do
            let uid = show . userId . messageAuthor $ m
            maybePlayer <- liftIO $ getPlayerIdByEmail uid
            maybeGameState <- case maybePlayer of 
                Nothing -> do 
                    pure Nothing
                Just pl -> do
                    liftIO $ (onJust decodeGameState <$> getPlayerGameState uid)
            case maybeGameState of
                Nothing -> do
                    restCall (R.CreateMessage (messageChannel m) 
                        "Ye did nay start a game yet! Arr, type `!newgame`")
                Just ga -> do
                    let msg = T.pack $ "```" ++ drawGameState ga ++ "```"
                    restCall (R.CreateMessage (messageChannel m) msg)
            pure ()
        pure ()
    _ -> pure ()


isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False


fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)


isCommand :: Message -> Bool
isCommand = ("!" `T.isPrefixOf`) . T.toLower . messageText


isCommandMove :: Message -> Bool
isCommandMove = ("!move" `T.isPrefixOf`) . T.toLower . messageText


isCommandNewGame :: Message -> Bool
isCommandNewGame = ("!newgame" `T.isPrefixOf`) . T.toLower . messageText


isCommandShow :: Message -> Bool
isCommandShow = ("!show" `T.isPrefixOf`) . T.toLower . messageText


isCommandSetDifficulty :: Message -> Bool
isCommandSetDifficulty = ("!setdifficulty" `T.isPrefixOf`) . T.toLower . messageText

