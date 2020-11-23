--------------------------------------------------------------------------------
module Config where
--------------------------------------------------------------------------------
import           System.Environment                    (getEnv)
--------------------------------------------------------------------------------


-- TODO: refactor global ENV vars to configuration data
data EnvConfig = EnvConfig
  { envPostgresHost :: IO String
  , envPostgresPort :: IO String
  , envPostgresUser :: IO String
  , envPostgresPw   :: IO String
  , envPostgresDb   :: IO String
  }


globalEnvConfig :: EnvConfig
globalEnvConfig = EnvConfig
  { envPostgresHost = getEnv "POSTGRES_HOST"
  , envPostgresPort = getEnv "POSTGRES_PORT"
  , envPostgresUser = getEnv "POSTGRES_USER"
  , envPostgresPw   = getEnv "POSTGRES_PW"
  , envPostgresDb   = getEnv "POSTGRES_DB"
  }
  