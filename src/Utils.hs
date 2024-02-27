module Utils where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord.Types
import Text.Read (readMaybe)

getToken :: IO T.Text
getToken = TIO.readFile "./token.secret"

getGuildId :: IO GuildId
getGuildId = do
  gid <- readFile "./guildid.secret"
  case readMaybe gid of
    Just g -> pure g
    Nothing -> error "could not read guild id from file"
