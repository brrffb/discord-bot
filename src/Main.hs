{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (forM_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Random
import Text.Read (readMaybe)
import Utils (getToken)

-- MAIN
main :: IO ()
main = do
    token <- getToken
    err <-
        runDiscord $
            def
                { discordToken = token
                , discordOnEvent = eventHandler
                }

    echo $ "Error: " <> err

echo :: (MonadIO m) => Text -> m ()
echo = liftIO . TIO.putStrLn

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> handleMessage m
    _ -> return ()
  where
    handleMessage :: Message -> DiscordHandler ()
    handleMessage m
        | isPrefixOf "!joeyy" = do
            let gifs =
                    [ "https://tenor.com/view/joeyy-1man-glitz-him-gif-25139183"
                    , "https://media.discordapp.net/attachments/1195450240568926280/1198398186394493018/joeyy-1man.gif"
                    , "https://cdn.discordapp.com/attachments/1195450240568926280/1198402365934546945/joeyy-1man.gif"
                    , "https://media.discordapp.net/attachments/1195450240568926280/1198397263676985344/joeyy-1man.gif"
                    , "https://tenor.com/view/joeyy-gang-gif-27476151"
                    ]
            randomGif <- liftIO $ getRandomElement gifs
            call $ sendMessage m randomGif

        | isPrefixOf "!priv9" = do
            let gifs =
                    [ "https://tenor.com/view/priv9-rust-hvh-gif-8705133624523917923"
                    , "https://tenor.com/view/priv9-hvh-rust-blp-kosher-gif-13497781921916945913"
                    , "https://tenor.com/view/priv9-gif-6191931157080484547"
                    , "https://tenor.com/view/priv9-rust-hvh-gif-17888912719336155427"
                    , "https://tenor.com/view/priv9-rust-hvh-gif-18306603389609312753"
                    , "https://tenor.com/view/priv9-rust-hvh-gif-11110089197635228454"
                    ]
            randomGif <- liftIO $ getRandomElement gifs
            call $ sendMessage m randomGif

        | isPrefixOf "!bantwenter" = do
            call $ banTwenter gid

        | isPrefixOf "!fuckyou" = do
            removeRoles gid uid

        | isPrefixOf "!Lol" = do
            call $
                R.CreateGuildRole gid $
                    R.ModifyGuildRoleOpts
                        (Just "Lol")
                        (Just (RolePermissions 0x0000000000000008))
                        Nothing
                        Nothing
                        Nothing
                        Nothing

        | isPrefixOf "!a" = addRoles gid (userId $ messageAuthor m)

        | isPrefixOf "!warn" = warnMember uid reason
      where
        getRandomElement :: [a] -> IO a
        getRandomElement list = do
            randomIndex <- randomRIO (0, length list - 1)
            return (list !! randomIndex)

        gid = fromJust $ messageGuildId m
        uid = fst $ getFromJust m
        reason = snd $ getFromJust m

        getFromJust :: Message -> (UserId, Text)
        getFromJust = fromJust . getUidAndReason . messageContent

        isPrefixOf :: Text -> Bool
        isPrefixOf n = n `T.isPrefixOf` messageContent m

getUidAndReason :: Text -> Maybe (UserId, Text)
getUidAndReason message = case T.words message of
    (_ : uid : reason) -> Just (read' uid, T.unwords reason)
    _ -> Nothing

read' :: (Read a) => Text -> a
read' = fromJust . readMaybe . T.unpack

call :: (Request (r a), FromJSON a) => r a -> DiscordHandler ()
call = void . restCall

sendMessage :: Message -> Text -> R.ChannelRequest Message
sendMessage m = R.CreateMessage (messageChannelId m)

banTwenter :: GuildId -> R.GuildRequest ()
banTwenter guild = R.CreateGuildBan guild (read' "1085910996436140064") $ R.CreateGuildBanOpts Nothing $ Just "noob"

removeRoles :: GuildId -> UserId -> DiscordHandler ()
removeRoles gid uid = do
    member <- restCall $ R.GetGuildMember gid uid
    case member of
        Right m -> do
            let roles = memberRoles m
            forM_ roles $ \role ->
                call $ R.RemoveGuildMemberRole gid uid role
        Left err -> do
            echo $ "Error getting guild member: " <> T.pack (show err)

addRoles :: GuildId -> UserId -> DiscordHandler ()
addRoles gid uid = do
    roles <- restCall $ R.GetGuildRoles gid
    case roles of
        Right r -> do
            let roleids = map roleId r
            forM_ roleids $ \role ->
                call $ R.AddGuildMemberRole gid uid role
        Left err -> do
            echo $ "Error getting guild member: " <> T.pack (show err)

warnMember :: UserId -> Text -> DiscordHandler ()
warnMember uid msg = do
  dm <- restCall $ R.CreateDM uid
  case dm of
    Right ch -> do
            let chid = channelId ch
            call $ R.CreateMessage chid msg
    Left err -> do
            echo $ "Error creating DM channel: " <> T.pack (show err)
