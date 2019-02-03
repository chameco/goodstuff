{-# LANGUAGE QuasiQuotes #-}

module Good.Services.Coal.Snapshot.Info where

import Good.Prelude

import Data.Text (strip, splitOn)

import Text.Regex.PCRE.Heavy

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import Good.Architecture.Scraper
import Good.Architecture.Scrapers.Native
import Good.Interfaces.Log
import Good.Services.Coal
import Good.Services.Coal.Snapshot.Types

showplayer :: (MonadIO m, MonadCatch m) => Text -> Scraping Native m HTML
showplayer playerid = getHTML . toSL $ mconcat ["https://www.kingdomofloathing.com/showplayer.php?who=", playerid]

playerName :: HTML -> Text
playerName = innerText . take 1 . drop 1 . dropWhile (~/= ("<b>" :: String)) . dropWhile (~/= ("<body>" :: String))

playerID :: MonadThrow m => HTML -> m Text
playerID page = case headMay $ scan [re| \(#([0-9]+)\)|] s of
                  Just (_, [pid]) -> pure $ toSL pid
                  _ -> throwM $ KOLError "Could not extract player ID from showplayer.php"
  where s = innerText . take 1 . drop 1 . dropWhile (~/= ("</b>" :: String)) $ dropWhile (~/= ("<td valign=center>" :: String)) page

playerTitle :: HTML -> Text
playerTitle = innerText . take 1 . dropWhile (not . tagText (const True)) . dropWhile (~/= ("<br>" :: String)) . dropWhile (~/= ("<td valign=center>" :: String))

playerAvatar :: MonadThrow m => HTML -> m Text
playerAvatar page =
  case fmap (fromAttrib "src") . headMay . dropWhile (~/= ("<img>" :: String)) $ dropWhile (~/= ("<div class=''>" :: String)) page of
    Just a -> pure a
    _ -> throwM $ KOLError "Could not extract player avatar from showplayer.php"

playerSkills :: HTML -> [Skill]
playerSkills page = fmap (\s -> Skill (pullName s) (pullPerm s))
                    . sections (~== ("<tr class='blahblah'>" :: String))
                    $ dropWhile (~/= ("<a id='toggleskillsokplease'>" :: String)) page
  where cleanName :: Text -> Text
        cleanName = strip . fromMaybe "" . headMay . splitOn "("
        pullName :: [Tag Text] -> Text
        pullName = cleanName . innerText . take 1 . dropWhile (not . tagText (const True))
        perm :: Text -> Bool
        perm "HP" = True
        perm _ = False
        pullPerm :: [Tag Text] -> Bool
        pullPerm = perm . innerText . take 2 . dropWhile (tagText (== " (")) . dropWhile (not . tagText (const True)) . drop 1 . dropWhile (not . tagText (const True))

scrapeInfo :: (MonadIO m, MonadCatch m) => Text -> Text -> Text -> m Info
scrapeInfo botuser botpass playerid = scraping $ do
  notify $ "Scraping info for " <> playerid
  login botuser botpass
  infoTime <- liftIO getCurrentTime
  page <- showplayer playerid
  let name = playerName page
      title = playerTitle page
      skills = playerSkills page
  avatar <- playerAvatar page
  pure $ Info {..}
