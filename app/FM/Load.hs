module FM.Load
  ( PlayerNumber (..),
    LoadedData (..),
    MatchStats (..),
    Opponent (..),
    PointsType (..),
    loadData,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as M
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)

matchMinutes :: Double
matchMinutes = 90 + 4

newtype PlayerNumber = PlayerNumber Int
  deriving (Eq, Ord)
  deriving newtype (Show, Aeson.FromJSONKey)

data LoadedData = LoadedData
  { ldPlayers :: M.Map Day (M.Map PlayerNumber T.Text),
    ldMatches :: M.Map Day MatchStats
  }
  deriving (Show)

data MatchStats = MatchStats
  { msOpponent :: Opponent,
    msMinutes :: M.Map PlayerNumber Double,
    -- | More is bad
    msDefending :: M.Map PointsType [M.Map PlayerNumber Double],
    msAttacking :: M.Map PointsType [M.Map PlayerNumber Double]
  }
  deriving (Show)

data Opponent = Opponent
  { oppName :: T.Text,
    oppStrength :: Rational
  }
  deriving (Show)

data PointsType
  = Goal
  | ClearCutChance
  | HalfChance
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON PointsType where
  parseJSON = Aeson.withText "PointsType" $ \case
    "goal" -> pure Goal
    "ccc" -> pure ClearCutChance
    "half" -> pure HalfChance
    txt -> fail $ "Unknown points type: " <> T.unpack txt

instance Aeson.FromJSONKey PointsType where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ Aeson.parseJSON . Aeson.toJSON

instance Aeson.FromJSON MatchStats where
  parseJSON = Aeson.withObject "root" $ \o -> do
    msMinutes <- fmap goNegativeMinutes <$> o Aeson..: "minutes-played"
    msOpponent <- o Aeson..: "opposition"
    msDefending <- o Aeson..: "defending"
    msAttacking <- o Aeson..: "attacking"
    pure MatchStats {..}
    where
      goNegativeMinutes :: Double -> Double
      goNegativeMinutes m
        | m < 0 = matchMinutes + m
        | otherwise = m

instance Aeson.FromJSON Opponent where
  parseJSON = Aeson.withObject "Opponent" $ \o ->
    Opponent
      <$> o Aeson..: "name"
      <*> (parseStrength =<< o Aeson..: "strength")
    where
      parseStrength :: T.Text -> Aeson.Parser Rational
      parseStrength txt
        | [s1, s2] <- T.splitOn " / " txt,
          Just n1 <- readMaybe (T.unpack s1),
          Just n2 <- readMaybe (T.unpack s2) =
            pure $ (n2 - pred n1) % n2
        | otherwise = fail $ "Invalid strength: " <> T.unpack txt

loadData :: FilePath -> IO LoadedData
loadData rootPath = do
  ldPlayers <-
    listDirectory (rootPath </> "players")
      >>= (pure . fmap ((rootPath </> "players") </>))
      >>= traverse readPlayerNames
      >>= pure . M.fromList
  ldMatches <-
    listDirectory (rootPath </> "matches")
      >>= (pure . filter (/= "template.json"))
      >>= (pure . fmap ((rootPath </> "matches") </>))
      >>= traverse readMatchStats
      >>= pure . M.fromList

  pure $ LoadedData {..}

readMatchStats :: FilePath -> IO (Day, MatchStats)
readMatchStats path = do
  day :: Day <- iso8601ParseM (takeBaseName path)
  Aeson.eitherDecodeFileStrict path
    >>= either fail (pure . (day,))

readPlayerNames :: FilePath -> IO (Day, M.Map PlayerNumber T.Text)
readPlayerNames path = do
  day :: Day <- iso8601ParseM (takeBaseName path)
  Aeson.eitherDecodeFileStrict path
    >>= either fail (pure . (day,)) . (>>= Aeson.parseEither parseNames)
  where
    parseNames :: Aeson.Value -> Aeson.Parser (M.Map PlayerNumber T.Text)
    parseNames = Aeson.withObject "root" $ \o -> do
      o Aeson..: "names"
