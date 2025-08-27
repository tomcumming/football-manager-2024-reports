module FM.Load
  ( PlayerNumber (..),
    LoadedData (..),
    MatchStats (..),
    Opponent (..),
    ChanceType (..),
    PlayerStats (..),
    Tally (..),
    loadData,
    tallySuccess,
    tallyFailed,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Char (isSpace)
import Data.List (zipWith4)
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
  deriving newtype (Show, Aeson.FromJSONKey, Aeson.FromJSON)

data LoadedData = LoadedData
  { ldPlayers :: M.Map Day (M.Map PlayerNumber T.Text),
    ldMatches :: M.Map Day MatchStats
  }
  deriving (Show)

data Tally = Tally
  { talAttempted :: Int,
    talCompleted :: Double
  }
  deriving (Show)

tallySuccess :: Tally -> Double
tallySuccess Tally {..} = realToFrac talAttempted * talCompleted

tallyFailed :: Tally -> Double
tallyFailed Tally {..} = realToFrac talAttempted * (1 - talCompleted)

data PlayerStats = PlayerStats
  { psMinutes :: Double,
    psPasses :: Tally,
    psTackles :: Tally,
    psAerial :: Tally
  }
  deriving (Show)

data MatchStats = MatchStats
  { msOpponent :: Opponent,
    msPlayerStats :: M.Map PlayerNumber PlayerStats,
    msChances :: M.Map ChanceType [[PlayerNumber]]
  }
  deriving (Show)

data Opponent = Opponent
  { oppName :: T.Text,
    oppStrength :: Rational
  }
  deriving (Show)

data ChanceType
  = Goal
  | ClearCutChance
  | HalfChance
  deriving (Eq, Ord, Show)

instance Aeson.FromJSON ChanceType where
  parseJSON = Aeson.withText "ChanceType" $ \case
    "goal" -> pure Goal
    "ccc" -> pure ClearCutChance
    "half" -> pure HalfChance
    txt -> fail $ "Unknown chance type: " <> T.unpack txt

instance Aeson.FromJSONKey ChanceType where
  fromJSONKey = Aeson.FromJSONKeyTextParser $ Aeson.parseJSON . Aeson.toJSON

instance Aeson.FromJSON MatchStats where
  parseJSON = Aeson.withObject "root" $ \o -> do
    msOpponent <- o Aeson..: "opposition"
    msPlayerStats <- parsePlayerStats =<< o Aeson..: "players"
    msChances <- parseChances =<< o Aeson..: "chances"
    pure MatchStats {..}

parseChances :: Aeson.Value -> Aeson.Parser (M.Map ChanceType [[PlayerNumber]])
parseChances =
  Aeson.parseJSON @(M.Map ChanceType [T.Text])
    >=> traverse (traverse parseSpaceSeperated)

parseSpaceSeperated :: (Aeson.FromJSON a) => T.Text -> Aeson.Parser [a]
parseSpaceSeperated =
  T.split isSpace
    >>> traverse
      (Aeson.eitherDecodeStrictText >>> either fail pure)

parseTallys :: Aeson.Object -> Aeson.Parser [Tally]
parseTallys o = do
  attempted <- parseSpaceSeperated =<< o Aeson..: "attempted"
  completed <- parseSpaceSeperated =<< o Aeson..: "completed%"
  unless (length attempted == length completed) $
    fail "parseTallys column mismatch"

  pure $ zipWith Tally attempted (asRatio <$> completed)
  where
    asRatio :: Int -> Double
    asRatio n = realToFrac n / 100

parsePlayerStats :: Aeson.Object -> Aeson.Parser (M.Map PlayerNumber PlayerStats)
parsePlayerStats o = do
  numbers <- parseSpaceSeperated =<< o Aeson..: "numbers"
  minutes <- parseSpaceSeperated =<< o Aeson..: "minutes"
  passes <- parseTallys =<< o Aeson..: "passes"
  tackles <- parseTallys =<< o Aeson..: "tackles"
  aerials <- parseTallys =<< o Aeson..: "aerials"

  let ps =
        zipWith4
          PlayerStats
          (goNegativeMinutes <$> minutes)
          passes
          tackles
          aerials

  unless (length numbers == length ps) $
    fail "Mismatching columns in player stats"

  pure $ M.fromList $ zip numbers ps
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
