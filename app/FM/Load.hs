module FM.Load
  ( PlayerNumber (..),
    LoadedData (..),
    MatchStats (..),
    Opponent (..),
    ChanceType (..),
    PlayerDetails (..),
    loadData,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Char (isSpace)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))

matchMinutes :: Double
matchMinutes = 90 + 4

newtype PlayerNumber = PlayerNumber Int
  deriving (Eq, Ord)
  deriving newtype (Show, Aeson.FromJSONKey, Aeson.FromJSON)

newtype LoadedData = LoadedData
  { ldMatches :: M.Map Day MatchStats
  }
  deriving (Show)

data PlayerDetails = PlayerDetails
  { pdName :: T.Text,
    pdUid :: T.Text
  }
  deriving (Show)

data MatchStats = MatchStats
  { msOpponent :: Opponent,
    msMinutes :: M.Map PlayerNumber Double,
    msPlayers :: M.Map PlayerNumber PlayerDetails,
    msAttacking :: M.Map ChanceType [[PlayerNumber]],
    msDefending :: M.Map ChanceType [[PlayerNumber]]
  }
  deriving (Show)

data Opponent = Opponent
  { oppName :: T.Text,
    oppStrength :: Double
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

instance Aeson.FromJSON PlayerDetails where
  parseJSON = Aeson.withObject "PlayerDetails" $ \o -> do
    pdName <- o Aeson..: "name"
    pdUid <- o Aeson..: "uid"
    pure PlayerDetails {..}

instance Aeson.FromJSON Opponent where
  parseJSON = Aeson.withObject "Opponent" $ \o -> do
    oppName <- o Aeson..: "name"
    oppStrength <- o Aeson..: "strength"
    pure Opponent {..}

instance Aeson.FromJSON MatchStats where
  parseJSON = Aeson.withObject "root" $ \o -> do
    msOpponent <- o Aeson..: "opponent"
    msPlayers <- o Aeson..: "players"
    msAttacking <- parseChances =<< o Aeson..: "attacking"
    msDefending <- parseChances =<< o Aeson..: "defending"
    msMinutes <- parseMinutes =<< o Aeson..: "minutes"
    pure MatchStats {..}

parseMinutes :: Aeson.Value -> Aeson.Parser (M.Map PlayerNumber Double)
parseMinutes = Aeson.withObject "Minutes" $ \o -> do
  numbers <- parseSpaceSeperated =<< o Aeson..: "numbers"
  mins <- parseSpaceSeperated =<< o Aeson..: "minutes"
  unless (length numbers == length mins) $
    fail "Mismatching minutes"
  pure $ M.fromList $ zip numbers (flipMins <$> mins)
  where
    flipMins :: Double -> Double
    flipMins n
      | n < 0 = max 3 $ matchMinutes + n
      | otherwise = n

parseChances :: Aeson.Value -> Aeson.Parser (M.Map ChanceType [[PlayerNumber]])
parseChances =
  Aeson.parseJSON @(M.Map ChanceType [T.Text])
    >=> traverse (traverse parseSpaceSeperated)

parseSpaceSeperated :: (Aeson.FromJSON a) => T.Text -> Aeson.Parser [a]
parseSpaceSeperated =
  T.split isSpace
    >>> traverse
      (Aeson.eitherDecodeStrictText >>> either fail pure)

loadData :: FilePath -> IO LoadedData
loadData rootPath = do
  ldMatches <-
    listDirectory (rootPath </> "matches")
      >>= (pure . fmap ((rootPath </> "matches") </>))
      >>= traverse readMatchStats
      >>= pure . M.fromList
  pure $ LoadedData {..}

readMatchStats :: FilePath -> IO (Day, MatchStats)
readMatchStats path = do
  day :: Day <- iso8601ParseM (takeBaseName path)
  Aeson.eitherDecodeFileStrict path
    >>= either fail (pure . (day,))
