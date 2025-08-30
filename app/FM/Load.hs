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
import Control.Monad ((>=>), unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Foldable (forM_, traverse_)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Tuple qualified as Tuple
import Debug.Trace (traceShowM)
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import qualified Data.Aeson.KeyMap as KM

matchMinutes :: Double
matchMinutes = 90 + 4

newtype PlayerNumber = PlayerNumber Int
  deriving (Eq, Ord)
  deriving newtype (Show, Aeson.FromJSONKey, Aeson.FromJSON)

newtype LoadedData = LoadedData
  { ldMatches :: M.Map Day MatchStats
  }
  deriving (Show)

data Tally = Tally
  { talAttempted :: Int,
    talCompleted :: Double
  }
  deriving (Show)

zeroTally :: Tally
zeroTally = Tally 0 0

tallySuccess :: Tally -> Double
tallySuccess Tally {..} = realToFrac talAttempted * talCompleted

tallyFailed :: Tally -> Double
tallyFailed Tally {..} = realToFrac talAttempted * (1 - talCompleted)

data PlayerStats = PlayerStats
  { psNumber :: PlayerNumber,
    psMinutes :: Double,
    psPasses :: Tally,
    psTackles :: Tally,
    psAerial :: Tally
  }
  deriving (Show)

data MatchStats = MatchStats
  { msOpponent :: Opponent,
    msPlayerStats :: M.Map T.Text PlayerStats,
    msChances :: M.Map ChanceType [[PlayerNumber]]
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

instance Aeson.FromJSON MatchStats where
  parseJSON = Aeson.withObject "root" $ \o -> do
    oppName <- o Aeson..: "opponent"
    oppStrength <- o Aeson..: "strength"
    let msOpponent = Opponent {..}
    msPlayerStats <- traverse parsePlayerStats =<< o Aeson..: "players"
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

parsePlayerStats :: Aeson.Object -> Aeson.Parser PlayerStats
parsePlayerStats o = do
  row :: Int <- o Aeson..: "row"
  psNumber :: PlayerNumber <- o Aeson..: "number"
  psMinutes <- rowMinutes row <$> o Aeson..:? "mins"
  psPasses <-
    fromMaybe zeroTally
      <$> ( liftA2 Tally
              <$> (o Aeson..:? "passAtt")
              <*> (fmap asRatio <$> (o Aeson..:? "passCmp%"))
          )
  psTackles <-
    fromMaybe zeroTally
      <$> ( liftA2 Tally
              <$> (o Aeson..:? "tackAtt")
              <*> (fmap asRatio <$> (o Aeson..:? "tackCmp%"))
          )
  psAerial <-
    fromMaybe zeroTally
      <$> ( liftA2 Tally
              <$> (o Aeson..:? "aerAtt")
              <*> (fmap asRatio <$> (o Aeson..:? "aerCmp%"))
          )
  sanityCheckNumbers o
  pure PlayerStats {..}
  where
    rowMinutes :: Int -> Maybe Int -> Double
    rowMinutes row = \case
      Just n
        | row >= 11 -> max 3 $ matchMinutes - realToFrac n
        | otherwise -> realToFrac n
      Nothing
        | row >= 11 -> 0
        | otherwise -> 90

    asRatio :: Int -> Double
    asRatio n = realToFrac n / 100

sanityCheckNumbers :: Aeson.Object -> Aeson.Parser ()
sanityCheckNumbers = KM.toList
  >>> traverse_ (\(k, v) -> Aeson.modifyFailure (("In " <> show k) <>) $ do
    n :: Int <- Aeson.parseJSON v
    let maxv = if k == "mins" then 200 else 100
    unless (n >= 0 && n <= maxv) $ fail $ "Out of sensible range: " <> show n)

loadData :: FilePath -> IO LoadedData
loadData rootPath = do
  ldMatches <-
    listDirectory (rootPath </> "matches")
      >>= (pure . fmap ((rootPath </> "matches") </>))
      >>= traverse readMatchStats
      >>= pure . M.fromList

  reportDupNumbers ldMatches
  reportChangedNumbers ldMatches

  pure $ LoadedData {..}

reportDupNumbers :: M.Map Day MatchStats -> IO ()
reportDupNumbers ldMatches = do
  forM_ (M.toList ldMatches) $ \(d, ms) -> do
    let numbers =
          psNumber <$> msPlayerStats ms
            & M.toList
            & fmap (first S.singleton)
            & fmap Tuple.swap
            & M.fromListWith (<>)
            & M.filter (S.size >>> (/= 1))
    forM_ (M.toList numbers) $ \(n, ns) ->
      fail $
        unwords ["Players share number", show d, show n, show ns]

reportChangedNumbers :: M.Map Day MatchStats -> IO ()
reportChangedNumbers = go mempty
  where
    go :: M.Map T.Text PlayerNumber -> M.Map Day MatchStats -> IO ()
    go current =
      M.minViewWithKey >>> \case
        Nothing -> pure ()
        Just ((d, next), rest) -> do
          let nextNames = psNumber <$> msPlayerStats next
          let current' = nextNames <> current

          let changes =
                M.intersectionWith
                  ( \n1 n2 ->
                      if n1 == n2
                        then Nothing
                        else Just (n1, n2)
                  )
                  current
                  nextNames
                  & M.mapMaybe id

          forM_ (M.toList changes) $ \(n, (n1, n2)) ->
            putStrLn $
              unwords
                ["On", show d, T.unpack n, "changed number", show n1, "to", show n2]

          go current' rest

readMatchStats :: FilePath -> IO (Day, MatchStats)
readMatchStats path = do
  day :: Day <- iso8601ParseM (takeBaseName path)
  Aeson.eitherDecodeFileStrict path
    >>= either fail (pure . (day,))
