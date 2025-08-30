module Main where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FM.Load qualified as FM
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [season] -> do
      ld <- FM.loadData "data" season
      T.putStrLn "GOALS"
      goalPointsChart ld
      T.putStrLn "CHANCES"
      chancesPointsChart ld
      T.putStrLn "AERIALS WON"
      aerialsWonPointsChart ld
      T.putStrLn "AERIALS LOST"
      aerialsLostPointsChart ld
      T.putStrLn "TACKLES WON"
      tacklesWonPointsChart ld
      T.putStrLn "TACKLES LOST"
      tacklesLostPointsChart ld
      T.putStrLn "PASSING"
      passingPointsChart ld
    _ -> fail "Just pass season folder name as single arg"

tShow :: (Show a) => a -> T.Text
tShow = T.pack . show

data Performance = Performance
  { perfMins :: Double,
    perfPoints :: Double
  }
  deriving (Eq)

instance Ord Performance where
  compare p1 p2 = compare (r1, perfMins p1) (r2, perfMins p2)
    where
      r1 = perfPoints p1 / perfMins p1
      r2 = perfPoints p2 / perfMins p2

instance Semigroup Performance where
  p1 <> p2 =
    Performance
      (perfMins p1 + perfMins p2)
      (perfPoints p1 + perfPoints p2)

matchesWithPlayers :: FM.LoadedData -> [(M.Map FM.PlayerNumber T.Text, FM.MatchStats)]
matchesWithPlayers FM.LoadedData {..} = do
  (d, m) <- M.toList ldMatches
  (_, ns) <-
    M.lookupLE d ldPlayers
      & maybe (error $ "Can't find player names for " <> show d) pure
  pure (ns, m)

goalPointsChart :: FM.LoadedData -> IO ()
goalPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      ( FM.msChances
          >>> (M.!? FM.Goal)
          >>> fromMaybe mempty
          >>> fmap chanceScores
          >>> M.unionsWith (+)
      )

chancesPointsChart :: FM.LoadedData -> IO ()
chancesPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      ( FM.msChances
          >>> M.toList
          >>> concatMap
            ( \(ct, ps) ->
                fmap
                  (chanceScores >>> fmap (chanceTypeMultiple ct *))
                  ps
            )
          >>> M.unionsWith (+)
      )

aerialsWonPointsChart :: FM.LoadedData -> IO ()
aerialsWonPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      (FM.msPlayerStats >>> fmap (FM.psAerial >>> FM.tallySuccess))

aerialsLostPointsChart :: FM.LoadedData -> IO ()
aerialsLostPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      (FM.msPlayerStats >>> fmap (FM.psAerial >>> FM.tallyFailed))

tacklesWonPointsChart :: FM.LoadedData -> IO ()
tacklesWonPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      (FM.msPlayerStats >>> fmap (FM.psTackles >>> FM.tallySuccess))

tacklesLostPointsChart :: FM.LoadedData -> IO ()
tacklesLostPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      (FM.msPlayerStats >>> fmap (FM.psTackles >>> FM.tallyFailed))

passingPointsChart :: FM.LoadedData -> IO ()
passingPointsChart ld = do
  let comps =
        matchesWithPlayers ld
          & fmap
            ( \(ns, ms) ->
                FM.msPlayerStats ms
                  & fmap (\ps -> [(FM.psMinutes ps, FM.psPasses ps & FM.talCompleted)])
                  & M.mapKeys (\k -> fromMaybe ("Unknown: " <> tShow k) $ ns M.!? k)
            )
          & M.unionsWith (<>)
          & M.mapMaybe NE.nonEmpty
          & fmap weightedMean
  let mins =
        matchesWithPlayers ld
          & fmap
            ( \(ns, ms) ->
                FM.msPlayerStats ms
                  & fmap FM.psMinutes
                  & M.mapKeys (\k -> fromMaybe ("Unknown: " <> tShow k) $ ns M.!? k)
            )
          & M.unionsWith (+)
          & M.filter (> 0)
  let rows =
        M.intersectionWithKey
          (\n m c -> ((n, m), c))
          mins
          comps
          & M.elems
          & List.sortOn snd
          & reverse
          & fmap (\((n, m), c) -> [n, tShow m, tShow (c * 100)])
  renderChart ["Name", "Mins", "Passing%"] rows

weightedMean :: NE.NonEmpty (Double, Double) -> Double
weightedMean xs = totalSum / totalWeight
  where
    totalWeight = sum $ fst <$> xs
    totalSum = sum $ uncurry (*) <$> xs

chanceTypeMultiple :: FM.ChanceType -> Double
chanceTypeMultiple = \case
  FM.Goal -> 1
  FM.ClearCutChance -> 0.75
  FM.HalfChance -> 0.5

performanceChart ::
  (FM.MatchStats -> M.Map FM.PlayerNumber Double) ->
  FM.LoadedData ->
  [[T.Text]]
performanceChart calc =
  matchesWithPlayers >>> \mss ->
    let scores =
          mss
            & fmap
              ( \(ns, ms) ->
                  calc ms
                    & M.mapKeys (\k -> fromMaybe ("Unknown: " <> tShow k) $ ns M.!? k)
              )
            & M.unionsWith (+)
        mins =
          mss
            & fmap
              ( \(ns, ms) ->
                  FM.msPlayerStats ms
                    & fmap FM.psMinutes
                    & M.mapKeys (\k -> fromMaybe ("Unknown: " <> tShow k) $ ns M.!? k)
              )
            & M.unionsWith (+)
            & M.filter (> 0)
        perfs =
          M.mapWithKey
            (\n m -> Performance m $ fromMaybe 0 $ scores M.!? n)
            mins
     in M.toList perfs
          & List.sortOn snd
          & reverse
          & fmap
            ( \(n, Performance {..}) ->
                [ n,
                  tShow perfMins,
                  tShow perfPoints,
                  tShow (90 * perfPoints / perfMins)
                ]
            )

renderChart ::
  [T.Text] ->
  [[T.Text]] ->
  IO ()
renderChart hs rs = do
  T.putStrLn $ T.intercalate "\t" hs
  forM_ rs (T.putStrLn . T.intercalate "\t")
  T.putStrLn ""

chanceScores :: (Ord a) => [a] -> M.Map a Double
chanceScores = flip zip chanceScales >>> M.fromListWith (\_ s -> s)

chanceScales :: [Double]
chanceScales = (1 :) $ (1 /) . (2 **) <$> [0 ..]
