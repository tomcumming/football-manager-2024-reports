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
import Data.Tuple (swap)
import FM.Load qualified as FM
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    [] -> do
      ld <- FM.loadData "data"
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
      T.putStrLn "PLAYERS"
      nameAllPlayers ld
    _ -> fail "Just pass season folder name as single arg"

nameAllPlayers :: FM.LoadedData -> IO ()
nameAllPlayers =
  FM.ldMatches
    >>> M.elems
    >>> foldMap (FM.msPlayerStats >>> M.keysSet)
    >>> mapM_ T.putStrLn

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

goalPointsChart :: FM.LoadedData -> IO ()
goalPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      ( namedChances
          >>> (M.!? FM.Goal)
          >>> fromMaybe mempty
          >>> fmap chanceScores
          >>> M.unionsWith (+)
      )

chancesPointsChart :: FM.LoadedData -> IO ()
chancesPointsChart =
  renderChart ["Name", "Mins", "Score", "Per90"]
    . performanceChart
      ( namedChances
          >>> M.toList
          >>> concatMap
            ( \(ct, ps) ->
                fmap
                  (chanceScores >>> fmap (chanceTypeMultiple ct *))
                  ps
            )
          >>> M.unionsWith (+)
      )

namedChances :: FM.MatchStats -> M.Map FM.ChanceType [[T.Text]]
namedChances FM.MatchStats {..} =
  msChances
    & fmap
      ( fmap
          ( fmap
              ( \n ->
                  fromMaybe
                    ("Unknown: " <> tShow n)
                    (names M.!? n)
              )
          )
      )
  where
    names :: M.Map FM.PlayerNumber T.Text
    names =
      FM.psNumber <$> msPlayerStats
        & M.toList
        & fmap swap
        & M.fromList

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
        FM.ldMatches ld
          & fmap
            ( \ms ->
                FM.msPlayerStats ms
                  & fmap (\ps -> [(FM.psMinutes ps, FM.psPasses ps & FM.talCompleted)])
            )
          & M.unionsWith (<>)
          & M.mapMaybe NE.nonEmpty
          & fmap weightedMean
  let mins =
        FM.ldMatches ld
          & fmap
            ( \ms ->
                FM.msPlayerStats ms
                  & fmap FM.psMinutes
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
  (FM.MatchStats -> M.Map T.Text Double) ->
  FM.LoadedData ->
  [[T.Text]]
performanceChart calc =
  FM.ldMatches >>> \mss ->
    let scores =
          mss
            & fmap calc
            & M.unionsWith (+)
        mins =
          mss
            & fmap
              ( \ms ->
                  FM.msPlayerStats ms
                    & fmap FM.psMinutes
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
