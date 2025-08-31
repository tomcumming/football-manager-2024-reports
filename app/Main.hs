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
    [] -> do
      ld <- FM.loadData "data"
      T.putStrLn "GOAL CONTRIBUTION"
      goalPointsChart ld
      T.putStrLn "CHANCE CONTRIBUTION"
      chancesPointsChart ld
      T.putStrLn "DEFENCE GOAL"
      goalDefencePointsChart ld
      T.putStrLn "DEFENCE CHANCE"
      chanceDefencePointsChart ld
    _ -> fail "Just pass season folder name as single arg"

goalPointsChart :: FM.LoadedData -> IO ()
goalPointsChart ld = do
  let rows = flip reportFromMatchScore ld $ \FM.MatchStats {msAttacking} ->
        let goals = fromMaybe mempty $ msAttacking M.!? FM.Goal
         in M.unionsWith (+) $ chanceScores <$> goals
  renderChart
    ["No", "Name", "Mins", "Score", "Score / 90"]
    rows

chancesPointsChart :: FM.LoadedData -> IO ()
chancesPointsChart ld = do
  let rows = flip reportFromMatchScore ld $ \FM.MatchStats {msAttacking} ->
        M.toList msAttacking
          & concatMap
            ( \(ct, cs) ->
                cs
                  & fmap (chanceScores >>> fmap (* chanceTypeMultiple ct))
            )
          & M.unionsWith (+)
  renderChart
    ["No", "Name", "Mins", "Score", "Score / 90"]
    rows

goalDefencePointsChart :: FM.LoadedData -> IO ()
goalDefencePointsChart ld = do
  let rows = flip reportFromMatchScore ld $ \FM.MatchStats {msDefending} ->
        let goals = fromMaybe mempty $ msDefending M.!? FM.Goal
         in M.unionsWith (+) $ chanceScores <$> goals
  renderChart
    ["No", "Name", "Mins", "Score", "Score / 90"]
    rows

chanceDefencePointsChart :: FM.LoadedData -> IO ()
chanceDefencePointsChart ld = do
  let rows = flip reportFromMatchScore ld $ \FM.MatchStats {msDefending} ->
        M.toList msDefending
          & concatMap
            ( \(ct, cs) ->
                cs
                  & fmap (chanceScores >>> fmap (* chanceTypeMultiple ct))
            )
          & M.unionsWith (+)
  renderChart
    ["No", "Name", "Mins", "Score", "Score / 90"]
    rows

reportFromMatchScore ::
  (FM.MatchStats -> M.Map FM.PlayerNumber Double) ->
  FM.LoadedData ->
  [[T.Text]]
reportFromMatchScore calc =
  FM.ldMatches
    >>> M.elems
    >>> fmap
      ( \ms@FM.MatchStats {..} ->
          let scores = calc ms
              named =
                M.toList (M.intersectionWith (,) msMinutes scores)
                  & fmap
                    ( \(n, (m, s)) ->
                        M.singleton
                          ( msPlayers M.!? n
                              & maybe ("Unknown " <> tShow n) FM.pdName
                          )
                          (n, Performance m s)
                    )
                  & M.unionsWith comb
           in named
      )
    >>> M.unionsWith comb
    >>> M.toList
    >>> List.sortOn (snd >>> snd)
    >>> reverse
    >>> fmap
      ( \(na, (no, Performance m s)) ->
          [tShow no, na, tShow m, tShow s, tShow (s * 90 / m)]
      )
  where
    comb (n, s1) (_, s2) = (n, s1 <> s2)

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
