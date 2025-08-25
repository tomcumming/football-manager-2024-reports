module Main where

import Control.Monad (forM_)
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (Day)
import FM.Load qualified as FM

main :: IO ()
main = do
  FM.LoadedData {..} <- FM.loadData "data"

  attRows <- reverse <$> makeRows ldPlayers ldMatches FM.msAttacking

  T.putStrLn ""
  T.putStrLn "Attacking"
  T.putStrLn $ T.intercalate "\t" ["Name", "Score", "Mins"]
  forM_ attRows $ \(name, (pn, Performance {..})) -> do
    let r = 90 * perfPoints / perfMins
    T.putStrLn $ T.intercalate "\t" [tShow pn, name, tShow r, tShow perfMins]

  defRows <- makeRows ldPlayers ldMatches FM.msDefending

  T.putStrLn ""
  T.putStrLn "Defending"
  T.putStrLn $ T.intercalate "\t" ["Name", "Score", "Mins"]
  forM_ defRows $ \(name, (pn, Performance {..})) -> do
    let r = 90 * perfPoints / perfMins
    T.putStrLn $ T.intercalate "\t" [tShow pn, name, tShow r, tShow perfMins]
  where
    makeRows players matches f =
      List.sortOn (snd . snd)
        . M.toList
        . M.unionsWith (\(_, p1) (pn, p2) -> (pn, p1 <> p2))
        <$> M.traverseWithKey (go f players) matches

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

pointsTypeMultiplier :: FM.PointsType -> Double
pointsTypeMultiplier _ = 1

go ::
  (FM.MatchStats -> M.Map FM.PointsType [M.Map FM.PlayerNumber Double]) ->
  M.Map Day (M.Map FM.PlayerNumber T.Text) ->
  Day ->
  FM.MatchStats ->
  IO (M.Map T.Text (FM.PlayerNumber, Performance))
go mode playerNums d ms@FM.MatchStats {..} = do
  M.fromList <$> traverse goMinutes (M.toList msMinutes)
  where
    points = M.unionsWith (+) $ M.mapWithKey goPoints $ mode ms

    goPoints ::
      FM.PointsType ->
      [M.Map FM.PlayerNumber Double] ->
      M.Map FM.PlayerNumber Double
    goPoints pt ps = (pointsTypeMultiplier pt *) <$> M.unionsWith (+) ps

    goMinutes ::
      (FM.PlayerNumber, Double) ->
      IO (T.Text, (FM.PlayerNumber, Performance))
    goMinutes (pn, perfMins) = do
      name <-
        maybe (fail $ "Could not loopup player: " <> show (pn, d)) pure $
          M.lookupLE d playerNums >>= M.lookup pn . snd
      let perfPoints = fromMaybe 0 $ points M.!? pn
      pure (name, (pn, Performance {..}))
