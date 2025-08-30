module Main (main) where

import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forM, forM_, void)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (fold)
import Data.Map qualified as M
import Data.Text qualified as T
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode, system)
import Text.Read (readMaybe)

dumpPath :: FilePath
dumpPath = "dumped-image-sections"

firstRow :: Int
firstRow = 326

rowHeight :: Int
rowHeight = 56

cells :: [(T.Text, Int, Int, Int)]
cells =
  [ ("mins", 200, 40, 25),
    ("passAtt", 1284, cellWidth, cellHeight),
    ("passCmp%", 1354, cellWidth, cellHeight),
    ("keyPass", 1424, cellWidth, cellTallHeight),
    ("cccPass", 1488, cellWidth, cellTallHeight),
    ("crossAtt", 1560, cellWidth, cellHeight),
    ("crossCmp%", 1638, cellBigWidth, cellHeight),
    ("tackAtt", 1730, cellWidth, cellHeight),
    ("tackCmp%", 1798, cellBigWidth, cellHeight),
    ("keyTack", 1872, cellWidth, cellTallHeight),
    ("foulTack", 1936, cellWidth, cellTallHeight),
    ("intTack", 2000, cellWidth, cellTallHeight),
    ("clrTack", 2062, cellSmallWidth, cellTallHeight),
    ("aerAtt", 2114, cellWidth, cellHeight),
    ("aerCmp%", 2190, cellBigWidth, cellHeight),
    ("keyHeader", 2274, cellSmallWidth, cellTallHeight),
    ("dribble", 2336, cellWidth, cellTallHeight),
    ("shotAtt", 2418, cellWidth, cellHeight),
    ("shotOnt%", 2495, cellBigWidth, cellHeight)
  ]
  where
    cellHeight = 24
    cellTallHeight = 30
    cellWidth = 60
    cellBigWidth = 66
    cellSmallWidth = 40

main :: IO ()
main =
  getArgs >>= \case
    [rowsStr, imgPath]
      | Just rows <- readMaybe rowsStr -> do
          void $ system $ "mkdir -p " <> dumpPath
          -- prs <- forM [0.. pred rows] $ \row -> do
          prs <- fmap M.fromList $ forConcurrently [0 .. pred rows] $ \row -> do
            pr <- readPlayerRow imgPath row
            hPutStrLn stderr $
              unwords
                [ "Done row",
                  show (succ row),
                  T.unpack $ fst pr
                ]
            pure pr
          BS.putStrLn $ Aeson.encode prs
    _args -> do
      putStrLn "Expected args: <row-count> <img-path>"

readPlayerRow :: FilePath -> Int -> IO (T.Text, M.Map T.Text Aeson.Value)
readPlayerRow imgPath row = do
  let rowTop = firstRow + rowHeight * row
  name <- ocrText imgPath 358 rowTop 500 30 False

  cellVals <- fmap M.fromList <$> forM cells $ \(n, l, w, h) -> do
    txt <- ocrText imgPath l rowTop w h True
    v <- case txt of
      "" -> pure Nothing
      "-" -> pure Nothing
      _
        | [(x :: Int, "")] <- reads (T.unpack txt) ->
            pure $ Just $ Aeson.toJSON x
      _
        | [(x :: Int, "...")] <- reads (T.unpack txt) ->
            pure $ Just $ Aeson.toJSON x
      _
        | [(x :: Int, "%")] <- reads (T.unpack txt) ->
            pure $ Just $ Aeson.toJSON x
      _ ->
        fail $
          unwords
            [ "Could not parse cell",
              show row,
              T.unpack n,
              show l <> "x" <> show rowTop,
              "=",
              show txt
            ]
    pure (n, v)

  pure
    ( name,
      M.insert "row" (Aeson.toJSON row) $ M.mapMaybe id cellVals
    )

cropImage :: FilePath -> Int -> Int -> Int -> Int -> IO FilePath
cropImage path x y w h = do
  let outPath = dumpPath </> (show x <> "x" <> show y <> ".png")
  (ec, _stdo, stde) <-
    readProcessWithExitCode
      "magick"
      [ path,
        "-crop",
        fold [show w, "x", show h, "+", show x, "+", show y],
        -- , "-channel"
        -- , "RGB"
        -- , "-threshold"
        -- , "50%"
        outPath
      ]
      ""
  case ec of
    ExitSuccess -> pure outPath
    ExitFailure {} -> fail stde

ocrText :: FilePath -> Int -> Int -> Int -> Int -> Bool -> IO T.Text
ocrText path x y w h isNumber = do
  let numberFlags = if isNumber then ["numbers-config"] else []
  croppedPath <- cropImage path x y w h
  (ec, stdo, stde) <-
    readProcessWithExitCode
      "tesseract"
      ( [ "-l",
          "eng",
          "--psm",
          "7",
          croppedPath,
          "stdout"
        ]
          <> numberFlags
      )
      ""
  case ec of
    ExitSuccess -> pure $ T.strip $ T.pack stdo
    ExitFailure {} -> fail stde
