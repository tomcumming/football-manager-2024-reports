module Main (main) where

import Control.Category ((>>>))
import Control.Monad (when)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Text.Read (readMaybe)

main :: IO ()
main = T.getContents >>= parsePrinted >>= printParsed

printParsed :: M.Map Int Player -> IO ()
printParsed = makeTemplate >>> Aeson.encode >>> BS.putStrLn

makeTemplate :: M.Map Int Player -> M.Map T.Text Aeson.Value
makeTemplate players =
  M.fromList
    [ ( "opponent",
        Aeson.toJSON $
          M.fromList
            [ ("name" :: T.Text, Aeson.toJSON @T.Text ""),
              ("strength", Aeson.toJSON @Double 0)
            ]
      ),
      ("attacking", chancesTemplate),
      ("defending", chancesTemplate),
      ( "minutes",
        Aeson.toJSON $
          M.fromList @T.Text @T.Text
            [ ("numbers", ""),
              ("minutes", "")
            ]
      ),
      ("players", Aeson.toJSON players)
    ]
  where
    chancesTemplate =
      Aeson.toJSON $
        M.fromList
          [ ("goal" :: T.Text, [] :: [T.Text]),
            ("ccc" :: T.Text, [] :: [T.Text]),
            ("half" :: T.Text, [] :: [T.Text])
          ]

data Player = Player
  { plUid :: T.Text,
    plName :: T.Text
  }

instance Aeson.ToJSON Player where
  toJSON Player {..} =
    Aeson.toJSON $
      M.fromList
        [("uid" :: T.Text, plUid), ("name", plName)]

parsePrinted :: T.Text -> IO (M.Map Int Player)
parsePrinted =
  T.lines
    >>> fmap T.strip
    >>> zip [1 ..]
    >>> filter (snd >>> T.null >>> not)
    >>> fmap (second (T.dropWhile (== '|') >>> T.dropWhileEnd (== '|')))
    >>> parseHeadings
    >>> (>>= uncurry parseRows)

lookupCell :: Int -> M.Map T.Text T.Text -> T.Text -> IO T.Text
lookupCell line cells name = case cells M.!? name of
  Nothing -> fail $ unwords ["cant find", T.unpack name, "on line", show line]
  Just v -> pure v

parseRows :: [T.Text] -> [(Int, T.Text)] -> IO (M.Map Int Player)
parseRows headings = \case
  [] -> pure mempty
  (line, rowStr) : blanks : rest -> do
    let cells =
          T.splitOn "|" rowStr
            & fmap T.strip
            & zip headings
            & M.fromList
    no <-
      lookupCell line cells "No." >>= \case
        noStr | Just no <- readMaybe (T.unpack noStr) -> pure no
        _ -> fail $ "Cant read No. on " <> show line
    plUid <- lookupCell line cells "UID"
    plName <- lookupCell line cells "Name"
    uncurry checkSpacer blanks
    (M.singleton no Player {..} <>) <$> parseRows headings rest
  ls -> fail $ show $ take 2 ls

parseHeadings :: [(Int, T.Text)] -> IO ([T.Text], [(Int, T.Text)])
parseHeadings = \case
  (line, headingsRow) : blanks : rest -> do
    let headings = T.splitOn "|" headingsRow & fmap T.strip
    when (null headings) $ fail $ "Can't read headings on " <> show line
    uncurry checkSpacer blanks
    pure (headings, rest)
  ls -> fail $ show $ take 2 ls

checkSpacer :: Int -> T.Text -> IO ()
checkSpacer line =
  T.strip >>> T.all (== '-') >>> \case
    True -> pure ()
    False -> fail $ "Line not a spacer: " <> show line
