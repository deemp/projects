{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Lens (At (at), itraverse_, use, (%=), (&), (+=), (.=), (<>=), (?~), (^.))
import Control.Monad.State
import Converter (Format (Hs, Md), convertTo, dedent, def, disable, enable, indent)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Data.Yaml (FromJSON, ToJSON, decodeFileThrow)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)

newtype Path = Path {_path :: FilePath} deriving newtype (Eq, IsString)
instance Show Path where
  show :: Path -> String
  show = _path

data NodeSubDir = NodeSubDir {path :: Path, children :: Map.Map String NodeSubDir} deriving (Eq, Show)

addPath :: [String] -> Path -> NodeSubDir -> State Int NodeSubDir
addPath [] path n = error "can't construct a node without a module name."
addPath (x : xs) finalPath currentNode = do
  childNode <- case xs of
    [] -> pure NodeSubDir{path = finalPath, children = Map.empty}
    xs_ ->
      case currentNode.children ^. at x of
        Nothing -> do
          idx <- use id
          id += 1
          addPath xs_ finalPath NodeSubDir{path = [i|dummy/#{idx}|], children = Map.empty}
        Just n -> do
          addPath xs_ finalPath n
  pure NodeSubDir{children = currentNode.children & at x ?~ childNode, path = currentNode.path}

data NodeProject = NodeProject {heading :: Text.Text, nodes :: [NodeSubDir]}

mkNodesProject :: FilePath -> [Project] -> [NodeProject]
mkNodesProject prefix projects =
  flip evalState 0 $
    for
      projects
      ( \Project{..} -> do
          nodes <-
            for
              subDirs
              ( \SubDir{..} -> do
                  cnt <- use id
                  let (res_, cnt_) = flip execState (NodeSubDir{children = Map.empty, path = ""}, cnt) $
                        for hsModules $ \hsModule -> do
                          let xs = splitOn "/" hsModule
                          id %= \(n, c) -> runState (addPath xs (Path [i|#{prefix}/#{mdPrefix}/#{hsModule}.md|]) n) c
                  id .= cnt_
                  pure res_
              )
          pure NodeProject{heading, nodes}
      )

pPrintNodeSubDir :: Int -> NodeSubDir -> State [Text.Text] ()
pPrintNodeSubDir depth t =
  itraverse_
    ( \idx child -> do
        id <>= [[i|#{concat (replicate depth " ")}- [#{idx}](#{path child})|]]
        case child of
          NodeSubDir{} -> pPrintNodeSubDir (depth + 2) child
    )
    t.children

pPrintNodeProject :: NodeProject -> Text.Text
pPrintNodeProject t =
  Text.intercalate
    "\n"
    [ let h = t.heading in [i|\# #{h}\n|]
    , Text.intercalate "\n" $ (\x -> Text.intercalate "\n" (execState (pPrintNodeSubDir 0 x) [])) <$> t.nodes
    ]

pPrintProjects :: FilePath -> [Project] -> Text.Text
pPrintProjects prefix ns = Text.intercalate "\n\n" $ pPrintNodeProject <$> mkNodesProject prefix ns

newtype M = M Text.Text
instance Show M where
  show :: M -> String
  show (M s) = Text.unpack s

configPath :: FilePath
configPath = "make-docs/config.yaml"

docsPrefix :: FilePath
docsPrefix = "haskell"

t5 :: IO M
t5 = do
  projects <- decodeFileThrow configPath
  let prefix = docsPrefix
  pure $ M $ pPrintProjects prefix projects

data Project = Project {heading :: Text.Text, subDirs :: [SubDir]} deriving (Generic)
instance FromJSON Project
data SubDir = SubDir {hsPrefix :: FilePath, mdPrefix :: FilePath, hsModules :: [FilePath]} deriving (Generic)
instance FromJSON SubDir

main :: IO ()
main = do
  putStrLn "Generating docs"
  projects :: [Project] <- decodeFileThrow configPath
  let
    docsPath :: FilePath
    docsPath = "../docs/src/haskell"
    convert :: FilePath -> FilePath -> IO ()
    convert hs md = do
      hsContent <- T.readFile hs
      let converted = (Hs `convertTo` Md) (def & indent ?~ "i" & dedent ?~ "d" & enable ?~ "E" & disable ?~ "D") hsContent
          prefix = splitOn "/" md & reverse & drop 1 & reverse & intercalate "/"
          dir = [i|#{docsPath}/#{prefix}|]
      createDirectoryIfMissing True dir
      T.writeFile [i|#{docsPath}/#{md}|] converted
    copyPaths :: SubDir -> [(FilePath, FilePath)]
    copyPaths SubDir{..} = (\x -> let y = [i|#{hsPrefix}/#{x}|] :: FilePath in ([i|#{y}.hs|], [i|#{mdPrefix}/#{x}.md|])) <$> hsModules
  
  -- Write text
  mapM_
    (uncurry convert)
    (concatMap (\Project{..} -> concatMap copyPaths subDirs) projects)
  
  -- Write Table of Contents
  T.writeFile [i|#{docsPath}/toc.md|] (pPrintProjects docsPrefix projects <> "\n\n")
