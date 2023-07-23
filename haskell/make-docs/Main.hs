{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Control.Lens (At (at), itraverse_, (%=), (&), (<&>), (<>=), (?~), (^.))
import Control.Monad.State
import Converter (Format (Hs, Md), convertTo, dedent, def, disable, enable, indent)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.String (IsString)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Data.Yaml (FromJSON, decodeFileThrow)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getEnv)
import System.FilePath (dropExtension, isExtensionOf, joinPath, splitPath)

newtype Path = Path {_path :: FilePath} deriving newtype (Eq, IsString)
instance Show Path where
  show :: Path -> String
  show = _path

data NodeSubDir = NodeSubDir {path :: Path, children :: Map.Map String NodeSubDir} deriving (Eq, Show)

addPath :: [String] -> Path -> NodeSubDir -> NodeSubDir
addPath [] _ _ = error "can't construct a node without a module name."
addPath (x : xs) finalPath currentNode = res
 where
  childNode = case xs of
    [] -> NodeSubDir{path = finalPath, children = Map.empty}
    xs_ ->
      case currentNode.children ^. at x of
        Nothing -> addPath xs_ finalPath NodeSubDir{path = "", children = Map.empty}
        Just n -> addPath xs_ finalPath n
  res = NodeSubDir{children = currentNode.children & at x ?~ childNode, path = currentNode.path}

data NodeProject = NodeProject {heading :: Text, nodes :: [NodeSubDir]}

mkNodesProject :: FilePath -> [Project] -> [NodeProject]
mkNodesProject prefix projects =
  projects
    <&> \Project{..} ->
      let
        nodes =
          subDirs
            <&> \SubDir{..} ->
              flip execState NodeSubDir{children = Map.empty, path = ""} $
                for hsModules $ \hsModule -> do
                  let xs = splitOn "/" hsModule
                  id %= \n -> addPath xs (Path [i|#{prefix}/#{mdPrefix}/#{hsModule}.md|]) n
       in
        NodeProject{heading, nodes}

pPrintNodeSubDir :: Int -> NodeSubDir -> State [Text] ()
pPrintNodeSubDir depth t =
  itraverse_
    ( \idx child -> do
        id <>= [[i|#{concat (replicate (depth * 2) " ")}- [#{idx}](#{path child})|]]
        case child of
          NodeSubDir{} -> pPrintNodeSubDir (depth + 1) child
    )
    t.children

pPrintNodeProject :: NodeProject -> Text
pPrintNodeProject t =
  Text.intercalate
    "\n"
    [ let h = t.heading in [i|\# #{h}\n|]
    , Text.intercalate "\n" $ (\x -> Text.intercalate "\n" (execState (pPrintNodeSubDir 0 x) [])) <$> t.nodes
    ]

pPrintProjects :: FilePath -> [Project] -> Text
pPrintProjects prefix ns = Text.intercalate "\n\n" $ pPrintNodeProject <$> mkNodesProject prefix ns

data Project = Project {heading :: Text, subDirs :: [SubDir]} deriving (Generic, Show)
data SubDir = SubDir {hsPrefix :: FilePath, mdPrefix :: FilePath, hsModules :: [FilePath]} deriving (Generic, Show)

findPaths :: Int -> Int -> FilePath -> IO [FilePath]
findPaths depth maxDepth dir =
  if depth > maxDepth
    then error "depth > max depth"
    else do
      paths <- ((\p -> [i|#{dir}/#{p}|]) <$>) <$> listDirectory dir
      files <- filterM (\x -> (&&) <$> doesFileExist x <*> pure (".hs" `isExtensionOf` x)) paths
      dirs <- filterM doesDirectoryExist paths
      subDirFiles <- if depth < maxDepth then concat <$> mapM (findPaths (depth + 1) maxDepth) dirs else pure []
      pure (files <> subDirFiles)

findModules :: Int -> FilePath -> IO [FilePath]
findModules maxDepth path = do
  paths <- findPaths 1 maxDepth path
  pure $ paths <&> dropExtension <&> splitPath <&> drop (length (splitPath path)) <&> joinPath & sort

data ConfigProject = ConfigProject {heading :: Text, subDirs :: [ConfigSubDir]} deriving (Generic)
instance FromJSON ConfigProject
data ConfigSubDir = ConfigSubDir {hsPrefix :: FilePath, mdPrefix :: FilePath, maxDepth :: Int} deriving (Generic)
instance FromJSON ConfigSubDir

main :: IO ()
main = do
  putStrLn "Generating docs"
  configPath <- getEnv "CONFIG_PATH"
  docsPath <- getEnv "DOCS_PATH"
  docsPrefix <- getEnv "DOCS_PREFIX"

  configsProject :: [ConfigProject] <- decodeFileThrow configPath
  projects <- forM configsProject $ \configProject -> do
    subDirs <- forM configProject.subDirs $ \ConfigSubDir{..} -> do
      hsModules <- findModules maxDepth hsPrefix
      pure SubDir{..}
    pure Project{heading = configProject.heading, subDirs}

  let
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
