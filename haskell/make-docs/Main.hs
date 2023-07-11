{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens
import Converter (Format (Hs, Md), convertTo, dedent, def, disable, enable, indent)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String.Interpolate (i)
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  putStrLn "Generating docs"
  let
    convert :: FilePath -> FilePath -> IO ()
    convert hs md = do
      hsContent <- T.readFile hs
      let converted = (Hs `convertTo` Md) (def & indent ?~ "i" & dedent ?~ "d" & enable ?~ "E" & disable ?~ "D") hsContent
          prefix = splitOn "/" md & reverse & drop 1 & reverse & intercalate "/"
          dir = [i|../docs/src/#{prefix}|]
      createDirectoryIfMissing True dir
      T.writeFile [i|../docs/src/#{md}|] converted
    copyPaths :: FilePath -> FilePath -> [FilePath] -> [(FilePath, FilePath)]
    copyPaths prefixHs prefixMd paths = (\x -> let y = [i|#{prefixHs}/#{x}|] :: FilePath in ([i|#{y}.hs|], [i|#{prefixMd}/#{x}.md|])) <$> paths
  mapM_
    (uncurry convert)
    ( copyPaths
        "developers-roadmap/src/Try"
        "developers-roadmap"
        [ "Aeson/HKD"
        , "Debug/Breakpoint"
        , "Effectful/Dynamic"
        , "Exceptions/Exceptions"
        , "Exceptions/Theory"
        , "Functions/Composition"
        , "Functions/Folds"
        , "Functions/General"
        , "FusedEffects/ReinterpretingEffects"
        , "FusedEffects/UndoIO"
        , "GADT/GADT"
        , "Generics/Generics"
        , "ImplicitParams/ImplicitParams"
        , "IO/RandomNumbers"
        , "Lens/MissingKey"
        , "Lens/Node"
        , "Misc/Determinant"
        , "Monads/FunctionalDependencies"
        , "Monads/MonadBaseControl"
        , "Monads/Monads"
        , "ParallelAndConcurrentHaskell/Exceptions"
        , "ParallelAndConcurrentHaskell/MVar"
        , "ParallelAndConcurrentHaskell/STM"
        , "TemplateHaskell/ConstructorTags/Declare"
        , "TemplateHaskell/ConstructorTags/Use"
        , "TemplateHaskell/Typed/Declare"
        , "TemplateHaskell/Typed/Use"
        , "Test/Theory"
        , "TypeClasses/Monoid"
        , "TypeClasses/Theory"
        , "TypeClasses/TypeClasses"
        , "TypeFamilies/Theory"
        , "TypeFamilies/TypeFamilies"
        ]
        <> copyPaths "optics-by-example/src" "optics-by-example" ["Extra", "README"]
    )