module LambdaBuffers.Prelude.Cli.Generate (GenerateOpts (..), generate) where

import Data.Foldable (for_)
import LambdaBuffers.Prelude.Json.Golden (writeGoldens)
import LambdaBuffers.Prelude.Json.Golden qualified as Golden

newtype GenerateOpts = GenerateOpts {directory :: FilePath} deriving stock (Show, Eq, Ord)

generate :: GenerateOpts -> IO ()
generate opts = do
  let goldenDir = directory opts
  fps <-
    mconcat
      [ writeGoldens goldenDir "Foo.A" Golden.aGoldens
      , writeGoldens goldenDir "Foo.B" Golden.bGoldens
      , writeGoldens goldenDir "Foo.C" Golden.cGoldens
      , writeGoldens goldenDir "Foo.D" Golden.dGoldens
      , writeGoldens goldenDir "Days.Day" Golden.dayGoldens
      , writeGoldens goldenDir "Days.WorkDay" Golden.workDayGoldens
      , writeGoldens goldenDir "Days.FreeDay" Golden.freeDayGoldens
      , writeGoldens goldenDir "Prelude.Bool" Golden.boolGoldens
      , writeGoldens goldenDir "Prelude.Integer" Golden.integerGoldens
      , writeGoldens goldenDir "Prelude.Bytes" Golden.bytesGoldens
      , writeGoldens goldenDir "Prelude.Char" Golden.charGoldens
      , writeGoldens goldenDir "Prelude.Text" Golden.textGoldens
      , writeGoldens goldenDir "Prelude.Maybe" Golden.maybeGoldens
      , writeGoldens goldenDir "Prelude.Either" Golden.eitherGoldens
      , writeGoldens goldenDir "Prelude.List" Golden.listGoldens
      , writeGoldens goldenDir "Prelude.Set" Golden.setGoldens
      , writeGoldens goldenDir "Prelude.Map" Golden.mapGoldens
      ]
  putStrLn "[lbf-prelude-golden] Wrote goldens:"
  for_ fps putStrLn
