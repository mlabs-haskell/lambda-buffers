module Main (main) where

import Control.Lens ((&), (.~))
import Data.ProtoLens.Default (def)
import Proto.Compiler (CompilerInput)
import Proto.Compiler_Fields (modules)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ (def :: CompilerInput) & modules .~ []
