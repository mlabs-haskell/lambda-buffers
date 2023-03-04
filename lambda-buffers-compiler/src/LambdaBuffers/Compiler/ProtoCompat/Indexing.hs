module LambdaBuffers.Compiler.ProtoCompat.Indexing (indexClassDefs, indexTyDefs) where

import Control.Lens ((^.))
import Data.Map (Map)
import Data.Map qualified as Map
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC

indexClassDefs :: PC.CompilerInput -> Map PC.QClassName PC.ClassDef
indexClassDefs ci =
  Map.fromList
    [ ((mn, cn), cd)
    | (mn, m) <- Map.toList $ ci ^. #modules
    , (cn, cd) <- Map.toList $ m ^. #classDefs
    ]

indexTyDefs :: PC.CompilerInput -> Map PC.QTyName PC.TyDef
indexTyDefs ci =
  Map.fromList
    [ ((mn, tn), td)
    | (mn, m) <- Map.toList $ ci ^. #modules
    , (tn, td) <- Map.toList $ m ^. #typeDefs
    ]
