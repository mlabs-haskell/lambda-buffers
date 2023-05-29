module LambdaBuffers.ProtoCompat.Indexing (
  ClassRels,
  TyDefs,
  ClassDefs,
  indexClassDefs,
  indexTyDefs,
  qualifyClassRef,
  qualifyTyRef,
  qualifyClassName,
  indexClassRelations,
  tyRefFromQualified,
  classRefFromQualified,
) where

import Control.Lens (view, (^.))
import Data.Default (Default (def))
import Data.Generics.Product (HasField')
import Data.Map (Map)
import Data.Map qualified as Map
import LambdaBuffers.ProtoCompat.InfoLess (InfoLess)
import LambdaBuffers.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.ProtoCompat.Types qualified as PC

type ClassDefs = Map PC.QClassName PC.ClassDef

type HasModules a = HasField' "modules" a (Map (InfoLess PC.ModuleName) PC.Module)

indexClassDefs :: HasModules a => a -> Map PC.QClassName PC.ClassDef
indexClassDefs ci =
  Map.fromList
    [ ((mn, cn), cd)
    | (mn, m) <- Map.toList $ ci ^. #modules
    , (cn, cd) <- Map.toList $ m ^. #classDefs
    ]

type TyDefs = Map PC.QTyName PC.TyDef

indexTyDefs :: HasModules a => a -> Map PC.QTyName PC.TyDef
indexTyDefs ci =
  Map.fromList
    [ ((mn, tn), td)
    | (mn, m) <- Map.toList $ ci ^. #modules
    , (tn, td) <- Map.toList $ m ^. #typeDefs
    ]

type ClassRels = Map PC.QClassName [PC.QClassName]

indexClassRelations :: HasModules a => a -> ClassRels
indexClassRelations ci =
  foldr
    ( \m classRels ->
        foldr
          ( \cd classRels' ->
              let qualifiedSupers = qualifyClassRef (m ^. #moduleName) . view #classRef <$> cd ^. #supers
                  qualifiedClassName = qualifyClassName (m ^. #moduleName) (cd ^. #className)
               in Map.insert qualifiedClassName qualifiedSupers classRels'
          )
          classRels
          (m ^. #classDefs)
    )
    mempty
    (ci ^. #modules)

qualifyClassRef :: PC.ModuleName -> PC.TyClassRef -> PC.QClassName
qualifyClassRef _ (PC.ForeignCI fcr) = (PC.mkInfoLess $ fcr ^. #moduleName, PC.mkInfoLess $ fcr ^. #className)
qualifyClassRef mn (PC.LocalCI lcr) = (PC.mkInfoLess mn, PC.mkInfoLess $ lcr ^. #className)

qualifyClassName :: PC.ModuleName -> PC.ClassName -> PC.QClassName
qualifyClassName mn cn = (PC.mkInfoLess mn, PC.mkInfoLess cn)

qualifyTyRef :: PC.ModuleName -> PC.TyRef -> PC.QTyName
qualifyTyRef _ (PC.ForeignI fr) = (PC.mkInfoLess $ fr ^. #moduleName, PC.mkInfoLess $ fr ^. #tyName)
qualifyTyRef mn (PC.LocalI lr) = (PC.mkInfoLess mn, PC.mkInfoLess $ lr ^. #tyName)

tyRefFromQualified :: PC.ModuleName -> PC.QTyName -> PC.TyRef
tyRefFromQualified locMn (mn, tyn)
  | PC.mkInfoLess locMn == mn =
      let tyn' = PC.withInfoLess tyn id
       in PC.LocalI $ PC.LocalRef tyn' def
tyRefFromQualified _locMn (mn, tyn) =
  let tyn' = PC.withInfoLess tyn id
      mn' = PC.withInfoLess mn id
   in PC.ForeignI $ PC.ForeignRef tyn' mn' def

classRefFromQualified :: PC.ModuleName -> PC.QClassName -> PC.TyClassRef
classRefFromQualified locMn (mn, cn)
  | PC.mkInfoLess locMn == mn =
      let cn' = PC.withInfoLess cn id
       in PC.LocalCI $ PC.LocalClassRef cn' def
classRefFromQualified _locMn (mn, cn) =
  let cn' = PC.withInfoLess cn id
      mn' = PC.withInfoLess mn id
   in PC.ForeignCI $ PC.ForeignClassRef cn' mn' def
