module Test.LambdaBuffers.ProtoCompat.Utils (
  abs,
  sum,
  tv,
  td,
  lr,
  fr,
  inst,
  mn,
  tn,
  (@),
  td'either,
  td'eitherO,
  td'maybe,
  td'maybeO,
  mod'preludeO,
  cstr,
  opq,
  ci,
  drv,
  mod',
  mod,
  td'list,
  mod'prelude,
  fcr,
  inst',
  recrd,
  prod',
  qcln',
  mod'prelude'only'eq,
  mod'prelude'noclass,
  td'phantomA,
  td'phantomB,
  td'phantomC,
  td'phantomD,
  td'phantomE,
  td'phantomF,
) where

import Control.Lens ((^.))
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import LambdaBuffers.ProtoCompat qualified as PC
import Prelude (Monoid (mempty), fmap, ($), (.))

lr :: Text -> PC.Ty
lr n = PC.TyRefI $ PC.LocalI (PC.LocalRef (tn n) def)

fr :: [Text] -> Text -> PC.Ty
fr mn' tyn = PC.TyRefI $ PC.ForeignI (PC.ForeignRef (tn tyn) (mn mn') def)

tn :: Text -> PC.TyName
tn n = PC.TyName n def

(@) :: PC.Ty -> [PC.Ty] -> PC.Ty
(@) f as = PC.TyAppI $ PC.TyApp f as def

mn :: [Text] -> PC.ModuleName
mn parts = PC.ModuleName [PC.ModuleNamePart p def | p <- parts] def

td :: Text -> PC.TyAbs -> PC.TyDef
td tn' tabs = PC.TyDef (tn tn') tabs def

opq :: PC.TyBody
opq = PC.OpaqueI def

sum :: [(Text, [PC.Ty])] -> PC.TyBody
sum ctors' = PC.SumI $ PC.Sum (OMap.fromList . fmap ctor $ ctors') def

ctor :: (Text, [PC.Ty]) -> (PC.InfoLess PC.ConstrName, PC.Constructor)
ctor (cn', tys) = (PC.mkInfoLess $ cn cn', PC.Constructor (cn cn') (prod tys))

prod :: [PC.Ty] -> PC.Product
prod tys = PC.Product tys def

prod' :: [PC.Ty] -> PC.TyBody
prod' = PC.ProductI . prod

recrd :: [(Text, PC.Ty)] -> PC.TyBody
recrd fields = PC.RecordI $ PC.Record (OMap.fromList [(PC.mkInfoLess . fn $ n, PC.Field (fn n) t) | (n, t) <- fields]) def

cn :: Text -> PC.ConstrName
cn n = PC.ConstrName n def

fn :: Text -> PC.FieldName
fn n = PC.FieldName n def

vn :: Text -> PC.VarName
vn n = PC.VarName n def

cln :: Text -> PC.ClassName
cln n = PC.ClassName n def

qcln' :: PC.ModuleName -> Text -> PC.QClassName
qcln' mn' n = (PC.mkInfoLess mn', PC.mkInfoLess $ PC.ClassName n def)

abs :: [Text] -> PC.TyBody -> PC.TyAbs
abs args body = PC.TyAbs (OMap.fromList . fmap arg $ args) body def
  where
    arg an = (PC.mkInfoLess (vn an), PC.TyArg (vn an) (PC.Kind $ PC.KindRef PC.KType) def)

tv :: Text -> PC.Ty
tv n = PC.TyVarI $ PC.TyVar (vn n)

mod :: [Text] -> [PC.TyDef] -> PC.Module
mod mn' tds = PC.Module (mn mn') (Map.fromList . fmap td' $ tds) mempty mempty mempty mempty def
  where
    td' :: PC.TyDef -> (PC.InfoLess PC.TyName, PC.TyDef)
    td' td'' = (PC.mkInfoLess $ td'' ^. #tyName, td'')

mod' :: [Text] -> [PC.TyDef] -> [PC.ClassDef] -> [PC.InstanceClause] -> [PC.Derive] -> [[Text]] -> PC.Module
mod' mn' tds cds insts drvs impts =
  PC.Module
    (mn mn')
    (Map.fromList . fmap infoLessTd $ tds)
    (Map.fromList . fmap infoLessCd $ cds)
    insts
    drvs
    (Map.fromList . fmap (infoLessMn . mn) $ impts)
    def
  where
    infoLessTd :: PC.TyDef -> (PC.InfoLess PC.TyName, PC.TyDef)
    infoLessTd td' = (PC.mkInfoLess $ td' ^. #tyName, td')

    infoLessCd :: PC.ClassDef -> (PC.InfoLess PC.ClassName, PC.ClassDef)
    infoLessCd cd' = (PC.mkInfoLess $ cd' ^. #className, cd')

    infoLessMn :: PC.ModuleName -> (PC.InfoLess PC.ModuleName, PC.ModuleName)
    infoLessMn mn_ = (PC.mkInfoLess mn_, mn_)

fcr :: [Text] -> Text -> PC.TyClassRef
fcr mn' cln' = PC.ForeignCI $ PC.ForeignClassRef (cln cln') (mn mn') def

lcr :: Text -> PC.TyClassRef
lcr cln' = PC.LocalCI $ PC.LocalClassRef (cln cln') def

classCstr :: (PC.TyClassRef, Text) -> PC.ClassConstraint
classCstr (clr, an') = PC.ClassConstraint clr (PC.TyVar . vn $ an')

classDef :: (Text, Text) -> [PC.ClassConstraint] -> PC.ClassDef
classDef (cln', an) sups = PC.ClassDef (cln cln') (mkArg an) sups "testing class def" def
  where
    mkArg an' = PC.TyArg (vn an') (PC.Kind $ PC.KindRef PC.KType) def

inst :: PC.TyClassRef -> PC.Ty -> [PC.Constraint] -> PC.InstanceClause
inst cr' ty' body = PC.InstanceClause (cstr cr' ty') body def

inst' :: PC.Constraint -> [PC.Constraint] -> PC.InstanceClause
inst' h body = PC.InstanceClause h body def

cstr :: PC.TyClassRef -> PC.Ty -> PC.Constraint
cstr cr' ty' = PC.Constraint cr' ty' def

drv :: PC.Constraint -> PC.Derive
drv = PC.Derive

ci :: [PC.Module] -> PC.CompilerInput
ci mods = PC.CompilerInput (Map.fromList . fmap mod_ $ mods)
  where
    mod_ :: PC.Module -> (PC.InfoLess PC.ModuleName, PC.Module)
    mod_ mod'' = (PC.mkInfoLess $ mod'' ^. #moduleName, mod'')

-- | Some type definitions.
td'either :: PC.TyDef
td'either = td "Either" (abs ["a", "b"] $ sum [("Left", [tv "a"]), ("Right", [tv "b"])])

td'eitherO :: PC.TyDef
td'eitherO = td "Either" (abs ["a", "b"] opq)

td'maybe :: PC.TyDef
td'maybe = td "Maybe" (abs ["a"] $ sum [("Nothing", []), ("Just", [tv "a"])])
td'maybeO :: PC.TyDef
td'maybeO = td "Maybe" (abs ["a"] opq)

td'int8 :: PC.TyDef
td'int8 = td "Int8" (abs [] opq)

td'bytes :: PC.TyDef
td'bytes = td "Bytes" (abs [] opq)

td'mapO :: PC.TyDef
td'mapO = td "Map" (abs ["k", "v"] opq)

td'list :: PC.TyDef
td'list = td "List" (abs ["a"] $ sum [("Nil", []), ("Cons", [tv "a", lr "List" @ [tv "a"]])])
td'listO :: PC.TyDef
td'listO = td "List" (abs ["a"] opq)

td'phantomA :: PC.TyDef
td'phantomA = td "PhantomA" (abs ["a"] $ sum [("A", [lr "Int", lr "Int"]), ("B", [lr "Int", lr "Int"])])

td'phantomB :: PC.TyDef
td'phantomB = td "PhantomB" (abs ["a", "b"] $ sum [("A", [lr "Int", lr "Int"]), ("B", [lr "Int", lr "Int"])])

td'phantomC :: PC.TyDef
td'phantomC = td "PhantomC" (abs ["a"] $ prod' [lr "Int", lr "Int"])

td'phantomD :: PC.TyDef
td'phantomD = td "PhantomD" (abs ["a", "b"] $ prod' [lr "Int", lr "Int"])

td'phantomE :: PC.TyDef
td'phantomE = td "PhantomE" (abs ["a"] $ recrd [("foo", lr "Int"), ("bar", lr "Int")])

td'phantomF :: PC.TyDef
td'phantomF = td "PhantomF" (abs ["a", "b"] $ recrd [("foo", lr "Int"), ("bar", lr "Int")])

-- | Some class definitions.
cd'eq :: PC.ClassDef
cd'eq = classDef ("Eq", "a") []

cd'ord :: PC.ClassDef
cd'ord = classDef ("Ord", "b") [classCstr (lcr "Eq", "b")]

-- | Some instances
inst'eq'int8 :: PC.InstanceClause
inst'eq'int8 = inst (lcr "Eq") (lr "Int8") []

inst'ord'int8 :: PC.InstanceClause
inst'ord'int8 = inst (lcr "Ord") (lr "Int8") []

inst'eq'bytes :: PC.InstanceClause
inst'eq'bytes = inst (lcr "Eq") (lr "Bytes") []

inst'ord'bytes :: PC.InstanceClause
inst'ord'bytes = inst (lcr "Ord") (lr "Bytes") []

inst'eq'maybeO :: PC.InstanceClause
inst'eq'maybeO = inst (lcr "Eq") (lr "Maybe" @ [tv "a"]) [cstr (lcr "Eq") (tv "a")]

inst'ord'maybeO :: PC.InstanceClause
inst'ord'maybeO = inst (lcr "Ord") (lr "Maybe" @ [tv "a"]) [cstr (lcr "Ord") (tv "a")]

inst'eq'eitherO :: PC.InstanceClause
inst'eq'eitherO = inst (lcr "Eq") (lr "Either" @ [tv "a", tv "b"]) [cstr (lcr "Eq") (tv "a"), cstr (lcr "Eq") (tv "b")]

inst'ord'eitherO :: PC.InstanceClause
inst'ord'eitherO = inst (lcr "Ord") (lr "Either" @ [tv "a", tv "b"]) [cstr (lcr "Ord") (tv "a"), cstr (lcr "Ord") (tv "b")]

inst'eq'listO :: PC.InstanceClause
inst'eq'listO = inst (lcr "Eq") (lr "List" @ [tv "a"]) [cstr (lcr "Eq") (tv "a")]

inst'ord'listO :: PC.InstanceClause
inst'ord'listO = inst (lcr "Ord") (lr "List" @ [tv "a"]) [cstr (lcr "Ord") (tv "a")]

inst'eq'mapO :: PC.InstanceClause
inst'eq'mapO = inst (lcr "Eq") (lr "Map" @ [tv "a", tv "b"]) [cstr (lcr "Eq") (tv "a"), cstr (lcr "Eq") (tv "b")]

inst'ord'mapO :: PC.InstanceClause
inst'ord'mapO = inst (lcr "Ord") (lr "Map" @ [tv "a", tv "b"]) [cstr (lcr "Ord") (tv "a"), cstr (lcr "Ord") (tv "b")]

-- | Some derive statements.
derive'eq'either :: PC.Derive
derive'eq'either = drv $ cstr (lcr "Eq") (lr "Either" @ [tv "a", tv "b"])

derive'ord'either :: PC.Derive
derive'ord'either = drv $ cstr (lcr "Ord") (lr "Either" @ [tv "a", tv "b"])

derive'eq'maybe :: PC.Derive
derive'eq'maybe = drv $ cstr (lcr "Eq") (lr "Maybe" @ [tv "a"])

derive'ord'maybe :: PC.Derive
derive'ord'maybe = drv $ cstr (lcr "Ord") (lr "Maybe" @ [tv "a"])

derive'eq'list :: PC.Derive
derive'eq'list = drv $ cstr (lcr "Eq") (lr "List" @ [tv "a"])

derive'ord'list :: PC.Derive
derive'ord'list = drv $ cstr (lcr "Ord") (lr "List" @ [tv "a"])

-- | Some modules.
mod'preludeO :: PC.Module
mod'preludeO =
  mod'
    ["Prelude"]
    [td'eitherO, td'maybeO, td'int8, td'bytes, td'mapO, td'listO]
    [cd'eq, cd'ord]
    [ inst'eq'int8
    , inst'ord'int8
    , inst'eq'bytes
    , inst'ord'bytes
    , inst'eq'maybeO
    , inst'ord'maybeO
    , inst'eq'eitherO
    , inst'ord'eitherO
    , inst'eq'mapO
    , inst'ord'mapO
    , inst'eq'listO
    , inst'ord'listO
    ]
    []
    []

mod'prelude :: PC.Module
mod'prelude =
  mod'
    ["Prelude"]
    [td'either, td'maybe, td'int8, td'bytes, td'mapO, td'list]
    [cd'eq, cd'ord]
    [ inst'eq'int8
    , inst'ord'int8
    , inst'eq'bytes
    , inst'ord'bytes
    , inst'eq'mapO
    , inst'ord'mapO
    ]
    [ derive'eq'maybe
    , derive'ord'maybe
    , derive'eq'either
    , derive'ord'either
    , derive'eq'list
    , derive'ord'list
    ]
    []

mod'prelude'only'eq :: PC.Module
mod'prelude'only'eq =
  mod'
    ["Prelude"]
    [td'eitherO, td'maybeO, td'int8, td'bytes, td'mapO, td'listO]
    [cd'eq]
    [ inst'eq'int8
    , inst'eq'bytes
    , inst'eq'maybeO
    , inst'eq'eitherO
    , inst'eq'mapO
    , inst'eq'listO
    ]
    []
    []

mod'prelude'noclass :: PC.Module
mod'prelude'noclass =
  mod'
    ["Prelude"]
    [td'eitherO, td'maybeO, td'int8, td'bytes, td'mapO, td'listO]
    []
    []
    []
    []
