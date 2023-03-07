module Test.LambdaBuffers.Compiler.ProtoCompat.Utils (abs, sum, tv, td, lr, fr, mn, tn, (@), td'either, td'eitherO, td'maybe, td'maybeO, mod'prelude, ci, mod, td'list) where

import Control.Lens ((^.))
import Data.Default (Default (def))
import Data.Map qualified as Map
import Data.Map.Ordered qualified as OMap
import Data.Text (Text)
import LambdaBuffers.Compiler.ProtoCompat.InfoLess qualified as PC
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
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
ctor (cn', tys) = (PC.mkInfoLess $ cn cn', PC.Constructor (cn cn') (tpl tys))

tpl :: [PC.Ty] -> PC.Product
tpl tys = PC.TupleI (PC.Tuple tys def)

cn :: Text -> PC.ConstrName
cn n = PC.ConstrName n def

vn :: Text -> PC.VarName
vn n = PC.VarName n def

abs :: [Text] -> PC.TyBody -> PC.TyAbs
abs args body = PC.TyAbs (OMap.fromList . fmap arg $ args) body def
  where
    arg an = (PC.mkInfoLess (vn an), PC.TyArg (vn an) (PC.Kind $ PC.KindRef PC.KType) def)

tv :: Text -> PC.Ty
tv n = PC.TyVarI $ PC.TyVar (vn n)

mod :: [Text] -> [PC.TyDef] -> PC.Module
mod mn' tds = PC.Module (mn mn') (Map.fromList . fmap td' $ tds) mempty mempty mempty def
  where
    td' :: PC.TyDef -> (PC.InfoLess PC.TyName, PC.TyDef)
    td' td'' = (PC.mkInfoLess $ td'' ^. #tyName, td'')

ci :: [PC.Module] -> PC.CompilerInput
ci mods = PC.CompilerInput (Map.fromList . fmap mod' $ mods)
  where
    mod' :: PC.Module -> (PC.InfoLess PC.ModuleName, PC.Module)
    mod' mod'' = (PC.mkInfoLess $ mod'' ^. #moduleName, mod'')

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

td'map :: PC.TyDef
td'map = td "Map" (abs ["k", "v"] opq)

td'list :: PC.TyDef
td'list = td "List" (abs ["a"] $ sum [("Nil", []), ("Cons", [tv "a", lr "List" @ [tv "a"]])])
td'listO :: PC.TyDef
td'listO = td "List" (abs ["a"] opq)

-- | Some modules
mod'prelude :: PC.Module
mod'prelude = mod ["Prelude"] [td'eitherO, td'maybeO, td'int8, td'bytes, td'map, td'listO]
