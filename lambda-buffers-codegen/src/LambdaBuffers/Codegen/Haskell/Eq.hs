module LambdaBuffers.Codegen.Haskell.Eq where

import Control.Lens ((^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader (ask), asks)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Map qualified as Map
import LambdaBuffers.Codegen.Haskell.Syntax qualified as H
import LambdaBuffers.Compiler.ProtoCompat.Types qualified as PC
import Prettyprinter (Doc, Pretty (pretty), equals, (<+>))

data PrintCtx
  deriving stock (Eq, Ord, Show)

data PrintRead = MkPrintRead
  { ci :: PC.CompilerInput
  , currentModule :: PC.Module
  , opaqueImpls :: Map PC.TyName H.ModuleName
  }

type PrintWrite = [PrintCommand]
newtype PrintCommand = ImportInstance H.ModuleName
  deriving stock (Show)

type PrintErr = String

type MonadPrint m = (MonadRWS PrintRead PrintWrite Int m, MonadError PrintErr m)

eqImplTy :: MonadPrint m => PC.Ty -> m ()
eqImplTy (PC.TyAppI ta) = _
eqImplTy (PC.TyRefI tr) = eqImplTyRef tr
eqImplTy (PC.TyVarI _tv) = throwError "I don't provide an implementation for a Constraint type variable: instance Eq a"

eqImplTyRef :: MonadPrint m => PC.TyRef -> m ()
eqImplTyRef (PC.LocalI lr) = do
  m <- asks currentModule
  case Map.lookup (lr ^. #tyName) (m ^. #typeDefs) of -- FIX: infoless
    Nothing -> throwError $ "TODO(bladyjoker): Missing local type " <> show lr
    Just td -> eqImplBody (lr ^. #tyName) (td ^. #tyAbs . #tyBody)
eqImplTyRef (PC.ForeignI fr) = _

eqImplBody :: MonadPrint m => PC.TyName -> PC.TyBody -> m ()
eqImplBody tn (PC.OpaqueI _) = do
  is <- asks opaqueImpls
  case Map.lookup tn is of
    Nothing -> throwError $ "TODO(bladyjoker): Missing implementation for opaque type " <> show tn
    Just mn -> tell [ImportInstance mn]
eqImplBody tn (PC.SumI (PC.Sum ctors _)) = _

eqImplCtor :: MonadPrint m => PC.Constructor -> m (Doc ())
eqImplCtor (PC.Constructor ctorN p) = return $ pretty (ctorN ^. #name) <+> equals <> equals

-- eq :: Either a b -> Either a b -> Bool
-- eq = \x -> \y -> case x of
--   Left xleft -> case y of
--     Left yleft -> eq xleft yleft
--     Right yright -> False
--   Right xright -> case y of
--     Left yleft -> False
--     Right yright -> eq xright yright
