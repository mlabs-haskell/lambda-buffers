module LambdaBuffers.Frontend.Monad (FrontendT, FrontState (..), FrontRead (..), FrontendResult (..), throwE') where

import Control.Monad.Except (ExceptT, MonadTrans (lift))
import Control.Monad.State.Strict (MonadIO, StateT)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import LambdaBuffers.Frontend.Errors (FrontendError)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (Module, ModuleName, SourceInfo)
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope)

data FrontRead = FrontRead
  { current :: ModuleName SourceInfo
  , visited :: [ModuleName ()]
  , importPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

newtype FrontState = FrontState
  { importedModules :: Map (ModuleName ()) (Module SourceInfo, (TyScope, ClassScope))
  }
  deriving stock (Eq, Show)

newtype FrontendResult = FrontendResult
  { processedModules :: Map (ModuleName ()) (Module SourceInfo, (TyScope, ClassScope))
  }
  deriving stock (Eq, Show)

type FrontendT m a = MonadIO m => ReaderT FrontRead (StateT FrontState (ExceptT FrontendError m)) a

throwE' :: FrontendError -> FrontendT m a
throwE' = lift . lift . throwE
