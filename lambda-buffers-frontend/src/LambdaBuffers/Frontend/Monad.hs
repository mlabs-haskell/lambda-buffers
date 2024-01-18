module LambdaBuffers.Frontend.Monad (FrontendT, FrontState (..), FrontRead (..), FrontendResult (..), throwE') where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (MonadIO, StateT)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map (Map)
import Data.Set (Set)
import LambdaBuffers.Frontend.Errors.Frontend (FrontendError)
import LambdaBuffers.Frontend.PPrint ()
import LambdaBuffers.Frontend.Syntax (Module, ModuleName, SourceInfo)
import LambdaBuffers.Frontend.Utils (ClassScope, TyScope)

data FrontRead = FrontRead
  { current :: ModuleName SourceInfo
  , visited :: [ModuleName ()]
  , importPaths :: Set FilePath
  }
  deriving stock (Eq, Show)

newtype FrontState = FrontState
  { fstate'modules :: Map (ModuleName ()) (Module SourceInfo, (TyScope, ClassScope))
  }
  deriving stock (Eq, Show)

data FrontendResult = FrontendResult
  { fres'modules :: Map (ModuleName ()) (Module SourceInfo, (TyScope, ClassScope))
  , fres'requested :: [ModuleName ()]
  }
  deriving stock (Eq, Show)

type FrontendT m a = MonadIO m => ReaderT FrontRead (StateT FrontState (ExceptT FrontendError m)) a

throwE' :: FrontendError -> FrontendT m a
throwE' = lift . lift . throwE
