module Gen.Generator where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T

import Common.Match
import Common.SourceTy
import Common.Types

data GenError
  = MatchFail Pat Pat
  | DeriveFail Pat
  | MultipleMatches Pat [Pat]
  | MalformedPatList Pat
  deriving (Show)

type GenResult = Either GenError Text

-- crude code generator
data Gen = Gen
  { genPat :: Pat
  , runGen :: Set Gen -> Pat -> GenResult
  }

instance Eq Gen where
  (Gen p _) == (Gen p' _) = p == p'

instance Ord Gen where
  (Gen p _) <= (Gen p' _) = p <= p'

genFromDefAndPrint :: Set Gen -> TyDef -> IO ()
genFromDefAndPrint rules def = do
  let pat = defToPat def
  case runGenerator rules pat of
    Left err -> print err
    Right t -> T.putStrLn t

runGenerator :: Set Gen -> Pat -> Either GenError Text
runGenerator gens pat = evalStateT (generator pat) gens

generator ::
  Pat ->
  StateT (Set Gen) (Either GenError) Text
generator pat =
  get >>= \rules ->
    case matchRules rules of
      [] -> lift . Left $ DeriveFail pat
      [g@(Gen p f)] -> do
        -- The trick is that we make sure every generator always has access to itself.
        -- This lets us handle infinite applications of TyCons,
        -- e.g. Maybe (Maybe (Maybe (Maybe IntP)))
        let gen = Gen p $ \scope px -> f (S.insert g scope) px
        modify $ S.insert gen
        newRules <- get
        lift $ runGen gen newRules pat
      xs -> lift . Left $ MultipleMatches pat (map genPat xs)
  where
    matchRules :: Set Gen -> [Gen]
    matchRules xs = S.toList $ S.filter (\gx -> matches (genPat gx) pat) xs

-- Constructs a primitive generator. Always succeeds when matching the target pattern,
-- ignores the set of generators in scope
primGen :: Pat -> Text -> Gen
primGen pat code = Gen pat $ \_ _ -> pure code
