module LambdaBuffers.Codegen.Haskell.Backend.PlutusTx.LamVal (lamValContext) where

import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.LamVal (HaskellLamValContext (HaskellLamValContext), HaskellLamValMonad)
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import Prettyprinter (Doc, align, comma, encloseSep, group, lbracket, parens, rbracket, (<+>))

lamValContext :: HaskellLamValContext
lamValContext = HaskellLamValContext {ctx'printCaseIntE = printCaseIntE}

-- NOTE(bladyjoker): This is due to PlutusTx needing this to use the PlutusTx.Eq instances.
caseIntERef :: Haskell.QValName
caseIntERef = (Haskell.MkCabalPackageName "lbr-plutustx", Haskell.MkModuleName "LambdaBuffers.Runtime.PlutusTx.LamVal", Haskell.MkValueName "caseIntE")

--- | `printCaseIntE i [(1, x), (2,y)] (\other -> z)` translates into `LambdaBuffers.Runtime.PlutusTx.LamVal.caseIntE i [(1,x), (2,y)] (\other -> z)`
printCaseIntE :: HaskellLamValMonad m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  caseIntERefDoc <- Haskell.printHsQValName <$> LV.importValue caseIntERef
  caseValDoc <- Haskell.printValueE caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- Haskell.printValueE conditionVal
          bodyDoc <- Haskell.printValueE bodyVal
          return $ group $ parens (conditionDoc <+> "," <+> bodyDoc)
      )
  otherDoc <- Haskell.printLamE otherCase
  return $ group $ caseIntERefDoc <+> align (caseValDoc <+> align (encloseSep lbracket rbracket comma caseDocs) <+> otherDoc)
