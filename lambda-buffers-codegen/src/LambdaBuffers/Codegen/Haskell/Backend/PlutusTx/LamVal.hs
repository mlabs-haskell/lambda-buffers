module LambdaBuffers.Codegen.Haskell.Backend.PlutusTx.LamVal (printValueE) where

import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as Haskell
import LambdaBuffers.Codegen.Haskell.Print.Syntax qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as LV
import LambdaBuffers.Codegen.LamVal.MonadPrint qualified as LV
import Prettyprinter (Doc, align, comma, encloseSep, group, lbracket, parens, rbracket, (<+>))

type MonadPrint m = LV.MonadPrint m Haskell.QValName

-- NOTE(bladyjoker): This is due to PlutusTx needing this to use the PlutusTx.Eq instances.
caseIntERef :: Haskell.QValName
caseIntERef = (Haskell.MkCabalPackageName "lbr-plutustx", Haskell.MkModuleName "LambdaBuffers.Runtime.PlutusTx.LamVal", Haskell.MkValueName "caseIntE")

--- | `printCaseIntE i [(1, x), (2,y)] (\other -> z)` translates into `LambdaBuffers.Runtime.PlutusTx.LamVal.caseIntE i [(1,x), (2,y)] (\other -> z)`
printCaseIntE :: MonadPrint m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  caseIntERefDoc <- Haskell.printHsQValName <$> LV.importValue caseIntERef
  caseValDoc <- printValueE caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- printValueE conditionVal
          bodyDoc <- printValueE bodyVal
          return $ group $ parens (conditionDoc <+> "," <+> bodyDoc)
      )
  otherDoc <- Haskell.printLamE otherCase
  return $ group $ caseIntERefDoc <+> align (caseValDoc <+> align (encloseSep lbracket rbracket comma caseDocs) <+> otherDoc)

printValueE :: MonadPrint m => LV.ValueE -> m (Doc ann)
printValueE = Haskell.printValueEWith printCaseIntE
