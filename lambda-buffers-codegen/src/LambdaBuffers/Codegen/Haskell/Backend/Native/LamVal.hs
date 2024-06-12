module LambdaBuffers.Codegen.Haskell.Backend.Native.LamVal (printCaseIntE, lamValContext) where

import Data.Traversable (for)
import LambdaBuffers.Codegen.Haskell.Print.LamVal (HaskellLamValContext (HaskellLamValContext))
import LambdaBuffers.Codegen.Haskell.Print.LamVal qualified as Haskell
import LambdaBuffers.Codegen.LamVal qualified as LV
import Prettyprinter (Doc, align, group, line, vsep, (<+>))

lamValContext :: HaskellLamValContext
lamValContext = HaskellLamValContext {ctx'printCaseIntE = printCaseIntE}

printCaseIntE :: Haskell.HaskellLamValMonad m => LV.ValueE -> [(LV.ValueE, LV.ValueE)] -> (LV.ValueE -> LV.ValueE) -> m (Doc ann)
printCaseIntE caseIntVal cases otherCase = do
  caseValDoc <- Haskell.printValueE caseIntVal
  caseDocs <-
    for
      cases
      ( \(conditionVal, bodyVal) -> do
          conditionDoc <- Haskell.printValueE conditionVal
          bodyDoc <- Haskell.printValueE bodyVal
          return $ group $ conditionDoc <+> "->" <+> bodyDoc
      )
  otherDoc <- Haskell.printOtherCase otherCase
  return $ "ca" <> align ("se" <+> caseValDoc <+> "of" <> line <> vsep (caseDocs <> [otherDoc]))
