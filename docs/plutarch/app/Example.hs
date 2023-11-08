module Main (main) where

import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import LambdaBuffers.Example.Plutarch (
  Content (Content'Text),
  Message (Message),
  Ref (Ref),
  Status (Status'Active),
  User (User),
 )
import LambdaBuffers.Plutus.V1.Plutarch (Bytes, POSIXTime)
import LambdaBuffers.Prelude.Plutarch ()
import LambdaBuffers.Runtime.Plutarch (PList (PList))
import LambdaBuffers.Runtime.Plutarch qualified as Lb
import Plutarch (ClosedTerm, Config (Config), PlutusType, Term, TracingMode (DoTracingAndBinds), compile, pcon, perror, plam, pmatch, unTermCont, (#), (:-->))
import Plutarch.Api.V1 (PCurrencySymbol (PCurrencySymbol), PTokenName (PTokenName), ptuple)
import Plutarch.Api.V1.Time (PPOSIXTime (PPOSIXTime))
import Plutarch.ByteString (PByteString)
import Plutarch.Evaluate (evalScript)
import Plutarch.Extra.TermCont (pletC, pmatchC)
import Plutarch.Maybe qualified as Scott
import Plutarch.Prelude (PAsData, PBool (PFalse, PTrue), PBuiltinList, PEq ((#==)), PIsData, pconstant, pdata, pfind, pfromData, pif, pshow, ptrace, (#&&))

userRef :: Text -> Term s (Ref User)
userRef userName = userRef' (pfromData $ name userName)

userRef' :: Term s Bytes -> Term s (Ref User)
userRef' userName = pcon $ Ref (pdata $ ptuple # pcon' (PCurrencySymbol (pconstant "users")) # pcon' (PTokenName userName))

activeUser :: Text -> [Term s (Ref User)] -> Integer -> Term s User
activeUser n friends since = pcon $ User (name n) (pdata $ activeSince since) (pdata $ Lb.plistFrom friends)

activeSince :: Integer -> Term s Status
activeSince since = pcon (Status'Active (pcon' $ PPOSIXTime (pconstant since)))

name :: Text -> Term s (PAsData PByteString)
name = textToBytes

message :: Term s POSIXTime -> Term s (Ref User) -> Term s (Ref User) -> Term s Content -> Term s Message
message at from to content = pcon $ Message (pdata at) (pdata from) (pdata to) (pdata content)

-- | `isFriendly users msg` checks whether a "'sup" message is exchanged between friends.
isFriendly :: Term s (Lb.PList User :--> Message :--> PBool)
isFriendly = plam $ \users msg -> unTermCont $ do
  Message _at from to content <- pmatchC msg
  PList users' <- pmatchC users
  User fromName _ fromFriends <- pmatchC (pfromData $ findUserOrError # users' # pfromData from)
  User toName _ toFriends <- pmatchC (pfromData $ findUserOrError # users' # pfromData to)
  pletC $
    pif
      ( (isFriend # fromFriends # toName)
          #== (isFriend # toFriends # fromName)
          #&& (content #== pcon' (Content'Text (textToBytes "'sup")))
      )
      (pcon PTrue)
      (ptrace ("This wasn't a friendly message :(" <> pshow msg) perror)
  where
    findUser :: Term s (PBuiltinList (PAsData User) :--> Ref User :--> Scott.PMaybe (PAsData User))
    findUser = plam $
      \users uRef ->
        pfind
          # plam (\u -> pmatch (pfromData u) (\(User userName _userActiveSince _userFriends) -> userRef' (pfromData userName) #== uRef))
          # users

    findUserOrError :: Term s (PBuiltinList (PAsData User) :--> Ref User :--> PAsData User)
    findUserOrError = plam $
      \users uRef ->
        pmatch
          (findUser # users # uRef)
          $ \case
            Scott.PJust uName -> uName
            Scott.PNothing -> ptrace ("Error while finding a user with reference " <> pshow uRef <> " amongst given users " <> pshow users) perror

    isFriend :: Term s (PAsData (Lb.PList (Ref User)) :--> (PAsData Bytes :--> PBool))
    isFriend = plam $ \friends uname ->
      pmatch
        (pfind # plam (\friendRef -> pdata (userRef' (pfromData uname)) #== friendRef) # (toBuiltinList # pfromData friends))
        ( \case
            Scott.PJust _ -> pcon PTrue
            _ -> pcon PFalse
        )

-- | Utils
pcon' :: PIsData a => PlutusType a => a s -> Term s (PAsData a)
pcon' = pdata . pcon

textToBytes :: Text -> Term s (PAsData PByteString)
textToBytes = pdata . pconstant . Text.encodeUtf8

toBuiltinList :: Term s (Lb.PList a :--> PBuiltinList (PAsData a))
toBuiltinList = plam $ \xs -> pmatch xs (\(Lb.PList xs') -> xs')

evalBool :: ClosedTerm PBool -> IO ()
evalBool t =
  case Plutarch.compile (Config DoTracingAndBinds) (pif t (pcon PTrue) (ptrace "Term evaluated to False" perror)) of
    Left err -> print ("Error while compiling a Plutarch Term" :: String, err)
    Right script -> case evalScript script of
      (Left err, _, trace) -> print ("Not a friendly message it seems" :: String, err, trace)
      _ -> print ("Friends, peace and love!!!" :: String)

-- | Main program
drazen :: Term s User
drazen = activeUser "Drazen Popovic" [userRef "Gergely Szabó", userRef "Jared Pon"] 0

gergo :: Term s User
gergo = activeUser "Gergely Szabó" [userRef "Jared Pon", userRef "Drazen Popovic"] 1

jared :: Term s User
jared = activeUser "Jared Pon" [userRef "Gergely Szabó", userRef "Drazen Popovic"] 2

supJaredSaidGergo :: Term s Message
supJaredSaidGergo = message (pcon $ PPOSIXTime (pconstant 10)) (userRef "Gergely Szabó") (userRef "Jared Pon") (pcon $ Content'Text (textToBytes "'sup"))

main :: IO ()
main = evalBool $ isFriendly # Lb.plistFrom [drazen, gergo, jared] # supJaredSaidGergo
