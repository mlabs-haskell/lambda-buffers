import { IsPlutusDataError } from "../PlutusData.js";
import type { IsPlutusData } from "../PlutusData.js";
import type { Integer } from "lbr-prelude";
import type { PubKeyHash } from "./Crypto.js";
import type { ScriptHash } from "./Scripts.js";
import * as LbCrypto from "./Crypto.js";
import * as LbScript from "./Scripts.js";
import type { Eq, Json } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../../Prelude/Instances.js";
import { JsonError } from "lbr-prelude";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs

/**
 * {@link StakingCredential} used to assign rewards
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs#L25-L40 }
 */
export type StakingCredential =
  | { name: "StakingHash"; fields: Credential }
  | { name: "StakingPtr"; fields: [Integer, Integer, Integer] };

/**
 * {@link Eq} instance for {@link StakingCredential}
 */
export const eqStakingCredential: Eq<StakingCredential> = {
  eq: (l, r) => {
    if (l.name === "StakingHash" && r.name === "StakingHash") {
      return eqCredential.eq(l.fields, r.fields);
    } else if (l.name === "StakingPtr" && r.name === "StakingPtr") {
      return LbPrelude.eqList(LbPrelude.eqInteger).eq(l.fields, r.fields);
    } else {
      return false;
    }
  },
  neq: (l, r) => {
    if (l.name === "StakingHash" && r.name === "StakingHash") {
      return eqCredential.neq(l.fields, r.fields);
    } else if (l.name === "StakingPtr" && r.name === "StakingPtr") {
      return LbPrelude.eqList(LbPrelude.eqInteger).neq(l.fields, r.fields);
    } else {
      return true;
    }
  },
};

/**
 * {@link Json} instance for {@link StakingCredential}
 */
export const jsonStakingCredential: Json<StakingCredential> = {
  toJson: (stakingCredential) => {
    switch (stakingCredential.name) {
      case "StakingHash":
        return LbPrelude.jsonConstructor(stakingCredential.name, [
          jsonCredential.toJson(stakingCredential.fields),
        ]);
      case "StakingPtr":
        return LbPrelude.jsonConstructor(stakingCredential.name, [{
          "slot_number": LbPrelude.jsonInteger.toJson(
            stakingCredential.fields[0],
          ),
          "transaction_index": LbPrelude.jsonInteger.toJson(
            stakingCredential.fields[1],
          ),
          "certificate_index": LbPrelude.jsonInteger.toJson(
            stakingCredential.fields[2],
          ),
        }]);
    }
  },
  fromJson: (value) => {
    return LbPrelude.caseJsonConstructor<StakingCredential>(
      "Plutus.V1.StakingCredential",
      {
        "StakingHash": (ctorFields) => {
          if (ctorFields.length !== 1) {
            throw new JsonError(`Expected one field`);
          }
          return {
            name: "StakingHash",
            fields: jsonCredential.fromJson(ctorFields[0]!),
          };
        },
        "StakingPtr": (ctorFields) => {
          if (ctorFields.length !== 1) {
            throw new JsonError(`Expected one field`);
          }

          const fields = ctorFields[0]!;

          const slotNumber = LbPrelude.caseFieldWithValue(
            "slot_number",
            LbPrelude.jsonInteger.fromJson,
            fields,
          );
          const transactionIndex = LbPrelude.caseFieldWithValue(
            "transaction_index",
            LbPrelude.jsonInteger.fromJson,
            fields,
          );
          const certificateIndex = LbPrelude.caseFieldWithValue(
            "certificate_index",
            LbPrelude.jsonInteger.fromJson,
            fields,
          );

          return {
            name: "StakingPtr",
            fields: [slotNumber, transactionIndex, certificateIndex],
          };
        },
      },
      value,
    );
  },
};

/**
 * {@link IsPlutusData} instance for {@link StakingCredential}
 */
export const isPlutusDataStakingCredential: IsPlutusData<StakingCredential> = {
  toData: (stakingCredential) => {
    switch (stakingCredential.name) {
      case "StakingHash":
        return {
          name: "Constr",
          fields: [0n, [
            isPlutusDataCredential.toData(stakingCredential.fields),
          ]],
        };
      case "StakingPtr":
        return {
          name: "Constr",
          fields: [1n, [
            LbPreludeInstances.isPlutusDataInteger.toData(
              stakingCredential.fields[0],
            ),
            LbPreludeInstances.isPlutusDataInteger.toData(
              stakingCredential.fields[1],
            ),
            LbPreludeInstances.isPlutusDataInteger.toData(
              stakingCredential.fields[2],
            ),
          ]],
        };
    }
  },

  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr": {
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 1) {
          return {
            name: "StakingHash",
            fields: isPlutusDataCredential.fromData(plutusData.fields[1][0]!),
          };
        } else if (
          plutusData.fields[0] === 1n && plutusData.fields[1].length === 3
        ) {
          return {
            name: "StakingPtr",
            fields: [
              LbPreludeInstances.isPlutusDataInteger.fromData(
                plutusData.fields[1][0]!,
              ),
              LbPreludeInstances.isPlutusDataInteger.fromData(
                plutusData.fields[1][1]!,
              ),
              LbPreludeInstances.isPlutusDataInteger.fromData(
                plutusData.fields[1][2]!,
              ),
            ],
          };
        } else {
          break;
        }
      }
      default:
        break;
    }
    throw new IsPlutusDataError("Unexpected data");
  },
};

/**
 * {@link Credential} required to unlock a transaction output.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Credential.hs#L55-L65}
 */
export type Credential =
  | { name: "PubKeyCredential"; fields: PubKeyHash }
  | { name: "ScriptCredential"; fields: ScriptHash };

/**
 * {@link Eq} instance for {@link Credential}
 */
export const eqCredential: Eq<Credential> = {
  eq: (l, r) => {
    if (l.name === "PubKeyCredential" && r.name === "PubKeyCredential") {
      return LbCrypto.eqPubKeyHash.eq(l.fields, r.fields);
    } else if (l.name === "ScriptCredential" && r.name === "ScriptCredential") {
      return LbScript.eqScriptHash.eq(l.fields, r.fields);
    } else {
      return false;
    }
  },
  neq: (l, r) => {
    if (l.name === "PubKeyCredential" && r.name === "PubKeyCredential") {
      return LbCrypto.eqPubKeyHash.neq(l.fields, r.fields);
    } else if (l.name === "ScriptCredential" && r.name === "ScriptCredential") {
      return LbScript.eqScriptHash.neq(l.fields, r.fields);
    } else {
      return true;
    }
  },
};

/**
 * {@link Json} instance for {@link Credential}
 */
export const jsonCredential: Json<Credential> = {
  toJson: (credential) => {
    switch (credential.name) {
      case "PubKeyCredential":
        return LbPrelude.jsonConstructor(credential.name, [
          LbCrypto.jsonPubKeyHash.toJson(credential.fields),
        ]);
      case "ScriptCredential":
        return LbPrelude.jsonConstructor(credential.name, [
          LbScript.jsonScriptHash.toJson(credential.fields),
        ]);
    }
  },
  fromJson: (value) => {
    return LbPrelude.caseJsonConstructor<Credential>("Plutus.V1.Credential", {
      "PubKeyCredential": (ctorFields) => {
        if (ctorFields.length !== 1) {
          throw new JsonError(`Expected one field`);
        }
        return {
          name: "PubKeyCredential",
          fields: LbCrypto.jsonPubKeyHash.fromJson(ctorFields[0]!),
        };
      },
      "ScriptCredential": (ctorFields) => {
        if (ctorFields.length !== 1) {
          throw new JsonError(`Expected one field`);
        }
        return {
          name: "ScriptCredential",
          fields: LbScript.jsonScriptHash.fromJson(ctorFields[0]!),
        };
      },
    }, value);
  },
};

/**
 * {@link IsPlutusData} instance for {@link Credential}
 */
export const isPlutusDataCredential: IsPlutusData<Credential> = {
  toData: (credential) => {
    switch (credential.name) {
      case "PubKeyCredential":
        return {
          name: "Constr",
          fields: [0n, [
            LbCrypto.isPlutusDataPubKeyHash.toData(credential.fields),
          ]],
        };
      case "ScriptCredential":
        return {
          name: "Constr",
          fields: [1n, [
            LbScript.isPlutusDataScriptHash.toData(credential.fields),
          ]],
        };
    }
  },

  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr": {
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 1) {
          return {
            name: "PubKeyCredential",
            fields: LbCrypto.isPlutusDataPubKeyHash.fromData(
              plutusData.fields[1][0]!,
            ),
          };
        } else if (
          plutusData.fields[0] === 1n && plutusData.fields[1].length === 1
        ) {
          return {
            name: "ScriptCredential",
            fields: LbScript.isPlutusDataScriptHash.fromData(
              plutusData.fields[1][0]!,
            ),
          };
        } else {
          break;
        }
      }
      default:
        break;
    }
    throw new IsPlutusDataError("Unexpected data");
  },
};
