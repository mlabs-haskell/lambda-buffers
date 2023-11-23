import { FromDataError } from "../../PlutusData.js";
import type { FromData, ToData } from "../../PlutusData.js";
import type { Eq, Json, Maybe } from "lbr-prelude";
import * as LbPrelude from "lbr-prelude";
import * as LbPreludeInstances from "../../Prelude/Instances.js";

import type { Credential, StakingCredential } from "./Credential.js";
import * as LbCredential from "./Credential.js";

// https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Address.hs

/**
 * An {@link Address} that may contain two credentials.
 *
 * @see {@link https://github.com/input-output-hk/plutus/blob/1.16.0.0/plutus-ledger-api/src/PlutusLedgerApi/V1/Address.hs#L30-L36}
 */
export type Address = {
  addressCredential: Credential;
  addressStakingCredential: Maybe<StakingCredential>;
};

/**
 * {@link Eq} instance for {@link Address}
 */
export const eqAddress: Eq<Address> = {
  eq: (l, r) => {
    return LbCredential.eqCredential.eq(
      l.addressCredential,
      r.addressCredential,
    ) &&
      LbPrelude.eqMaybe(LbCredential.eqStakingCredential).eq(
        l.addressStakingCredential,
        r.addressStakingCredential,
      );
  },
  neq: (l, r) => {
    return LbCredential.eqCredential.neq(
      l.addressCredential,
      r.addressCredential,
    ) ||
      LbPrelude.eqMaybe(LbCredential.eqStakingCredential).neq(
        l.addressStakingCredential,
        r.addressStakingCredential,
      );
  },
};

/**
 * {@link Json} instance for {@link Address}
 */
export const jsonAddress: Json<Address> = {
  toJson: (address) => {
    return {
      "credential": LbCredential.jsonCredential.toJson(
        address.addressCredential,
      ),
      "staking_credential": LbPrelude.jsonMaybe(
        LbCredential.jsonStakingCredential,
      ).toJson(address.addressStakingCredential),
    };
  },
  fromJson: (value) => {
    const credential = LbPrelude.caseFieldWithValue(
      "credential",
      LbCredential.jsonCredential.fromJson,
      value,
    );
    const maybeStakingCredential = LbPrelude.caseFieldWithValue(
      "staking_credential",
      LbPrelude.jsonMaybe(LbCredential.jsonStakingCredential).fromJson,
      value,
    );
    return {
      addressCredential: credential,
      addressStakingCredential: maybeStakingCredential,
    };
  },
};

/**
 * {@link ToData} instance for {@link Address}
 */
export const toDataAddress: ToData<Address> = {
  toData: (address) => {
    return {
      name: "Constr",
      fields: [0n, [
        LbCredential.toDataCredential.toData(address.addressCredential),
        LbPreludeInstances
          .toDataMaybe(LbCredential.toDataStakingCredential)
          .toData(address.addressStakingCredential),
      ]],
    };
  },
};

/**
 * {@link FromData} instance for {@link Address}
 */
export const fromDataAddress: FromData<Address> = {
  fromData: (plutusData) => {
    switch (plutusData.name) {
      case "Constr": {
        if (plutusData.fields[0] === 0n && plutusData.fields[1].length === 2) {
          return {
            addressCredential: LbCredential.fromDataCredential.fromData(
              plutusData.fields[1][0]!,
            ),
            addressStakingCredential: LbPreludeInstances.fromDataMaybe(
              LbCredential.fromDataStakingCredential,
            ).fromData(plutusData.fields[1][1]!),
          };
        } else {
          break;
        }
      }
      default:
        break;
    }
    throw new FromDataError("Unexpected data");
  },
};
