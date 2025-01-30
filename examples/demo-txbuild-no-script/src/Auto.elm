module Auto exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (SpendSource(..), TxFinalizationError, TxIntent(..))
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value as Value exposing (Value)
import Natural exposing (Natural)
import Utils exposing (ada, alice, bob, iUsd)


txToSubmit : List ( OutputReference, Output ) -> Result TxFinalizationError Transaction
txToSubmit availableUtxos =
    let
        intents =
            -- unbalancedIntents01
            -- insufficientMinAda02
            validIntents03
    in
    Cardano.finalize (Utxo.refDictFromList availableUtxos) [] intents
        |> Result.map .tx


unbalancedIntents01 : List TxIntent
unbalancedIntents01 =
    [ SendTo bob (token <| iUsd 13)
    ]


insufficientMinAda02 : List TxIntent
insufficientMinAda02 =
    [ Spend <| FromWallet alice (token <| iUsd 13)
    , SendTo bob (token <| iUsd 13)
    ]


validIntents03 : List TxIntent
validIntents03 =
    let
        transferredValue =
            token (iUsd 13)
                |> Value.add (Value.onlyLovelace <| ada 2.0)
    in
    [ Spend <| FromWallet alice transferredValue
    , SendTo bob transferredValue
    ]



-- Helper functions
--
--
--
--
--
--
--
--
--
--
--
--


token : { policyId : Bytes PolicyId, name : Bytes AssetName, amount : Natural } -> Value
token { policyId, name, amount } =
    Value.onlyToken policyId name amount
