module Manual exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Address, NetworkId(..))
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Transaction as Transaction exposing (Transaction, TransactionBody)
import Cardano.Utxo as Utxo exposing (OutputReference)
import Cardano.Value as Value
import Natural exposing (Natural)
import Utils exposing (ada, alice, bob, iUsd)


txToSubmit : Transaction
txToSubmit =
    -- insufficientMinAda01
    -- insufficientFee02
    -- unbalanced03
    validTx04


insufficientMinAda01 : Transaction
insufficientMinAda01 =
    Transaction.new
        -- spend the 50 iUSD input
        |> spendInput "0552665141bfa89cd18904d0cf27bae3ad4d9198259c9b3da72485491e112b2b#0"
        -- send 13 iUSD to Bob
        |> createOutput bob (ada 0) [ iUsd 13 ]


insufficientFee02 : Transaction
insufficientFee02 =
    Transaction.new
        -- spend the 50 iUSD input
        |> spendInput "0552665141bfa89cd18904d0cf27bae3ad4d9198259c9b3da72485491e112b2b#0"
        -- send 13 iUSD to Bob with some 2 ada
        |> createOutput bob (ada 2) [ iUsd 13 ]


unbalanced03 : Transaction
unbalanced03 =
    Transaction.new
        -- spend the 50 iUSD input
        |> spendInput "0552665141bfa89cd18904d0cf27bae3ad4d9198259c9b3da72485491e112b2b#0"
        -- send 13 iUSD to Bob with some 2 ada
        |> createOutput bob (ada 2) [ iUsd 13 ]
        -- add 0.20 ada to pay the fee
        |> setFee (ada 0.2)


validTx04 : Transaction
validTx04 =
    Transaction.new
        -- spend the 50 iUSD input
        |> spendInput "0552665141bfa89cd18904d0cf27bae3ad4d9198259c9b3da72485491e112b2b#0"
        -- send 13 iUSD to Bob with some 2 ada
        |> createOutput bob (ada 2) [ iUsd 13 ]
        -- add 0.20 ada to pay the fee
        |> setFee (ada 0.2)
        -- balancing the Tx by:
        --  * adding another input to pay the 0.2 ada fees
        --  * sending the unspent ada & iUSD back to Alice
        |> spendInput "7ca6473e59d24f173d0315fa01ba327bc5426c06ff40d7d854aa7aedd906a390#3"
        |> createOutput alice (ada <| 2) [ iUsd (100 - 13) ]
        |> createOutput alice (ada <| 5 - 2 - 0.2) []



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


setFee : Natural -> Transaction -> Transaction
setFee amount tx =
    updateBody (\body -> { body | fee = amount }) tx


createOutput : Address -> Natural -> List { policyId : Bytes PolicyId, name : Bytes AssetName, amount : Natural } -> Transaction -> Transaction
createOutput address adaAmount tokens tx =
    let
        addToken { policyId, name, amount } value =
            Value.add value (Value.onlyToken policyId name amount)

        adaPlusTokens =
            List.foldl addToken (Value.onlyLovelace adaAmount) tokens
    in
    updateBody (\body -> { body | outputs = Utxo.simpleOutput address adaPlusTokens :: body.outputs }) tx


spendInput : String -> Transaction -> Transaction
spendInput inputRefStr tx =
    inputFromString inputRefStr
        |> Maybe.map (\inputRef -> updateBody (addInput inputRef) tx)
        |> Maybe.withDefault tx


inputFromString : String -> Maybe OutputReference
inputFromString str =
    case String.split "#" str of
        [ txId, index ] ->
            Maybe.map2 OutputReference (Bytes.fromHex txId) (String.toInt index)

        _ ->
            Nothing


updateBody : (TransactionBody -> TransactionBody) -> Transaction -> Transaction
updateBody f tx =
    { tx | body = f tx.body }


addInput : OutputReference -> TransactionBody -> TransactionBody
addInput ref body =
    { body | inputs = ref :: body.inputs }
