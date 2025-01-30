module Utils exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, NetworkId(..))
import Cardano.MultiAsset exposing (AssetName, PolicyId)
import Cardano.Transaction exposing (Transaction, TransactionBody)
import Cardano.Utxo as Utxo exposing (OutputReference)
import Cardano.Value as Value
import Natural exposing (Natural)


alice : Address
alice =
    Address.base Testnet
        (Address.VKeyHash <| Bytes.fromHexUnchecked "8b59b4aef299653ed7ddccab220b6f366ea6b6cd8f98c258eef3060c")
        (Address.VKeyHash <| Bytes.fromHexUnchecked "ffe5c07578bec2d05ab50c6c89ff9f6879c7973635992aee5114b11a")


bob : Address
bob =
    Address.base Testnet
        (Address.VKeyHash <| Bytes.fromHexUnchecked "256f7f65163cc5fedfbab03a15de582f81b8448d1046417bf6d5b8e6")
        (Address.VKeyHash <| Bytes.fromHexUnchecked "7cd7e2ee8c20698f222c0b8e34c168cc19add679b9541249c49b8a07")


ada : Float -> Natural
ada amount =
    round (amount * 1000000)
        |> Natural.fromSafeInt


iUsd : Int -> { policyId : Bytes PolicyId, name : Bytes AssetName, amount : Natural }
iUsd usd =
    { policyId = Bytes.fromHexUnchecked "2fe3c3364b443194b10954771c95819b8d6ed464033c21f03f8facb5"
    , name = Bytes.fromHexUnchecked "69555344"
    , amount = Natural.fromSafeInt usd
    }


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
