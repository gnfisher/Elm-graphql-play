-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UnknownSignature exposing (email, isValid, payload, signature, signer, state, wasSignedByGitHub)

import Api.Enum.GitSignatureState
import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| Email used to sign this object.
-}
email : SelectionSet String Api.Object.UnknownSignature
email =
    Object.selectionForField "String" "email" [] Decode.string


{-| True if the signature is valid and verified by GitHub.
-}
isValid : SelectionSet Bool Api.Object.UnknownSignature
isValid =
    Object.selectionForField "Bool" "isValid" [] Decode.bool


{-| Payload for GPG signing object. Raw ODB object without the signature header.
-}
payload : SelectionSet String Api.Object.UnknownSignature
payload =
    Object.selectionForField "String" "payload" [] Decode.string


{-| ASCII-armored signature header from object.
-}
signature : SelectionSet String Api.Object.UnknownSignature
signature =
    Object.selectionForField "String" "signature" [] Decode.string


{-| GitHub user corresponding to the email signing this commit.
-}
signer : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.UnknownSignature
signer object_ =
    Object.selectionForCompositeField "signer" [] object_ (identity >> Decode.nullable)


{-| The state of this signature. `VALID` if signature is valid and verified by GitHub, otherwise represents reason why signature is considered invalid.
-}
state : SelectionSet Api.Enum.GitSignatureState.GitSignatureState Api.Object.UnknownSignature
state =
    Object.selectionForField "Enum.GitSignatureState.GitSignatureState" "state" [] Api.Enum.GitSignatureState.decoder


{-| True if the signature was made with GitHub's signing key.
-}
wasSignedByGitHub : SelectionSet Bool Api.Object.UnknownSignature
wasSignedByGitHub =
    Object.selectionForField "Bool" "wasSignedByGitHub" [] Decode.bool
