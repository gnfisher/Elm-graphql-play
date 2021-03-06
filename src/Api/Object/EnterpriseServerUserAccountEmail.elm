-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.EnterpriseServerUserAccountEmail exposing (createdAt, email, id, isPrimary, updatedAt, userAccount)

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


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.EnterpriseServerUserAccountEmail
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The email address.
-}
email : SelectionSet String Api.Object.EnterpriseServerUserAccountEmail
email =
    Object.selectionForField "String" "email" [] Decode.string


id : SelectionSet Api.ScalarCodecs.Id Api.Object.EnterpriseServerUserAccountEmail
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Indicates whether this is the primary email of the associated user account.
-}
isPrimary : SelectionSet Bool Api.Object.EnterpriseServerUserAccountEmail
isPrimary =
    Object.selectionForField "Bool" "isPrimary" [] Decode.bool


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.EnterpriseServerUserAccountEmail
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The user account to which the email belongs.
-}
userAccount : SelectionSet decodesTo Api.Object.EnterpriseServerUserAccount -> SelectionSet decodesTo Api.Object.EnterpriseServerUserAccountEmail
userAccount object_ =
    Object.selectionForCompositeField "userAccount" [] object_ identity
