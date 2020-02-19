-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.DeployKey exposing (createdAt, id, key, readOnly, title, verified)

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
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.DeployKey
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.DeployKey
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The deploy key.
-}
key : SelectionSet String Api.Object.DeployKey
key =
    Object.selectionForField "String" "key" [] Decode.string


{-| Whether or not the deploy key is read only.
-}
readOnly : SelectionSet Bool Api.Object.DeployKey
readOnly =
    Object.selectionForField "Bool" "readOnly" [] Decode.bool


{-| The deploy key title.
-}
title : SelectionSet String Api.Object.DeployKey
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Whether or not the deploy key has been verified.
-}
verified : SelectionSet Bool Api.Object.DeployKey
verified =
    Object.selectionForField "Bool" "verified" [] Decode.bool