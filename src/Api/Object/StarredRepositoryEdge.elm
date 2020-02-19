-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.StarredRepositoryEdge exposing (cursor, node, starredAt)

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


{-| A cursor for use in pagination.
-}
cursor : SelectionSet String Api.Object.StarredRepositoryEdge
cursor =
    Object.selectionForField "String" "cursor" [] Decode.string


node : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.StarredRepositoryEdge
node object_ =
    Object.selectionForCompositeField "node" [] object_ identity


{-| Identifies when the item was starred.
-}
starredAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.StarredRepositoryEdge
starredAt =
    Object.selectionForField "ScalarCodecs.DateTime" "starredAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
