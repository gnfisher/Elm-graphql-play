-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.TeamMemberEdge exposing (cursor, memberAccessResourcePath, memberAccessUrl, node, role)

import Api.Enum.TeamMemberRole
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
cursor : SelectionSet String Api.Object.TeamMemberEdge
cursor =
    Object.selectionForField "String" "cursor" [] Decode.string


{-| The HTTP path to the organization's member access page.
-}
memberAccessResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamMemberEdge
memberAccessResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "memberAccessResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL to the organization's member access page.
-}
memberAccessUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamMemberEdge
memberAccessUrl =
    Object.selectionForField "ScalarCodecs.Uri" "memberAccessUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


node : SelectionSet decodesTo Api.Object.User -> SelectionSet decodesTo Api.Object.TeamMemberEdge
node object_ =
    Object.selectionForCompositeField "node" [] object_ identity


{-| The role the member has on the team.
-}
role : SelectionSet Api.Enum.TeamMemberRole.TeamMemberRole Api.Object.TeamMemberEdge
role =
    Object.selectionForField "Enum.TeamMemberRole.TeamMemberRole" "role" [] Api.Enum.TeamMemberRole.decoder
