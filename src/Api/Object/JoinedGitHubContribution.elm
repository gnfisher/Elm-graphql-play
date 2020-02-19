-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.JoinedGitHubContribution exposing (isRestricted, occurredAt, resourcePath, url, user)

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


{-| Whether this contribution is associated with a record you do not have access to. For
example, your own 'first issue' contribution may have been made on a repository you can no
longer access.
-}
isRestricted : SelectionSet Bool Api.Object.JoinedGitHubContribution
isRestricted =
    Object.selectionForField "Bool" "isRestricted" [] Decode.bool


{-| When this contribution was made.
-}
occurredAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.JoinedGitHubContribution
occurredAt =
    Object.selectionForField "ScalarCodecs.DateTime" "occurredAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP path for this contribution.
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.JoinedGitHubContribution
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this contribution.
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.JoinedGitHubContribution
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The user who made this contribution.
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet decodesTo Api.Object.JoinedGitHubContribution
user object_ =
    Object.selectionForCompositeField "user" [] object_ identity
