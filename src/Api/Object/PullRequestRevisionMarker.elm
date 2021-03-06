-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.PullRequestRevisionMarker exposing (createdAt, lastSeenCommit, pullRequest)

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
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.PullRequestRevisionMarker
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The last commit the viewer has seen.
-}
lastSeenCommit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet decodesTo Api.Object.PullRequestRevisionMarker
lastSeenCommit object_ =
    Object.selectionForCompositeField "lastSeenCommit" [] object_ identity


{-| The pull request to which the marker belongs.
-}
pullRequest : SelectionSet decodesTo Api.Object.PullRequest -> SelectionSet decodesTo Api.Object.PullRequestRevisionMarker
pullRequest object_ =
    Object.selectionForCompositeField "pullRequest" [] object_ identity
