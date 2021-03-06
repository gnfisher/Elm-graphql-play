-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Ref exposing (AssociatedPullRequestsOptionalArguments, associatedPullRequests, id, name, prefix, repository, target)

import Api.Enum.PullRequestState
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


type alias AssociatedPullRequestsOptionalArguments =
    { states : OptionalArgument (List Api.Enum.PullRequestState.PullRequestState)
    , labels : OptionalArgument (List String)
    , headRefName : OptionalArgument String
    , baseRefName : OptionalArgument String
    , orderBy : OptionalArgument Api.InputObject.IssueOrder
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of pull requests with this ref as the head ref.

  - states - A list of states to filter the pull requests by.
  - labels - A list of label names to filter the pull requests by.
  - headRefName - The head ref name to filter the pull requests by.
  - baseRefName - The base ref name to filter the pull requests by.
  - orderBy - Ordering options for pull requests returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
associatedPullRequests : (AssociatedPullRequestsOptionalArguments -> AssociatedPullRequestsOptionalArguments) -> SelectionSet decodesTo Api.Object.PullRequestConnection -> SelectionSet decodesTo Api.Object.Ref
associatedPullRequests fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { states = Absent, labels = Absent, headRefName = Absent, baseRefName = Absent, orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "states" filledInOptionals.states (Encode.enum Api.Enum.PullRequestState.toString |> Encode.list), Argument.optional "labels" filledInOptionals.labels (Encode.string |> Encode.list), Argument.optional "headRefName" filledInOptionals.headRefName Encode.string, Argument.optional "baseRefName" filledInOptionals.baseRefName Encode.string, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeIssueOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "associatedPullRequests" optionalArgs object_ identity


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Ref
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The ref name.
-}
name : SelectionSet String Api.Object.Ref
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The ref's prefix, such as `refs/heads/` or `refs/tags/`.
-}
prefix : SelectionSet String Api.Object.Ref
prefix =
    Object.selectionForField "String" "prefix" [] Decode.string


{-| The repository the ref belongs to.
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.Ref
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The object the ref points to.
-}
target : SelectionSet decodesTo Api.Interface.GitObject -> SelectionSet decodesTo Api.Object.Ref
target object_ =
    Object.selectionForCompositeField "target" [] object_ identity
