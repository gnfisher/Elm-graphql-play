-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Milestone exposing (IssuesOptionalArguments, PullRequestsOptionalArguments, closed, closedAt, createdAt, creator, description, dueOn, id, issuePrioritiesDebug, issues, number, pullRequests, repository, resourcePath, state, title, updatedAt, url)

import Api.Enum.IssueState
import Api.Enum.MilestoneState
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


{-| `true` if the object is closed (definition of closed may depend on type)
-}
closed : SelectionSet Bool Api.Object.Milestone
closed =
    Object.selectionForField "Bool" "closed" [] Decode.bool


{-| Identifies the date and time when the object was closed.
-}
closedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Milestone
closedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "closedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Milestone
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Identifies the actor who created the milestone.
-}
creator : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.Milestone
creator object_ =
    Object.selectionForCompositeField "creator" [] object_ (identity >> Decode.nullable)


{-| Identifies the description of the milestone.
-}
description : SelectionSet (Maybe String) Api.Object.Milestone
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


{-| Identifies the due date of the milestone.
-}
dueOn : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Milestone
dueOn =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "dueOn" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Milestone
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Just for debugging on review-lab
-}
issuePrioritiesDebug : SelectionSet String Api.Object.Milestone
issuePrioritiesDebug =
    Object.selectionForField "String" "issuePrioritiesDebug" [] Decode.string


type alias IssuesOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.IssueOrder
    , labels : OptionalArgument (List String)
    , states : OptionalArgument (List Api.Enum.IssueState.IssueState)
    , filterBy : OptionalArgument Api.InputObject.IssueFilters
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of issues associated with the milestone.

  - orderBy - Ordering options for issues returned from the connection.
  - labels - A list of label names to filter the pull requests by.
  - states - A list of states to filter the issues by.
  - filterBy - Filtering options for issues returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
issues : (IssuesOptionalArguments -> IssuesOptionalArguments) -> SelectionSet decodesTo Api.Object.IssueConnection -> SelectionSet decodesTo Api.Object.Milestone
issues fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, labels = Absent, states = Absent, filterBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeIssueOrder, Argument.optional "labels" filledInOptionals.labels (Encode.string |> Encode.list), Argument.optional "states" filledInOptionals.states (Encode.enum Api.Enum.IssueState.toString |> Encode.list), Argument.optional "filterBy" filledInOptionals.filterBy Api.InputObject.encodeIssueFilters, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "issues" optionalArgs object_ identity


{-| Identifies the number of the milestone.
-}
number : SelectionSet Int Api.Object.Milestone
number =
    Object.selectionForField "Int" "number" [] Decode.int


type alias PullRequestsOptionalArguments =
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


{-| A list of pull requests associated with the milestone.

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
pullRequests : (PullRequestsOptionalArguments -> PullRequestsOptionalArguments) -> SelectionSet decodesTo Api.Object.PullRequestConnection -> SelectionSet decodesTo Api.Object.Milestone
pullRequests fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { states = Absent, labels = Absent, headRefName = Absent, baseRefName = Absent, orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "states" filledInOptionals.states (Encode.enum Api.Enum.PullRequestState.toString |> Encode.list), Argument.optional "labels" filledInOptionals.labels (Encode.string |> Encode.list), Argument.optional "headRefName" filledInOptionals.headRefName Encode.string, Argument.optional "baseRefName" filledInOptionals.baseRefName Encode.string, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeIssueOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "pullRequests" optionalArgs object_ identity


{-| The repository associated with this milestone.
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.Milestone
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The HTTP path for this milestone
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Milestone
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the state of the milestone.
-}
state : SelectionSet Api.Enum.MilestoneState.MilestoneState Api.Object.Milestone
state =
    Object.selectionForField "Enum.MilestoneState.MilestoneState" "state" [] Api.Enum.MilestoneState.decoder


{-| Identifies the title of the milestone.
-}
title : SelectionSet String Api.Object.Milestone
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Milestone
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this milestone
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.Milestone
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)
