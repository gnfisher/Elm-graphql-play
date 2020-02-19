-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Issue exposing (AssigneesOptionalArguments, CommentsOptionalArguments, HovercardOptionalArguments, LabelsOptionalArguments, ParticipantsOptionalArguments, ProjectCardsOptionalArguments, ReactionsOptionalArguments, TimelineItemsOptionalArguments, TimelineOptionalArguments, UserContentEditsOptionalArguments, activeLockReason, assignees, author, authorAssociation, body, bodyHTML, bodyText, closed, closedAt, comments, createdAt, createdViaEmail, databaseId, editor, hovercard, id, includesCreatedEdit, labels, lastEditedAt, locked, milestone, number, participants, projectCards, publishedAt, reactionGroups, reactions, repository, resourcePath, state, timeline, timelineItems, title, updatedAt, url, userContentEdits, viewerCanReact, viewerCanSubscribe, viewerCanUpdate, viewerCannotUpdateReasons, viewerDidAuthor, viewerSubscription)

import Api.Enum.CommentAuthorAssociation
import Api.Enum.CommentCannotUpdateReason
import Api.Enum.IssueState
import Api.Enum.IssueTimelineItemsItemType
import Api.Enum.LockReason
import Api.Enum.ProjectCardArchivedState
import Api.Enum.ReactionContent
import Api.Enum.SubscriptionState
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


{-| Reason that the conversation was locked.
-}
activeLockReason : SelectionSet (Maybe Api.Enum.LockReason.LockReason) Api.Object.Issue
activeLockReason =
    Object.selectionForField "(Maybe Enum.LockReason.LockReason)" "activeLockReason" [] (Api.Enum.LockReason.decoder |> Decode.nullable)


type alias AssigneesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of Users assigned to this object.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
assignees : (AssigneesOptionalArguments -> AssigneesOptionalArguments) -> SelectionSet decodesTo Api.Object.UserConnection -> SelectionSet decodesTo Api.Object.Issue
assignees fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "assignees" optionalArgs object_ identity


{-| The actor who authored the comment.
-}
author : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.Issue
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Author's association with the subject of the comment.
-}
authorAssociation : SelectionSet Api.Enum.CommentAuthorAssociation.CommentAuthorAssociation Api.Object.Issue
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] Api.Enum.CommentAuthorAssociation.decoder


{-| Identifies the body of the issue.
-}
body : SelectionSet String Api.Object.Issue
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| The body rendered to HTML.
-}
bodyHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.Issue
bodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "bodyHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| Identifies the body of the issue rendered to text.
-}
bodyText : SelectionSet String Api.Object.Issue
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| `true` if the object is closed (definition of closed may depend on type)
-}
closed : SelectionSet Bool Api.Object.Issue
closed =
    Object.selectionForField "Bool" "closed" [] Decode.bool


{-| Identifies the date and time when the object was closed.
-}
closedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Issue
closedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "closedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


type alias CommentsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of comments associated with the Issue.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
comments : (CommentsOptionalArguments -> CommentsOptionalArguments) -> SelectionSet decodesTo Api.Object.IssueCommentConnection -> SelectionSet decodesTo Api.Object.Issue
comments fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "comments" optionalArgs object_ identity


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Issue
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool Api.Object.Issue
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Api.Object.Issue
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The actor who edited the comment.
-}
editor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.Issue
editor object_ =
    Object.selectionForCompositeField "editor" [] object_ (identity >> Decode.nullable)


type alias HovercardOptionalArguments =
    { includeNotificationContexts : OptionalArgument Bool }


{-| The hovercard information for this issue

  - includeNotificationContexts - Whether or not to include notification contexts

-}
hovercard : (HovercardOptionalArguments -> HovercardOptionalArguments) -> SelectionSet decodesTo Api.Object.Hovercard -> SelectionSet decodesTo Api.Object.Issue
hovercard fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { includeNotificationContexts = Absent }

        optionalArgs =
            [ Argument.optional "includeNotificationContexts" filledInOptionals.includeNotificationContexts Encode.bool ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "hovercard" optionalArgs object_ identity


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Issue
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Check if this comment was edited and includes an edit with the creation data
-}
includesCreatedEdit : SelectionSet Bool Api.Object.Issue
includesCreatedEdit =
    Object.selectionForField "Bool" "includesCreatedEdit" [] Decode.bool


type alias LabelsOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.LabelOrder
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of labels associated with the object.

  - orderBy - Ordering options for labels returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
labels : (LabelsOptionalArguments -> LabelsOptionalArguments) -> SelectionSet decodesTo Api.Object.LabelConnection -> SelectionSet (Maybe decodesTo) Api.Object.Issue
labels fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeLabelOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "labels" optionalArgs object_ (identity >> Decode.nullable)


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Issue
lastEditedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "lastEditedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| `true` if the object is locked
-}
locked : SelectionSet Bool Api.Object.Issue
locked =
    Object.selectionForField "Bool" "locked" [] Decode.bool


{-| Identifies the milestone associated with the issue.
-}
milestone : SelectionSet decodesTo Api.Object.Milestone -> SelectionSet (Maybe decodesTo) Api.Object.Issue
milestone object_ =
    Object.selectionForCompositeField "milestone" [] object_ (identity >> Decode.nullable)


{-| Identifies the issue number.
-}
number : SelectionSet Int Api.Object.Issue
number =
    Object.selectionForField "Int" "number" [] Decode.int


type alias ParticipantsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of Users that are participating in the Issue conversation.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
participants : (ParticipantsOptionalArguments -> ParticipantsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserConnection -> SelectionSet decodesTo Api.Object.Issue
participants fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "participants" optionalArgs object_ identity


type alias ProjectCardsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , archivedStates : OptionalArgument (List (Maybe Api.Enum.ProjectCardArchivedState.ProjectCardArchivedState))
    }


{-| List of project cards associated with this issue.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - archivedStates - A list of archived states to filter the cards by

-}
projectCards : (ProjectCardsOptionalArguments -> ProjectCardsOptionalArguments) -> SelectionSet decodesTo Api.Object.ProjectCardConnection -> SelectionSet decodesTo Api.Object.Issue
projectCards fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, archivedStates = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "archivedStates" filledInOptionals.archivedStates (Encode.enum Api.Enum.ProjectCardArchivedState.toString |> Encode.maybe |> Encode.list) ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "projectCards" optionalArgs object_ identity


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Issue
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups : SelectionSet decodesTo Api.Object.ReactionGroup -> SelectionSet (Maybe (List decodesTo)) Api.Object.Issue
reactionGroups object_ =
    Object.selectionForCompositeField "reactionGroups" [] object_ (identity >> Decode.list >> Decode.nullable)


type alias ReactionsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , content : OptionalArgument Api.Enum.ReactionContent.ReactionContent
    , orderBy : OptionalArgument Api.InputObject.ReactionOrder
    }


{-| A list of Reactions left on the Issue.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - content - Allows filtering Reactions by emoji.
  - orderBy - Allows specifying the order in which reactions are returned.

-}
reactions : (ReactionsOptionalArguments -> ReactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.ReactionConnection -> SelectionSet decodesTo Api.Object.Issue
reactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, content = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "content" filledInOptionals.content (Encode.enum Api.Enum.ReactionContent.toString), Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeReactionOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "reactions" optionalArgs object_ identity


{-| The repository associated with this node.
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.Issue
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The HTTP path for this issue
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Issue
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the state of the issue.
-}
state : SelectionSet Api.Enum.IssueState.IssueState Api.Object.Issue
state =
    Object.selectionForField "Enum.IssueState.IssueState" "state" [] Api.Enum.IssueState.decoder


type alias TimelineOptionalArguments =
    { since : OptionalArgument Api.ScalarCodecs.DateTime
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of events, comments, commits, etc. associated with the issue.

  - since - Allows filtering timeline events by a `since` timestamp.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
timeline : (TimelineOptionalArguments -> TimelineOptionalArguments) -> SelectionSet decodesTo Api.Object.IssueTimelineConnection -> SelectionSet decodesTo Api.Object.Issue
timeline fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { since = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "since" filledInOptionals.since (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecDateTime), Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "timeline" optionalArgs object_ identity


type alias TimelineItemsOptionalArguments =
    { since : OptionalArgument Api.ScalarCodecs.DateTime
    , skip : OptionalArgument Int
    , itemTypes : OptionalArgument (List Api.Enum.IssueTimelineItemsItemType.IssueTimelineItemsItemType)
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of events, comments, commits, etc. associated with the issue.

  - since - Filter timeline items by a `since` timestamp.
  - skip - Skips the first _n_ elements in the list.
  - itemTypes - Filter timeline items by type.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
timelineItems : (TimelineItemsOptionalArguments -> TimelineItemsOptionalArguments) -> SelectionSet decodesTo Api.Object.IssueTimelineItemsConnection -> SelectionSet decodesTo Api.Object.Issue
timelineItems fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { since = Absent, skip = Absent, itemTypes = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "since" filledInOptionals.since (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecDateTime), Argument.optional "skip" filledInOptionals.skip Encode.int, Argument.optional "itemTypes" filledInOptionals.itemTypes (Encode.enum Api.Enum.IssueTimelineItemsItemType.toString |> Encode.list), Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "timelineItems" optionalArgs object_ identity


{-| Identifies the issue title.
-}
title : SelectionSet String Api.Object.Issue
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Issue
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this issue
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.Issue
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


type alias UserContentEditsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of edits to this content.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
userContentEdits : (UserContentEditsOptionalArguments -> UserContentEditsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserContentEditConnection -> SelectionSet (Maybe decodesTo) Api.Object.Issue
userContentEdits fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "userContentEdits" optionalArgs object_ (identity >> Decode.nullable)


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool Api.Object.Issue
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool Api.Object.Issue
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool Api.Object.Issue
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List Api.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) Api.Object.Issue
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (Api.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool Api.Object.Issue
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet (Maybe Api.Enum.SubscriptionState.SubscriptionState) Api.Object.Issue
viewerSubscription =
    Object.selectionForField "(Maybe Enum.SubscriptionState.SubscriptionState)" "viewerSubscription" [] (Api.Enum.SubscriptionState.decoder |> Decode.nullable)