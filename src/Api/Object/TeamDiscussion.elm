-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.TeamDiscussion exposing (CommentsOptionalArguments, ReactionsOptionalArguments, UserContentEditsOptionalArguments, author, authorAssociation, body, bodyHTML, bodyText, bodyVersion, comments, commentsResourcePath, commentsUrl, createdAt, createdViaEmail, databaseId, editor, id, includesCreatedEdit, isPinned, isPrivate, lastEditedAt, number, publishedAt, reactionGroups, reactions, resourcePath, team, title, updatedAt, url, userContentEdits, viewerCanDelete, viewerCanPin, viewerCanReact, viewerCanSubscribe, viewerCanUpdate, viewerCannotUpdateReasons, viewerDidAuthor, viewerSubscription)

import Api.Enum.CommentAuthorAssociation
import Api.Enum.CommentCannotUpdateReason
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


{-| The actor who authored the comment.
-}
author : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.TeamDiscussion
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Author's association with the discussion's team.
-}
authorAssociation : SelectionSet Api.Enum.CommentAuthorAssociation.CommentAuthorAssociation Api.Object.TeamDiscussion
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] Api.Enum.CommentAuthorAssociation.decoder


{-| The body as Markdown.
-}
body : SelectionSet String Api.Object.TeamDiscussion
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| The body rendered to HTML.
-}
bodyHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.TeamDiscussion
bodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "bodyHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| The body rendered to text.
-}
bodyText : SelectionSet String Api.Object.TeamDiscussion
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| Identifies the discussion body hash.
-}
bodyVersion : SelectionSet String Api.Object.TeamDiscussion
bodyVersion =
    Object.selectionForField "String" "bodyVersion" [] Decode.string


type alias CommentsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , orderBy : OptionalArgument Api.InputObject.TeamDiscussionCommentOrder
    , fromComment : OptionalArgument Int
    }


{-| A list of comments on this discussion.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - orderBy - Order for connection
  - fromComment - When provided, filters the connection such that results begin with the comment with this number.

-}
comments : (CommentsOptionalArguments -> CommentsOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamDiscussionCommentConnection -> SelectionSet decodesTo Api.Object.TeamDiscussion
comments fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, orderBy = Absent, fromComment = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeTeamDiscussionCommentOrder, Argument.optional "fromComment" filledInOptionals.fromComment Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "comments" optionalArgs object_ identity


{-| The HTTP path for discussion comments
-}
commentsResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamDiscussion
commentsResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "commentsResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for discussion comments
-}
commentsUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamDiscussion
commentsUrl =
    Object.selectionForField "ScalarCodecs.Uri" "commentsUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.TeamDiscussion
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool Api.Object.TeamDiscussion
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Api.Object.TeamDiscussion
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The actor who edited the comment.
-}
editor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.TeamDiscussion
editor object_ =
    Object.selectionForCompositeField "editor" [] object_ (identity >> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.TeamDiscussion
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Check if this comment was edited and includes an edit with the creation data
-}
includesCreatedEdit : SelectionSet Bool Api.Object.TeamDiscussion
includesCreatedEdit =
    Object.selectionForField "Bool" "includesCreatedEdit" [] Decode.bool


{-| Whether or not the discussion is pinned.
-}
isPinned : SelectionSet Bool Api.Object.TeamDiscussion
isPinned =
    Object.selectionForField "Bool" "isPinned" [] Decode.bool


{-| Whether or not the discussion is only visible to team members and org admins.
-}
isPrivate : SelectionSet Bool Api.Object.TeamDiscussion
isPrivate =
    Object.selectionForField "Bool" "isPrivate" [] Decode.bool


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.TeamDiscussion
lastEditedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "lastEditedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| Identifies the discussion within its team.
-}
number : SelectionSet Int Api.Object.TeamDiscussion
number =
    Object.selectionForField "Int" "number" [] Decode.int


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.TeamDiscussion
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups : SelectionSet decodesTo Api.Object.ReactionGroup -> SelectionSet (Maybe (List decodesTo)) Api.Object.TeamDiscussion
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
reactions : (ReactionsOptionalArguments -> ReactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.ReactionConnection -> SelectionSet decodesTo Api.Object.TeamDiscussion
reactions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, content = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "content" filledInOptionals.content (Encode.enum Api.Enum.ReactionContent.toString), Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeReactionOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "reactions" optionalArgs object_ identity


{-| The HTTP path for this discussion
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamDiscussion
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The team that defines the context of this discussion.
-}
team : SelectionSet decodesTo Api.Object.Team -> SelectionSet decodesTo Api.Object.TeamDiscussion
team object_ =
    Object.selectionForCompositeField "team" [] object_ identity


{-| The title of the discussion
-}
title : SelectionSet String Api.Object.TeamDiscussion
title =
    Object.selectionForField "String" "title" [] Decode.string


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.TeamDiscussion
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this discussion
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.TeamDiscussion
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
userContentEdits : (UserContentEditsOptionalArguments -> UserContentEditsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserContentEditConnection -> SelectionSet (Maybe decodesTo) Api.Object.TeamDiscussion
userContentEdits fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "userContentEdits" optionalArgs object_ (identity >> Decode.nullable)


{-| Check if the current viewer can delete this object.
-}
viewerCanDelete : SelectionSet Bool Api.Object.TeamDiscussion
viewerCanDelete =
    Object.selectionForField "Bool" "viewerCanDelete" [] Decode.bool


{-| Whether or not the current viewer can pin this discussion.
-}
viewerCanPin : SelectionSet Bool Api.Object.TeamDiscussion
viewerCanPin =
    Object.selectionForField "Bool" "viewerCanPin" [] Decode.bool


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool Api.Object.TeamDiscussion
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool Api.Object.TeamDiscussion
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool Api.Object.TeamDiscussion
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List Api.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) Api.Object.TeamDiscussion
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (Api.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool Api.Object.TeamDiscussion
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet (Maybe Api.Enum.SubscriptionState.SubscriptionState) Api.Object.TeamDiscussion
viewerSubscription =
    Object.selectionForField "(Maybe Enum.SubscriptionState.SubscriptionState)" "viewerSubscription" [] (Api.Enum.SubscriptionState.decoder |> Decode.nullable)
