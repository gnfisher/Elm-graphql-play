-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.CommitComment exposing (ReactionsOptionalArguments, UserContentEditsOptionalArguments, author, authorAssociation, body, bodyHTML, bodyText, commit, createdAt, createdViaEmail, databaseId, editor, id, includesCreatedEdit, isMinimized, lastEditedAt, minimizedReason, path, position, publishedAt, reactionGroups, reactions, repository, resourcePath, updatedAt, url, userContentEdits, viewerCanDelete, viewerCanMinimize, viewerCanReact, viewerCanUpdate, viewerCannotUpdateReasons, viewerDidAuthor)

import Api.Enum.CommentAuthorAssociation
import Api.Enum.CommentCannotUpdateReason
import Api.Enum.ReactionContent
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
author : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.CommitComment
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Author's association with the subject of the comment.
-}
authorAssociation : SelectionSet Api.Enum.CommentAuthorAssociation.CommentAuthorAssociation Api.Object.CommitComment
authorAssociation =
    Object.selectionForField "Enum.CommentAuthorAssociation.CommentAuthorAssociation" "authorAssociation" [] Api.Enum.CommentAuthorAssociation.decoder


{-| Identifies the comment body.
-}
body : SelectionSet String Api.Object.CommitComment
body =
    Object.selectionForField "String" "body" [] Decode.string


{-| The body rendered to HTML.
-}
bodyHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.CommitComment
bodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "bodyHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| The body rendered to text.
-}
bodyText : SelectionSet String Api.Object.CommitComment
bodyText =
    Object.selectionForField "String" "bodyText" [] Decode.string


{-| Identifies the commit associated with the comment, if the commit exists.
-}
commit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (Maybe decodesTo) Api.Object.CommitComment
commit object_ =
    Object.selectionForCompositeField "commit" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.CommitComment
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if this comment was created via an email reply.
-}
createdViaEmail : SelectionSet Bool Api.Object.CommitComment
createdViaEmail =
    Object.selectionForField "Bool" "createdViaEmail" [] Decode.bool


{-| Identifies the primary key from the database.
-}
databaseId : SelectionSet (Maybe Int) Api.Object.CommitComment
databaseId =
    Object.selectionForField "(Maybe Int)" "databaseId" [] (Decode.int |> Decode.nullable)


{-| The actor who edited the comment.
-}
editor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.CommitComment
editor object_ =
    Object.selectionForCompositeField "editor" [] object_ (identity >> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.CommitComment
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Check if this comment was edited and includes an edit with the creation data
-}
includesCreatedEdit : SelectionSet Bool Api.Object.CommitComment
includesCreatedEdit =
    Object.selectionForField "Bool" "includesCreatedEdit" [] Decode.bool


{-| Returns whether or not a comment has been minimized.
-}
isMinimized : SelectionSet Bool Api.Object.CommitComment
isMinimized =
    Object.selectionForField "Bool" "isMinimized" [] Decode.bool


{-| The moment the editor made the last edit
-}
lastEditedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.CommitComment
lastEditedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "lastEditedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| Returns why the comment was minimized.
-}
minimizedReason : SelectionSet (Maybe String) Api.Object.CommitComment
minimizedReason =
    Object.selectionForField "(Maybe String)" "minimizedReason" [] (Decode.string |> Decode.nullable)


{-| Identifies the file path associated with the comment.
-}
path : SelectionSet (Maybe String) Api.Object.CommitComment
path =
    Object.selectionForField "(Maybe String)" "path" [] (Decode.string |> Decode.nullable)


{-| Identifies the line position associated with the comment.
-}
position : SelectionSet (Maybe Int) Api.Object.CommitComment
position =
    Object.selectionForField "(Maybe Int)" "position" [] (Decode.int |> Decode.nullable)


{-| Identifies when the comment was published at.
-}
publishedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.CommitComment
publishedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "publishedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| A list of reactions grouped by content left on the subject.
-}
reactionGroups : SelectionSet decodesTo Api.Object.ReactionGroup -> SelectionSet (Maybe (List decodesTo)) Api.Object.CommitComment
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
reactions : (ReactionsOptionalArguments -> ReactionsOptionalArguments) -> SelectionSet decodesTo Api.Object.ReactionConnection -> SelectionSet decodesTo Api.Object.CommitComment
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
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.CommitComment
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The HTTP path permalink for this commit comment.
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.CommitComment
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.CommitComment
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL permalink for this commit comment.
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.CommitComment
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
userContentEdits : (UserContentEditsOptionalArguments -> UserContentEditsOptionalArguments) -> SelectionSet decodesTo Api.Object.UserContentEditConnection -> SelectionSet (Maybe decodesTo) Api.Object.CommitComment
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
viewerCanDelete : SelectionSet Bool Api.Object.CommitComment
viewerCanDelete =
    Object.selectionForField "Bool" "viewerCanDelete" [] Decode.bool


{-| Check if the current viewer can minimize this object.
-}
viewerCanMinimize : SelectionSet Bool Api.Object.CommitComment
viewerCanMinimize =
    Object.selectionForField "Bool" "viewerCanMinimize" [] Decode.bool


{-| Can user react to this subject
-}
viewerCanReact : SelectionSet Bool Api.Object.CommitComment
viewerCanReact =
    Object.selectionForField "Bool" "viewerCanReact" [] Decode.bool


{-| Check if the current viewer can update this object.
-}
viewerCanUpdate : SelectionSet Bool Api.Object.CommitComment
viewerCanUpdate =
    Object.selectionForField "Bool" "viewerCanUpdate" [] Decode.bool


{-| Reasons why the current viewer can not update this comment.
-}
viewerCannotUpdateReasons : SelectionSet (List Api.Enum.CommentCannotUpdateReason.CommentCannotUpdateReason) Api.Object.CommitComment
viewerCannotUpdateReasons =
    Object.selectionForField "(List Enum.CommentCannotUpdateReason.CommentCannotUpdateReason)" "viewerCannotUpdateReasons" [] (Api.Enum.CommentCannotUpdateReason.decoder |> Decode.list)


{-| Did the viewer author this comment.
-}
viewerDidAuthor : SelectionSet Bool Api.Object.CommitComment
viewerDidAuthor =
    Object.selectionForField "Bool" "viewerDidAuthor" [] Decode.bool
