-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Commit exposing (AssociatedPullRequestsOptionalArguments, BlameRequiredArguments, CommentsOptionalArguments, DeploymentsOptionalArguments, HistoryOptionalArguments, ParentsOptionalArguments, abbreviatedOid, additions, associatedPullRequests, author, authoredByCommitter, authoredDate, blame, changedFiles, comments, commitResourcePath, commitUrl, committedDate, committedViaWeb, committer, deletions, deployments, history, id, message, messageBody, messageBodyHTML, messageHeadline, messageHeadlineHTML, oid, parents, pushedDate, repository, resourcePath, signature, status, tarballUrl, tree, treeResourcePath, treeUrl, url, viewerCanSubscribe, viewerSubscription, zipballUrl)

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


{-| An abbreviated version of the Git object ID
-}
abbreviatedOid : SelectionSet String Api.Object.Commit
abbreviatedOid =
    Object.selectionForField "String" "abbreviatedOid" [] Decode.string


{-| The number of additions in this commit.
-}
additions : SelectionSet Int Api.Object.Commit
additions =
    Object.selectionForField "Int" "additions" [] Decode.int


type alias AssociatedPullRequestsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , orderBy : OptionalArgument Api.InputObject.PullRequestOrder
    }


{-| The pull requests associated with a commit

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - orderBy - Ordering options for pull requests.

-}
associatedPullRequests : (AssociatedPullRequestsOptionalArguments -> AssociatedPullRequestsOptionalArguments) -> SelectionSet decodesTo Api.Object.PullRequestConnection -> SelectionSet (Maybe decodesTo) Api.Object.Commit
associatedPullRequests fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodePullRequestOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "associatedPullRequests" optionalArgs object_ (identity >> Decode.nullable)


{-| Authorship details of the commit.
-}
author : SelectionSet decodesTo Api.Object.GitActor -> SelectionSet (Maybe decodesTo) Api.Object.Commit
author object_ =
    Object.selectionForCompositeField "author" [] object_ (identity >> Decode.nullable)


{-| Check if the committer and the author match.
-}
authoredByCommitter : SelectionSet Bool Api.Object.Commit
authoredByCommitter =
    Object.selectionForField "Bool" "authoredByCommitter" [] Decode.bool


{-| The datetime when this commit was authored.
-}
authoredDate : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Commit
authoredDate =
    Object.selectionForField "ScalarCodecs.DateTime" "authoredDate" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


type alias BlameRequiredArguments =
    { path : String }


{-| Fetches `git blame` information.

  - path - The file whose Git blame information you want.

-}
blame : BlameRequiredArguments -> SelectionSet decodesTo Api.Object.Blame -> SelectionSet decodesTo Api.Object.Commit
blame requiredArgs object_ =
    Object.selectionForCompositeField "blame" [ Argument.required "path" requiredArgs.path Encode.string ] object_ identity


{-| The number of changed files in this commit.
-}
changedFiles : SelectionSet Int Api.Object.Commit
changedFiles =
    Object.selectionForField "Int" "changedFiles" [] Decode.int


type alias CommentsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| Comments made on the commit.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
comments : (CommentsOptionalArguments -> CommentsOptionalArguments) -> SelectionSet decodesTo Api.Object.CommitCommentConnection -> SelectionSet decodesTo Api.Object.Commit
comments fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "comments" optionalArgs object_ identity


{-| The HTTP path for this Git object
-}
commitResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
commitResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "commitResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this Git object
-}
commitUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
commitUrl =
    Object.selectionForField "ScalarCodecs.Uri" "commitUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The datetime when this commit was committed.
-}
committedDate : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Commit
committedDate =
    Object.selectionForField "ScalarCodecs.DateTime" "committedDate" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Check if commited via GitHub web UI.
-}
committedViaWeb : SelectionSet Bool Api.Object.Commit
committedViaWeb =
    Object.selectionForField "Bool" "committedViaWeb" [] Decode.bool


{-| Committership details of the commit.
-}
committer : SelectionSet decodesTo Api.Object.GitActor -> SelectionSet (Maybe decodesTo) Api.Object.Commit
committer object_ =
    Object.selectionForCompositeField "committer" [] object_ (identity >> Decode.nullable)


{-| The number of deletions in this commit.
-}
deletions : SelectionSet Int Api.Object.Commit
deletions =
    Object.selectionForField "Int" "deletions" [] Decode.int


type alias DeploymentsOptionalArguments =
    { environments : OptionalArgument (List String)
    , orderBy : OptionalArgument Api.InputObject.DeploymentOrder
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| The deployments associated with a commit.

  - environments - Environments to list deployments for
  - orderBy - Ordering options for deployments returned from the connection.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
deployments : (DeploymentsOptionalArguments -> DeploymentsOptionalArguments) -> SelectionSet decodesTo Api.Object.DeploymentConnection -> SelectionSet (Maybe decodesTo) Api.Object.Commit
deployments fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { environments = Absent, orderBy = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "environments" filledInOptionals.environments (Encode.string |> Encode.list), Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeDeploymentOrder, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "deployments" optionalArgs object_ (identity >> Decode.nullable)


type alias HistoryOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , path : OptionalArgument String
    , author : OptionalArgument Api.InputObject.CommitAuthor
    , since : OptionalArgument Api.ScalarCodecs.GitTimestamp
    , until : OptionalArgument Api.ScalarCodecs.GitTimestamp
    }


{-| The linear commit history starting from (and including) this commit, in the same order as `git log`.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - path - If non-null, filters history to only show commits touching files under this path.
  - author - If non-null, filters history to only show commits with matching authorship.
  - since - Allows specifying a beginning time or date for fetching commits.
  - until - Allows specifying an ending time or date for fetching commits.

-}
history : (HistoryOptionalArguments -> HistoryOptionalArguments) -> SelectionSet decodesTo Api.Object.CommitHistoryConnection -> SelectionSet decodesTo Api.Object.Commit
history fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, path = Absent, author = Absent, since = Absent, until = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "path" filledInOptionals.path Encode.string, Argument.optional "author" filledInOptionals.author Api.InputObject.encodeCommitAuthor, Argument.optional "since" filledInOptionals.since (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecGitTimestamp), Argument.optional "until" filledInOptionals.until (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapEncoder .codecGitTimestamp) ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "history" optionalArgs object_ identity


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Commit
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The Git commit message
-}
message : SelectionSet String Api.Object.Commit
message =
    Object.selectionForField "String" "message" [] Decode.string


{-| The Git commit message body
-}
messageBody : SelectionSet String Api.Object.Commit
messageBody =
    Object.selectionForField "String" "messageBody" [] Decode.string


{-| The commit message body rendered to HTML.
-}
messageBodyHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.Commit
messageBodyHTML =
    Object.selectionForField "ScalarCodecs.Html" "messageBodyHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| The Git commit message headline
-}
messageHeadline : SelectionSet String Api.Object.Commit
messageHeadline =
    Object.selectionForField "String" "messageHeadline" [] Decode.string


{-| The commit message headline rendered to HTML.
-}
messageHeadlineHTML : SelectionSet Api.ScalarCodecs.Html Api.Object.Commit
messageHeadlineHTML =
    Object.selectionForField "ScalarCodecs.Html" "messageHeadlineHTML" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecHtml |> .decoder)


{-| The Git object ID
-}
oid : SelectionSet Api.ScalarCodecs.GitObjectID Api.Object.Commit
oid =
    Object.selectionForField "ScalarCodecs.GitObjectID" "oid" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecGitObjectID |> .decoder)


type alias ParentsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| The parents of a commit.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
parents : (ParentsOptionalArguments -> ParentsOptionalArguments) -> SelectionSet decodesTo Api.Object.CommitConnection -> SelectionSet decodesTo Api.Object.Commit
parents fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "parents" optionalArgs object_ identity


{-| The datetime when this commit was pushed.
-}
pushedDate : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.Commit
pushedDate =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "pushedDate" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| The Repository this commit belongs to
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.Commit
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ identity


{-| The HTTP path for this commit
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Commit signing information, if present.
-}
signature : SelectionSet decodesTo Api.Interface.GitSignature -> SelectionSet (Maybe decodesTo) Api.Object.Commit
signature object_ =
    Object.selectionForCompositeField "signature" [] object_ (identity >> Decode.nullable)


{-| Status information for this commit
-}
status : SelectionSet decodesTo Api.Object.Status -> SelectionSet (Maybe decodesTo) Api.Object.Commit
status object_ =
    Object.selectionForCompositeField "status" [] object_ (identity >> Decode.nullable)


{-| Returns a URL to download a tarball archive for a repository.
Note: For private repositories, these links are temporary and expire after five minutes.
-}
tarballUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
tarballUrl =
    Object.selectionForField "ScalarCodecs.Uri" "tarballUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Commit's root Tree
-}
tree : SelectionSet decodesTo Api.Object.Tree -> SelectionSet decodesTo Api.Object.Commit
tree object_ =
    Object.selectionForCompositeField "tree" [] object_ identity


{-| The HTTP path for the tree of this commit
-}
treeResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
treeResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "treeResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for the tree of this commit
-}
treeUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
treeUrl =
    Object.selectionForField "ScalarCodecs.Uri" "treeUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this commit
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool Api.Object.Commit
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet (Maybe Api.Enum.SubscriptionState.SubscriptionState) Api.Object.Commit
viewerSubscription =
    Object.selectionForField "(Maybe Enum.SubscriptionState.SubscriptionState)" "viewerSubscription" [] (Api.Enum.SubscriptionState.decoder |> Decode.nullable)


{-| Returns a URL to download a zipball archive for a repository.
Note: For private repositories, these links are temporary and expire after five minutes.
-}
zipballUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Commit
zipballUrl =
    Object.selectionForField "ScalarCodecs.Uri" "zipballUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)
