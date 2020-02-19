-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Team exposing (AncestorsOptionalArguments, AvatarUrlOptionalArguments, ChildTeamsOptionalArguments, DiscussionRequiredArguments, DiscussionsOptionalArguments, InvitationsOptionalArguments, MemberStatusesOptionalArguments, MembersOptionalArguments, RepositoriesOptionalArguments, ancestors, avatarUrl, childTeams, combinedSlug, createdAt, description, discussion, discussions, discussionsResourcePath, discussionsUrl, editTeamResourcePath, editTeamUrl, id, invitations, memberStatuses, members, membersResourcePath, membersUrl, name, newTeamResourcePath, newTeamUrl, organization, parentTeam, privacy, repositories, repositoriesResourcePath, repositoriesUrl, resourcePath, slug, teamsResourcePath, teamsUrl, updatedAt, url, viewerCanAdminister, viewerCanSubscribe, viewerSubscription)

import Api.Enum.SubscriptionState
import Api.Enum.TeamMemberRole
import Api.Enum.TeamMembershipType
import Api.Enum.TeamPrivacy
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


type alias AncestorsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of teams that are ancestors of this team.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
ancestors : (AncestorsOptionalArguments -> AncestorsOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamConnection -> SelectionSet decodesTo Api.Object.Team
ancestors fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "ancestors" optionalArgs object_ identity


type alias AvatarUrlOptionalArguments =
    { size : OptionalArgument Int }


{-| A URL pointing to the team's avatar.

  - size - The size in pixels of the resulting square image.

-}
avatarUrl : (AvatarUrlOptionalArguments -> AvatarUrlOptionalArguments) -> SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.Team
avatarUrl fillInOptionals =
    let
        filledInOptionals =
            fillInOptionals { size = Absent }

        optionalArgs =
            [ Argument.optional "size" filledInOptionals.size Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "avatarUrl" optionalArgs (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


type alias ChildTeamsOptionalArguments =
    { orderBy : OptionalArgument Api.InputObject.TeamOrder
    , userLogins : OptionalArgument (List String)
    , immediateOnly : OptionalArgument Bool
    , after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| List of child teams belonging to this team

  - orderBy - Order for connection
  - userLogins - User logins to filter by
  - immediateOnly - Whether to list immediate child teams or all descendant child teams.
  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
childTeams : (ChildTeamsOptionalArguments -> ChildTeamsOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamConnection -> SelectionSet decodesTo Api.Object.Team
childTeams fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { orderBy = Absent, userLogins = Absent, immediateOnly = Absent, after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeTeamOrder, Argument.optional "userLogins" filledInOptionals.userLogins (Encode.string |> Encode.list), Argument.optional "immediateOnly" filledInOptionals.immediateOnly Encode.bool, Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "childTeams" optionalArgs object_ identity


{-| The slug corresponding to the organization and team.
-}
combinedSlug : SelectionSet String Api.Object.Team
combinedSlug =
    Object.selectionForField "String" "combinedSlug" [] Decode.string


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Team
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The description of the team.
-}
description : SelectionSet (Maybe String) Api.Object.Team
description =
    Object.selectionForField "(Maybe String)" "description" [] (Decode.string |> Decode.nullable)


type alias DiscussionRequiredArguments =
    { number : Int }


{-| Find a team discussion by its number.

  - number - The sequence number of the discussion to find.

-}
discussion : DiscussionRequiredArguments -> SelectionSet decodesTo Api.Object.TeamDiscussion -> SelectionSet (Maybe decodesTo) Api.Object.Team
discussion requiredArgs object_ =
    Object.selectionForCompositeField "discussion" [ Argument.required "number" requiredArgs.number Encode.int ] object_ (identity >> Decode.nullable)


type alias DiscussionsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , isPinned : OptionalArgument Bool
    , orderBy : OptionalArgument Api.InputObject.TeamDiscussionOrder
    }


{-| A list of team discussions.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - isPinned - If provided, filters discussions according to whether or not they are pinned.
  - orderBy - Order for connection

-}
discussions : (DiscussionsOptionalArguments -> DiscussionsOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamDiscussionConnection -> SelectionSet decodesTo Api.Object.Team
discussions fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, isPinned = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "isPinned" filledInOptionals.isPinned Encode.bool, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeTeamDiscussionOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "discussions" optionalArgs object_ identity


{-| The HTTP path for team discussions
-}
discussionsResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
discussionsResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "discussionsResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for team discussions
-}
discussionsUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
discussionsUrl =
    Object.selectionForField "ScalarCodecs.Uri" "discussionsUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP path for editing this team
-}
editTeamResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
editTeamResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "editTeamResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for editing this team
-}
editTeamUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
editTeamUrl =
    Object.selectionForField "ScalarCodecs.Uri" "editTeamUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.Team
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


type alias InvitationsOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    }


{-| A list of pending invitations for users to this team

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.

-}
invitations : (InvitationsOptionalArguments -> InvitationsOptionalArguments) -> SelectionSet decodesTo Api.Object.OrganizationInvitationConnection -> SelectionSet (Maybe decodesTo) Api.Object.Team
invitations fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "invitations" optionalArgs object_ (identity >> Decode.nullable)


type alias MemberStatusesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , orderBy : OptionalArgument Api.InputObject.UserStatusOrder
    }


{-| Get the status messages members of this entity have set that are either public or visible only to the organization.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - orderBy - Ordering options for user statuses returned from the connection.

-}
memberStatuses : (MemberStatusesOptionalArguments -> MemberStatusesOptionalArguments) -> SelectionSet decodesTo Api.Object.UserStatusConnection -> SelectionSet decodesTo Api.Object.Team
memberStatuses fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeUserStatusOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "memberStatuses" optionalArgs object_ identity


type alias MembersOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , query : OptionalArgument String
    , membership : OptionalArgument Api.Enum.TeamMembershipType.TeamMembershipType
    , role : OptionalArgument Api.Enum.TeamMemberRole.TeamMemberRole
    , orderBy : OptionalArgument Api.InputObject.TeamMemberOrder
    }


{-| A list of users who are members of this team.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - query - The search string to look for.
  - membership - Filter by membership type
  - role - Filter by team member role
  - orderBy - Order for the connection.

-}
members : (MembersOptionalArguments -> MembersOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamMemberConnection -> SelectionSet decodesTo Api.Object.Team
members fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, query = Absent, membership = Absent, role = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "query" filledInOptionals.query Encode.string, Argument.optional "membership" filledInOptionals.membership (Encode.enum Api.Enum.TeamMembershipType.toString), Argument.optional "role" filledInOptionals.role (Encode.enum Api.Enum.TeamMemberRole.toString), Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeTeamMemberOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "members" optionalArgs object_ identity


{-| The HTTP path for the team' members
-}
membersResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
membersResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "membersResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for the team' members
-}
membersUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
membersUrl =
    Object.selectionForField "ScalarCodecs.Uri" "membersUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The name of the team.
-}
name : SelectionSet String Api.Object.Team
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The HTTP path creating a new team
-}
newTeamResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
newTeamResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "newTeamResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL creating a new team
-}
newTeamUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
newTeamUrl =
    Object.selectionForField "ScalarCodecs.Uri" "newTeamUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The organization that owns this team.
-}
organization : SelectionSet decodesTo Api.Object.Organization -> SelectionSet decodesTo Api.Object.Team
organization object_ =
    Object.selectionForCompositeField "organization" [] object_ identity


{-| The parent team of the team.
-}
parentTeam : SelectionSet decodesTo Api.Object.Team -> SelectionSet (Maybe decodesTo) Api.Object.Team
parentTeam object_ =
    Object.selectionForCompositeField "parentTeam" [] object_ (identity >> Decode.nullable)


{-| The level of privacy the team has.
-}
privacy : SelectionSet Api.Enum.TeamPrivacy.TeamPrivacy Api.Object.Team
privacy =
    Object.selectionForField "Enum.TeamPrivacy.TeamPrivacy" "privacy" [] Api.Enum.TeamPrivacy.decoder


type alias RepositoriesOptionalArguments =
    { after : OptionalArgument String
    , before : OptionalArgument String
    , first : OptionalArgument Int
    , last : OptionalArgument Int
    , query : OptionalArgument String
    , orderBy : OptionalArgument Api.InputObject.TeamRepositoryOrder
    }


{-| A list of repositories this team has access to.

  - after - Returns the elements in the list that come after the specified cursor.
  - before - Returns the elements in the list that come before the specified cursor.
  - first - Returns the first _n_ elements from the list.
  - last - Returns the last _n_ elements from the list.
  - query - The search string to look for.
  - orderBy - Order for the connection.

-}
repositories : (RepositoriesOptionalArguments -> RepositoriesOptionalArguments) -> SelectionSet decodesTo Api.Object.TeamRepositoryConnection -> SelectionSet decodesTo Api.Object.Team
repositories fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { after = Absent, before = Absent, first = Absent, last = Absent, query = Absent, orderBy = Absent }

        optionalArgs =
            [ Argument.optional "after" filledInOptionals.after Encode.string, Argument.optional "before" filledInOptionals.before Encode.string, Argument.optional "first" filledInOptionals.first Encode.int, Argument.optional "last" filledInOptionals.last Encode.int, Argument.optional "query" filledInOptionals.query Encode.string, Argument.optional "orderBy" filledInOptionals.orderBy Api.InputObject.encodeTeamRepositoryOrder ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "repositories" optionalArgs object_ identity


{-| The HTTP path for this team's repositories
-}
repositoriesResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
repositoriesResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "repositoriesResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this team's repositories
-}
repositoriesUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
repositoriesUrl =
    Object.selectionForField "ScalarCodecs.Uri" "repositoriesUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP path for this team
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The slug corresponding to the team.
-}
slug : SelectionSet String Api.Object.Team
slug =
    Object.selectionForField "String" "slug" [] Decode.string


{-| The HTTP path for this team's teams
-}
teamsResourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
teamsResourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "teamsResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this team's teams
-}
teamsUrl : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
teamsUrl =
    Object.selectionForField "ScalarCodecs.Uri" "teamsUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.Team
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The HTTP URL for this team
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.Team
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| Team is adminable by the viewer.
-}
viewerCanAdminister : SelectionSet Bool Api.Object.Team
viewerCanAdminister =
    Object.selectionForField "Bool" "viewerCanAdminister" [] Decode.bool


{-| Check if the viewer is able to change their subscription status for the repository.
-}
viewerCanSubscribe : SelectionSet Bool Api.Object.Team
viewerCanSubscribe =
    Object.selectionForField "Bool" "viewerCanSubscribe" [] Decode.bool


{-| Identifies if the viewer is watching, not watching, or ignoring the subscribable entity.
-}
viewerSubscription : SelectionSet (Maybe Api.Enum.SubscriptionState.SubscriptionState) Api.Object.Team
viewerSubscription =
    Object.selectionForField "(Maybe Enum.SubscriptionState.SubscriptionState)" "viewerSubscription" [] (Api.Enum.SubscriptionState.decoder |> Decode.nullable)
