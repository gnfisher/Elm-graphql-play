-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.OrgRestoreMemberMembershipTeamAuditEntryData exposing (team, teamName, teamResourcePath, teamUrl)

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


{-| The team associated with the action
-}
team : SelectionSet decodesTo Api.Object.Team -> SelectionSet (Maybe decodesTo) Api.Object.OrgRestoreMemberMembershipTeamAuditEntryData
team object_ =
    Object.selectionForCompositeField "team" [] object_ (identity >> Decode.nullable)


{-| The name of the team
-}
teamName : SelectionSet (Maybe String) Api.Object.OrgRestoreMemberMembershipTeamAuditEntryData
teamName =
    Object.selectionForField "(Maybe String)" "teamName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for this team
-}
teamResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRestoreMemberMembershipTeamAuditEntryData
teamResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "teamResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for this team
-}
teamUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRestoreMemberMembershipTeamAuditEntryData
teamUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "teamUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
