-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Interface.EnterpriseAuditEntryData exposing (Fragments, enterpriseResourcePath, enterpriseSlug, enterpriseUrl, fragments, maybeFragments)

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
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onMembersCanDeleteReposClearAuditEntry : SelectionSet decodesTo Api.Object.MembersCanDeleteReposClearAuditEntry
    , onMembersCanDeleteReposDisableAuditEntry : SelectionSet decodesTo Api.Object.MembersCanDeleteReposDisableAuditEntry
    , onMembersCanDeleteReposEnableAuditEntry : SelectionSet decodesTo Api.Object.MembersCanDeleteReposEnableAuditEntry
    , onOrgInviteToBusinessAuditEntry : SelectionSet decodesTo Api.Object.OrgInviteToBusinessAuditEntry
    , onPrivateRepositoryForkingDisableAuditEntry : SelectionSet decodesTo Api.Object.PrivateRepositoryForkingDisableAuditEntry
    , onPrivateRepositoryForkingEnableAuditEntry : SelectionSet decodesTo Api.Object.PrivateRepositoryForkingEnableAuditEntry
    , onRepositoryVisibilityChangeDisableAuditEntry : SelectionSet decodesTo Api.Object.RepositoryVisibilityChangeDisableAuditEntry
    , onRepositoryVisibilityChangeEnableAuditEntry : SelectionSet decodesTo Api.Object.RepositoryVisibilityChangeEnableAuditEntry
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Api.Interface.EnterpriseAuditEntryData
fragments selections =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "MembersCanDeleteReposClearAuditEntry" selections.onMembersCanDeleteReposClearAuditEntry
        , Object.buildFragment "MembersCanDeleteReposDisableAuditEntry" selections.onMembersCanDeleteReposDisableAuditEntry
        , Object.buildFragment "MembersCanDeleteReposEnableAuditEntry" selections.onMembersCanDeleteReposEnableAuditEntry
        , Object.buildFragment "OrgInviteToBusinessAuditEntry" selections.onOrgInviteToBusinessAuditEntry
        , Object.buildFragment "PrivateRepositoryForkingDisableAuditEntry" selections.onPrivateRepositoryForkingDisableAuditEntry
        , Object.buildFragment "PrivateRepositoryForkingEnableAuditEntry" selections.onPrivateRepositoryForkingEnableAuditEntry
        , Object.buildFragment "RepositoryVisibilityChangeDisableAuditEntry" selections.onRepositoryVisibilityChangeDisableAuditEntry
        , Object.buildFragment "RepositoryVisibilityChangeEnableAuditEntry" selections.onRepositoryVisibilityChangeEnableAuditEntry
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onMembersCanDeleteReposClearAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onMembersCanDeleteReposDisableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onMembersCanDeleteReposEnableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onOrgInviteToBusinessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPrivateRepositoryForkingDisableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPrivateRepositoryForkingEnableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepositoryVisibilityChangeDisableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepositoryVisibilityChangeEnableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


{-| The HTTP path for this enterprise.
-}
enterpriseResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Interface.EnterpriseAuditEntryData
enterpriseResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "enterpriseResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The slug of the enterprise.
-}
enterpriseSlug : SelectionSet (Maybe String) Api.Interface.EnterpriseAuditEntryData
enterpriseSlug =
    Object.selectionForField "(Maybe String)" "enterpriseSlug" [] (Decode.string |> Decode.nullable)


{-| The HTTP URL for this enterprise.
-}
enterpriseUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Interface.EnterpriseAuditEntryData
enterpriseUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "enterpriseUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)