-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Interface.RepositoryAuditEntryData exposing (Fragments, fragments, maybeFragments, repository, repositoryName, repositoryResourcePath, repositoryUrl)

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
    { onOrgRestoreMemberMembershipRepositoryAuditEntryData : SelectionSet decodesTo Api.Object.OrgRestoreMemberMembershipRepositoryAuditEntryData
    , onPrivateRepositoryForkingDisableAuditEntry : SelectionSet decodesTo Api.Object.PrivateRepositoryForkingDisableAuditEntry
    , onPrivateRepositoryForkingEnableAuditEntry : SelectionSet decodesTo Api.Object.PrivateRepositoryForkingEnableAuditEntry
    , onRepoAccessAuditEntry : SelectionSet decodesTo Api.Object.RepoAccessAuditEntry
    , onRepoAddMemberAuditEntry : SelectionSet decodesTo Api.Object.RepoAddMemberAuditEntry
    , onRepoAddTopicAuditEntry : SelectionSet decodesTo Api.Object.RepoAddTopicAuditEntry
    , onRepoArchivedAuditEntry : SelectionSet decodesTo Api.Object.RepoArchivedAuditEntry
    , onRepoChangeMergeSettingAuditEntry : SelectionSet decodesTo Api.Object.RepoChangeMergeSettingAuditEntry
    , onRepoConfigDisableAnonymousGitAccessAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigDisableAnonymousGitAccessAuditEntry
    , onRepoConfigDisableCollaboratorsOnlyAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigDisableCollaboratorsOnlyAuditEntry
    , onRepoConfigDisableContributorsOnlyAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigDisableContributorsOnlyAuditEntry
    , onRepoConfigDisableSockpuppetDisallowedAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigDisableSockpuppetDisallowedAuditEntry
    , onRepoConfigEnableAnonymousGitAccessAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigEnableAnonymousGitAccessAuditEntry
    , onRepoConfigEnableCollaboratorsOnlyAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigEnableCollaboratorsOnlyAuditEntry
    , onRepoConfigEnableContributorsOnlyAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigEnableContributorsOnlyAuditEntry
    , onRepoConfigEnableSockpuppetDisallowedAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigEnableSockpuppetDisallowedAuditEntry
    , onRepoConfigLockAnonymousGitAccessAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigLockAnonymousGitAccessAuditEntry
    , onRepoConfigUnlockAnonymousGitAccessAuditEntry : SelectionSet decodesTo Api.Object.RepoConfigUnlockAnonymousGitAccessAuditEntry
    , onRepoCreateAuditEntry : SelectionSet decodesTo Api.Object.RepoCreateAuditEntry
    , onRepoDestroyAuditEntry : SelectionSet decodesTo Api.Object.RepoDestroyAuditEntry
    , onRepoRemoveMemberAuditEntry : SelectionSet decodesTo Api.Object.RepoRemoveMemberAuditEntry
    , onRepoRemoveTopicAuditEntry : SelectionSet decodesTo Api.Object.RepoRemoveTopicAuditEntry
    , onTeamAddRepositoryAuditEntry : SelectionSet decodesTo Api.Object.TeamAddRepositoryAuditEntry
    , onTeamRemoveRepositoryAuditEntry : SelectionSet decodesTo Api.Object.TeamRemoveRepositoryAuditEntry
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo Api.Interface.RepositoryAuditEntryData
fragments selections =
    Object.exhaustiveFragmentSelection
        [ Object.buildFragment "OrgRestoreMemberMembershipRepositoryAuditEntryData" selections.onOrgRestoreMemberMembershipRepositoryAuditEntryData
        , Object.buildFragment "PrivateRepositoryForkingDisableAuditEntry" selections.onPrivateRepositoryForkingDisableAuditEntry
        , Object.buildFragment "PrivateRepositoryForkingEnableAuditEntry" selections.onPrivateRepositoryForkingEnableAuditEntry
        , Object.buildFragment "RepoAccessAuditEntry" selections.onRepoAccessAuditEntry
        , Object.buildFragment "RepoAddMemberAuditEntry" selections.onRepoAddMemberAuditEntry
        , Object.buildFragment "RepoAddTopicAuditEntry" selections.onRepoAddTopicAuditEntry
        , Object.buildFragment "RepoArchivedAuditEntry" selections.onRepoArchivedAuditEntry
        , Object.buildFragment "RepoChangeMergeSettingAuditEntry" selections.onRepoChangeMergeSettingAuditEntry
        , Object.buildFragment "RepoConfigDisableAnonymousGitAccessAuditEntry" selections.onRepoConfigDisableAnonymousGitAccessAuditEntry
        , Object.buildFragment "RepoConfigDisableCollaboratorsOnlyAuditEntry" selections.onRepoConfigDisableCollaboratorsOnlyAuditEntry
        , Object.buildFragment "RepoConfigDisableContributorsOnlyAuditEntry" selections.onRepoConfigDisableContributorsOnlyAuditEntry
        , Object.buildFragment "RepoConfigDisableSockpuppetDisallowedAuditEntry" selections.onRepoConfigDisableSockpuppetDisallowedAuditEntry
        , Object.buildFragment "RepoConfigEnableAnonymousGitAccessAuditEntry" selections.onRepoConfigEnableAnonymousGitAccessAuditEntry
        , Object.buildFragment "RepoConfigEnableCollaboratorsOnlyAuditEntry" selections.onRepoConfigEnableCollaboratorsOnlyAuditEntry
        , Object.buildFragment "RepoConfigEnableContributorsOnlyAuditEntry" selections.onRepoConfigEnableContributorsOnlyAuditEntry
        , Object.buildFragment "RepoConfigEnableSockpuppetDisallowedAuditEntry" selections.onRepoConfigEnableSockpuppetDisallowedAuditEntry
        , Object.buildFragment "RepoConfigLockAnonymousGitAccessAuditEntry" selections.onRepoConfigLockAnonymousGitAccessAuditEntry
        , Object.buildFragment "RepoConfigUnlockAnonymousGitAccessAuditEntry" selections.onRepoConfigUnlockAnonymousGitAccessAuditEntry
        , Object.buildFragment "RepoCreateAuditEntry" selections.onRepoCreateAuditEntry
        , Object.buildFragment "RepoDestroyAuditEntry" selections.onRepoDestroyAuditEntry
        , Object.buildFragment "RepoRemoveMemberAuditEntry" selections.onRepoRemoveMemberAuditEntry
        , Object.buildFragment "RepoRemoveTopicAuditEntry" selections.onRepoRemoveTopicAuditEntry
        , Object.buildFragment "TeamAddRepositoryAuditEntry" selections.onTeamAddRepositoryAuditEntry
        , Object.buildFragment "TeamRemoveRepositoryAuditEntry" selections.onTeamRemoveRepositoryAuditEntry
        ]


{-| Can be used to create a non-exhaustive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onOrgRestoreMemberMembershipRepositoryAuditEntryData = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPrivateRepositoryForkingDisableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onPrivateRepositoryForkingEnableAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoAccessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoAddMemberAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoAddTopicAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoArchivedAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoChangeMergeSettingAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigDisableAnonymousGitAccessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigDisableCollaboratorsOnlyAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigDisableContributorsOnlyAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigDisableSockpuppetDisallowedAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigEnableAnonymousGitAccessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigEnableCollaboratorsOnlyAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigEnableContributorsOnlyAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigEnableSockpuppetDisallowedAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigLockAnonymousGitAccessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoConfigUnlockAnonymousGitAccessAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoCreateAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoDestroyAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoRemoveMemberAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onRepoRemoveTopicAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onTeamAddRepositoryAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    , onTeamRemoveRepositoryAuditEntry = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


{-| The repository associated with the action
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet (Maybe decodesTo) Api.Interface.RepositoryAuditEntryData
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ (identity >> Decode.nullable)


{-| The name of the repository
-}
repositoryName : SelectionSet (Maybe String) Api.Interface.RepositoryAuditEntryData
repositoryName =
    Object.selectionForField "(Maybe String)" "repositoryName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the repository
-}
repositoryResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Interface.RepositoryAuditEntryData
repositoryResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the repository
-}
repositoryUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Interface.RepositoryAuditEntryData
repositoryUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
