-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.OrgRemoveOutsideCollaboratorAuditEntry exposing (action, actor, actorIp, actorLocation, actorLogin, actorResourcePath, actorUrl, createdAt, id, membershipTypes, operationType, organization, organizationName, organizationResourcePath, organizationUrl, reason, user, userLogin, userResourcePath, userUrl)

import Api.Enum.OperationType
import Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryMembershipType
import Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryReason
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


{-| The action name
-}
action : SelectionSet String Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
action =
    Object.selectionForField "String" "action" [] Decode.string


{-| The user who initiated the action
-}
actor : SelectionSet decodesTo Api.Union.AuditEntryActor -> SelectionSet (Maybe decodesTo) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| The IP address of the actor
-}
actorIp : SelectionSet (Maybe String) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actorIp =
    Object.selectionForField "(Maybe String)" "actorIp" [] (Decode.string |> Decode.nullable)


{-| A readable representation of the actor's location
-}
actorLocation : SelectionSet decodesTo Api.Object.ActorLocation -> SelectionSet (Maybe decodesTo) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actorLocation object_ =
    Object.selectionForCompositeField "actorLocation" [] object_ (identity >> Decode.nullable)


{-| The username of the user who initiated the action
-}
actorLogin : SelectionSet (Maybe String) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actorLogin =
    Object.selectionForField "(Maybe String)" "actorLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the actor.
-}
actorResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actorResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the actor.
-}
actorUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
actorUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The time the action was initiated
-}
createdAt : SelectionSet Api.ScalarCodecs.PreciseDateTime Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
createdAt =
    Object.selectionForField "ScalarCodecs.PreciseDateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecPreciseDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The types of membership the outside collaborator has with the organization.
-}
membershipTypes : SelectionSet (Maybe (List Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryMembershipType.OrgRemoveOutsideCollaboratorAuditEntryMembershipType)) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
membershipTypes =
    Object.selectionForField "(Maybe (List Enum.OrgRemoveOutsideCollaboratorAuditEntryMembershipType.OrgRemoveOutsideCollaboratorAuditEntryMembershipType))" "membershipTypes" [] (Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryMembershipType.decoder |> Decode.list |> Decode.nullable)


{-| The corresponding operation type for the action
-}
operationType : SelectionSet (Maybe Api.Enum.OperationType.OperationType) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
operationType =
    Object.selectionForField "(Maybe Enum.OperationType.OperationType)" "operationType" [] (Api.Enum.OperationType.decoder |> Decode.nullable)


{-| The Organization associated with the Audit Entry.
-}
organization : SelectionSet decodesTo Api.Object.Organization -> SelectionSet (Maybe decodesTo) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
organization object_ =
    Object.selectionForCompositeField "organization" [] object_ (identity >> Decode.nullable)


{-| The name of the Organization.
-}
organizationName : SelectionSet (Maybe String) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
organizationName =
    Object.selectionForField "(Maybe String)" "organizationName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the organization
-}
organizationResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
organizationResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the organization
-}
organizationUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
organizationUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The reason for the outside collaborator being removed from the Organization.
-}
reason : SelectionSet (Maybe Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryReason.OrgRemoveOutsideCollaboratorAuditEntryReason) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
reason =
    Object.selectionForField "(Maybe Enum.OrgRemoveOutsideCollaboratorAuditEntryReason.OrgRemoveOutsideCollaboratorAuditEntryReason)" "reason" [] (Api.Enum.OrgRemoveOutsideCollaboratorAuditEntryReason.decoder |> Decode.nullable)


{-| The user affected by the action
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
user object_ =
    Object.selectionForCompositeField "user" [] object_ (identity >> Decode.nullable)


{-| For actions involving two users, the actor is the initiator and the user is the affected user.
-}
userLogin : SelectionSet (Maybe String) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
userLogin =
    Object.selectionForField "(Maybe String)" "userLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the user.
-}
userResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
userResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the user.
-}
userUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.OrgRemoveOutsideCollaboratorAuditEntry
userUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
