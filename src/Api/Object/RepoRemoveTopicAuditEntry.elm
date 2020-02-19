-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.RepoRemoveTopicAuditEntry exposing (action, actor, actorIp, actorLocation, actorLogin, actorResourcePath, actorUrl, createdAt, id, operationType, organization, organizationName, organizationResourcePath, organizationUrl, repository, repositoryName, repositoryResourcePath, repositoryUrl, topic, topicName, user, userLogin, userResourcePath, userUrl)

import Api.Enum.OperationType
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
action : SelectionSet String Api.Object.RepoRemoveTopicAuditEntry
action =
    Object.selectionForField "String" "action" [] Decode.string


{-| The user who initiated the action
-}
actor : SelectionSet decodesTo Api.Union.AuditEntryActor -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| The IP address of the actor
-}
actorIp : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
actorIp =
    Object.selectionForField "(Maybe String)" "actorIp" [] (Decode.string |> Decode.nullable)


{-| A readable representation of the actor's location
-}
actorLocation : SelectionSet decodesTo Api.Object.ActorLocation -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
actorLocation object_ =
    Object.selectionForCompositeField "actorLocation" [] object_ (identity >> Decode.nullable)


{-| The username of the user who initiated the action
-}
actorLogin : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
actorLogin =
    Object.selectionForField "(Maybe String)" "actorLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the actor.
-}
actorResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
actorResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the actor.
-}
actorUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
actorUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "actorUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The time the action was initiated
-}
createdAt : SelectionSet Api.ScalarCodecs.PreciseDateTime Api.Object.RepoRemoveTopicAuditEntry
createdAt =
    Object.selectionForField "ScalarCodecs.PreciseDateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecPreciseDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.RepoRemoveTopicAuditEntry
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The corresponding operation type for the action
-}
operationType : SelectionSet (Maybe Api.Enum.OperationType.OperationType) Api.Object.RepoRemoveTopicAuditEntry
operationType =
    Object.selectionForField "(Maybe Enum.OperationType.OperationType)" "operationType" [] (Api.Enum.OperationType.decoder |> Decode.nullable)


{-| The Organization associated with the Audit Entry.
-}
organization : SelectionSet decodesTo Api.Object.Organization -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
organization object_ =
    Object.selectionForCompositeField "organization" [] object_ (identity >> Decode.nullable)


{-| The name of the Organization.
-}
organizationName : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
organizationName =
    Object.selectionForField "(Maybe String)" "organizationName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the organization
-}
organizationResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
organizationResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the organization
-}
organizationUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
organizationUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "organizationUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The repository associated with the action
-}
repository : SelectionSet decodesTo Api.Object.Repository -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
repository object_ =
    Object.selectionForCompositeField "repository" [] object_ (identity >> Decode.nullable)


{-| The name of the repository
-}
repositoryName : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
repositoryName =
    Object.selectionForField "(Maybe String)" "repositoryName" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the repository
-}
repositoryResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
repositoryResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the repository
-}
repositoryUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
repositoryUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "repositoryUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The name of the topic added to the repository
-}
topic : SelectionSet decodesTo Api.Object.Topic -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
topic object_ =
    Object.selectionForCompositeField "topic" [] object_ (identity >> Decode.nullable)


{-| The name of the topic added to the repository
-}
topicName : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
topicName =
    Object.selectionForField "(Maybe String)" "topicName" [] (Decode.string |> Decode.nullable)


{-| The user affected by the action
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.RepoRemoveTopicAuditEntry
user object_ =
    Object.selectionForCompositeField "user" [] object_ (identity >> Decode.nullable)


{-| For actions involving two users, the actor is the initiator and the user is the affected user.
-}
userLogin : SelectionSet (Maybe String) Api.Object.RepoRemoveTopicAuditEntry
userLogin =
    Object.selectionForField "(Maybe String)" "userLogin" [] (Decode.string |> Decode.nullable)


{-| The HTTP path for the user.
-}
userResourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
userResourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userResourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for the user.
-}
userUrl : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.RepoRemoveTopicAuditEntry
userUrl =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "userUrl" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)
