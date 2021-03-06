-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.CancelEnterpriseAdminInvitationPayload exposing (clientMutationId, invitation, message)

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


{-| A unique identifier for the client performing the mutation.
-}
clientMutationId : SelectionSet (Maybe String) Api.Object.CancelEnterpriseAdminInvitationPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The invitation that was canceled.
-}
invitation : SelectionSet decodesTo Api.Object.EnterpriseAdministratorInvitation -> SelectionSet (Maybe decodesTo) Api.Object.CancelEnterpriseAdminInvitationPayload
invitation object_ =
    Object.selectionForCompositeField "invitation" [] object_ (identity >> Decode.nullable)


{-| A message confirming the result of canceling an administrator invitation.
-}
message : SelectionSet (Maybe String) Api.Object.CancelEnterpriseAdminInvitationPayload
message =
    Object.selectionForField "(Maybe String)" "message" [] (Decode.string |> Decode.nullable)
