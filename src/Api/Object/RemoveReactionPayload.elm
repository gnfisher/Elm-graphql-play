-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.RemoveReactionPayload exposing (clientMutationId, reaction, subject)

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
clientMutationId : SelectionSet (Maybe String) Api.Object.RemoveReactionPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The reaction object.
-}
reaction : SelectionSet decodesTo Api.Object.Reaction -> SelectionSet (Maybe decodesTo) Api.Object.RemoveReactionPayload
reaction object_ =
    Object.selectionForCompositeField "reaction" [] object_ (identity >> Decode.nullable)


{-| The reactable subject.
-}
subject : SelectionSet decodesTo Api.Interface.Reactable -> SelectionSet (Maybe decodesTo) Api.Object.RemoveReactionPayload
subject object_ =
    Object.selectionForCompositeField "subject" [] object_ (identity >> Decode.nullable)
