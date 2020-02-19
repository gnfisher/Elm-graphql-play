-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UserBlockedEvent exposing (actor, blockDuration, createdAt, id, subject)

import Api.Enum.UserBlockDuration
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


{-| Identifies the actor who performed the event.
-}
actor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.UserBlockedEvent
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| Number of days that the user was blocked for.
-}
blockDuration : SelectionSet Api.Enum.UserBlockDuration.UserBlockDuration Api.Object.UserBlockedEvent
blockDuration =
    Object.selectionForField "Enum.UserBlockDuration.UserBlockDuration" "blockDuration" [] Api.Enum.UserBlockDuration.decoder


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserBlockedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.UserBlockedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The user who was blocked.
-}
subject : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.UserBlockedEvent
subject object_ =
    Object.selectionForCompositeField "subject" [] object_ (identity >> Decode.nullable)
