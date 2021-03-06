-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.AssignedEvent exposing (actor, assignable, assignee, createdAt, id, user)

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
actor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.AssignedEvent
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| Identifies the assignable associated with the event.
-}
assignable : SelectionSet decodesTo Api.Interface.Assignable -> SelectionSet decodesTo Api.Object.AssignedEvent
assignable object_ =
    Object.selectionForCompositeField "assignable" [] object_ identity


{-| Identifies the user or mannequin that was assigned.
-}
assignee : SelectionSet decodesTo Api.Union.Assignee -> SelectionSet (Maybe decodesTo) Api.Object.AssignedEvent
assignee object_ =
    Object.selectionForCompositeField "assignee" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.AssignedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.AssignedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Identifies the user who was assigned.
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.AssignedEvent
user object_ =
    Object.selectionForCompositeField "user" [] object_ (identity >> Decode.nullable)
