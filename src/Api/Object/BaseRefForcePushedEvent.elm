-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.BaseRefForcePushedEvent exposing (actor, afterCommit, beforeCommit, createdAt, id, pullRequest, ref)

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
actor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.BaseRefForcePushedEvent
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| Identifies the after commit SHA for the 'base\_ref\_force\_pushed' event.
-}
afterCommit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (Maybe decodesTo) Api.Object.BaseRefForcePushedEvent
afterCommit object_ =
    Object.selectionForCompositeField "afterCommit" [] object_ (identity >> Decode.nullable)


{-| Identifies the before commit SHA for the 'base\_ref\_force\_pushed' event.
-}
beforeCommit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (Maybe decodesTo) Api.Object.BaseRefForcePushedEvent
beforeCommit object_ =
    Object.selectionForCompositeField "beforeCommit" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.BaseRefForcePushedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.BaseRefForcePushedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| PullRequest referenced by event.
-}
pullRequest : SelectionSet decodesTo Api.Object.PullRequest -> SelectionSet decodesTo Api.Object.BaseRefForcePushedEvent
pullRequest object_ =
    Object.selectionForCompositeField "pullRequest" [] object_ identity


{-| Identifies the fully qualified ref name for the 'base\_ref\_force\_pushed' event.
-}
ref : SelectionSet decodesTo Api.Object.Ref -> SelectionSet (Maybe decodesTo) Api.Object.BaseRefForcePushedEvent
ref object_ =
    Object.selectionForCompositeField "ref" [] object_ (identity >> Decode.nullable)
