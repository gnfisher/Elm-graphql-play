-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.MergedEvent exposing (actor, commit, createdAt, id, mergeRef, mergeRefName, pullRequest, resourcePath, url)

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
actor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.MergedEvent
actor object_ =
    Object.selectionForCompositeField "actor" [] object_ (identity >> Decode.nullable)


{-| Identifies the commit associated with the `merge` event.
-}
commit : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (Maybe decodesTo) Api.Object.MergedEvent
commit object_ =
    Object.selectionForCompositeField "commit" [] object_ (identity >> Decode.nullable)


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.MergedEvent
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.MergedEvent
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Identifies the Ref associated with the `merge` event.
-}
mergeRef : SelectionSet decodesTo Api.Object.Ref -> SelectionSet (Maybe decodesTo) Api.Object.MergedEvent
mergeRef object_ =
    Object.selectionForCompositeField "mergeRef" [] object_ (identity >> Decode.nullable)


{-| Identifies the name of the Ref associated with the `merge` event.
-}
mergeRefName : SelectionSet String Api.Object.MergedEvent
mergeRefName =
    Object.selectionForField "String" "mergeRefName" [] Decode.string


{-| PullRequest referenced by event.
-}
pullRequest : SelectionSet decodesTo Api.Object.PullRequest -> SelectionSet decodesTo Api.Object.MergedEvent
pullRequest object_ =
    Object.selectionForCompositeField "pullRequest" [] object_ identity


{-| The HTTP path for this merged event.
-}
resourcePath : SelectionSet Api.ScalarCodecs.Uri Api.Object.MergedEvent
resourcePath =
    Object.selectionForField "ScalarCodecs.Uri" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)


{-| The HTTP URL for this merged event.
-}
url : SelectionSet Api.ScalarCodecs.Uri Api.Object.MergedEvent
url =
    Object.selectionForField "ScalarCodecs.Uri" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder)
