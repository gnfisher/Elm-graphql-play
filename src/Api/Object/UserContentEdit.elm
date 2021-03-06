-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.UserContentEdit exposing (createdAt, deletedAt, deletedBy, diff, editedAt, editor, id, updatedAt)

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


{-| Identifies the date and time when the object was created.
-}
createdAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserContentEdit
createdAt =
    Object.selectionForField "ScalarCodecs.DateTime" "createdAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| Identifies the date and time when the object was deleted.
-}
deletedAt : SelectionSet (Maybe Api.ScalarCodecs.DateTime) Api.Object.UserContentEdit
deletedAt =
    Object.selectionForField "(Maybe ScalarCodecs.DateTime)" "deletedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder |> Decode.nullable)


{-| The actor who deleted this content
-}
deletedBy : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.UserContentEdit
deletedBy object_ =
    Object.selectionForCompositeField "deletedBy" [] object_ (identity >> Decode.nullable)


{-| A summary of the changes for this edit
-}
diff : SelectionSet (Maybe String) Api.Object.UserContentEdit
diff =
    Object.selectionForField "(Maybe String)" "diff" [] (Decode.string |> Decode.nullable)


{-| When this content was edited
-}
editedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserContentEdit
editedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "editedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


{-| The actor who edited this content
-}
editor : SelectionSet decodesTo Api.Interface.Actor -> SelectionSet (Maybe decodesTo) Api.Object.UserContentEdit
editor object_ =
    Object.selectionForCompositeField "editor" [] object_ (identity >> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.UserContentEdit
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Identifies the date and time when the object was last updated.
-}
updatedAt : SelectionSet Api.ScalarCodecs.DateTime Api.Object.UserContentEdit
updatedAt =
    Object.selectionForField "ScalarCodecs.DateTime" "updatedAt" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)
