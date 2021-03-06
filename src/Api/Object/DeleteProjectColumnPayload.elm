-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.DeleteProjectColumnPayload exposing (clientMutationId, deletedColumnId, project)

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
clientMutationId : SelectionSet (Maybe String) Api.Object.DeleteProjectColumnPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


{-| The deleted column ID.
-}
deletedColumnId : SelectionSet (Maybe Api.ScalarCodecs.Id) Api.Object.DeleteProjectColumnPayload
deletedColumnId =
    Object.selectionForField "(Maybe ScalarCodecs.Id)" "deletedColumnId" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


{-| The project the deleted column was in.
-}
project : SelectionSet decodesTo Api.Object.Project -> SelectionSet (Maybe decodesTo) Api.Object.DeleteProjectColumnPayload
project object_ =
    Object.selectionForCompositeField "project" [] object_ (identity >> Decode.nullable)
