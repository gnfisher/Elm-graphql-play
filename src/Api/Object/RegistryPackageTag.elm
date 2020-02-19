-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.RegistryPackageTag exposing (id, name, version)

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


id : SelectionSet Api.ScalarCodecs.Id Api.Object.RegistryPackageTag
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| Identifies the tag name of the version.
-}
name : SelectionSet String Api.Object.RegistryPackageTag
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| version that the tag is associated with
-}
version : SelectionSet decodesTo Api.Object.RegistryPackageVersion -> SelectionSet (Maybe decodesTo) Api.Object.RegistryPackageTag
version object_ =
    Object.selectionForCompositeField "version" [] object_ (identity >> Decode.nullable)