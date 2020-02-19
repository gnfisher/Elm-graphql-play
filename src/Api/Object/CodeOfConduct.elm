-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.CodeOfConduct exposing (body, id, key, name, resourcePath, url)

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


{-| The body of the Code of Conduct
-}
body : SelectionSet (Maybe String) Api.Object.CodeOfConduct
body =
    Object.selectionForField "(Maybe String)" "body" [] (Decode.string |> Decode.nullable)


id : SelectionSet Api.ScalarCodecs.Id Api.Object.CodeOfConduct
id =
    Object.selectionForField "ScalarCodecs.Id" "id" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecId |> .decoder)


{-| The key for the Code of Conduct
-}
key : SelectionSet String Api.Object.CodeOfConduct
key =
    Object.selectionForField "String" "key" [] Decode.string


{-| The formal name of the Code of Conduct
-}
name : SelectionSet String Api.Object.CodeOfConduct
name =
    Object.selectionForField "String" "name" [] Decode.string


{-| The HTTP path for this Code of Conduct
-}
resourcePath : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.CodeOfConduct
resourcePath =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "resourcePath" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)


{-| The HTTP URL for this Code of Conduct
-}
url : SelectionSet (Maybe Api.ScalarCodecs.Uri) Api.Object.CodeOfConduct
url =
    Object.selectionForField "(Maybe ScalarCodecs.Uri)" "url" [] (Api.ScalarCodecs.codecs |> Api.Scalar.unwrapCodecs |> .codecUri |> .decoder |> Decode.nullable)