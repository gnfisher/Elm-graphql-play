-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.LabelOrderField exposing (LabelOrderField(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which label connections can be ordered.

  - Name - Order labels by name
  - CreatedAt - Order labels by creation time

-}
type LabelOrderField
    = Name
    | CreatedAt


list : List LabelOrderField
list =
    [ Name, CreatedAt ]


decoder : Decoder LabelOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "NAME" ->
                        Decode.succeed Name

                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    _ ->
                        Decode.fail ("Invalid LabelOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : LabelOrderField -> String
toString enum =
    case enum of
        Name ->
            "NAME"

        CreatedAt ->
            "CREATED_AT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe LabelOrderField
fromString enumString =
    case enumString of
        "NAME" ->
            Just Name

        "CREATED_AT" ->
            Just CreatedAt

        _ ->
            Nothing