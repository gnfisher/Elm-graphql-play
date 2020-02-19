-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.OrderDirection exposing (OrderDirection(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Possible directions in which to order a list of items when provided an `orderBy` argument.

  - Asc - Specifies an ascending order for a given `orderBy` argument.
  - Desc - Specifies a descending order for a given `orderBy` argument.

-}
type OrderDirection
    = Asc
    | Desc


list : List OrderDirection
list =
    [ Asc, Desc ]


decoder : Decoder OrderDirection
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ASC" ->
                        Decode.succeed Asc

                    "DESC" ->
                        Decode.succeed Desc

                    _ ->
                        Decode.fail ("Invalid OrderDirection type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : OrderDirection -> String
toString enum =
    case enum of
        Asc ->
            "ASC"

        Desc ->
            "DESC"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrderDirection
fromString enumString =
    case enumString of
        "ASC" ->
            Just Asc

        "DESC" ->
            Just Desc

        _ ->
            Nothing
