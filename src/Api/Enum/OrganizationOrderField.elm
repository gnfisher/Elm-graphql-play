-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.OrganizationOrderField exposing (OrganizationOrderField(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which organization connections can be ordered.

  - CreatedAt - Order organizations by creation time
  - Login - Order organizations by login

-}
type OrganizationOrderField
    = CreatedAt
    | Login


list : List OrganizationOrderField
list =
    [ CreatedAt, Login ]


decoder : Decoder OrganizationOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    "LOGIN" ->
                        Decode.succeed Login

                    _ ->
                        Decode.fail ("Invalid OrganizationOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : OrganizationOrderField -> String
toString enum =
    case enum of
        CreatedAt ->
            "CREATED_AT"

        Login ->
            "LOGIN"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe OrganizationOrderField
fromString enumString =
    case enumString of
        "CREATED_AT" ->
            Just CreatedAt

        "LOGIN" ->
            Just Login

        _ ->
            Nothing
