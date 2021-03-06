-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.RepositoryOrderField exposing (RepositoryOrderField(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which repository connections can be ordered.

  - CreatedAt - Order repositories by creation time
  - UpdatedAt - Order repositories by update time
  - PushedAt - Order repositories by push time
  - Name - Order repositories by name
  - Stargazers - Order repositories by number of stargazers

-}
type RepositoryOrderField
    = CreatedAt
    | UpdatedAt
    | PushedAt
    | Name
    | Stargazers


list : List RepositoryOrderField
list =
    [ CreatedAt, UpdatedAt, PushedAt, Name, Stargazers ]


decoder : Decoder RepositoryOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "CREATED_AT" ->
                        Decode.succeed CreatedAt

                    "UPDATED_AT" ->
                        Decode.succeed UpdatedAt

                    "PUSHED_AT" ->
                        Decode.succeed PushedAt

                    "NAME" ->
                        Decode.succeed Name

                    "STARGAZERS" ->
                        Decode.succeed Stargazers

                    _ ->
                        Decode.fail ("Invalid RepositoryOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : RepositoryOrderField -> String
toString enum =
    case enum of
        CreatedAt ->
            "CREATED_AT"

        UpdatedAt ->
            "UPDATED_AT"

        PushedAt ->
            "PUSHED_AT"

        Name ->
            "NAME"

        Stargazers ->
            "STARGAZERS"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe RepositoryOrderField
fromString enumString =
    case enumString of
        "CREATED_AT" ->
            Just CreatedAt

        "UPDATED_AT" ->
            Just UpdatedAt

        "PUSHED_AT" ->
            Just PushedAt

        "NAME" ->
            Just Name

        "STARGAZERS" ->
            Just Stargazers

        _ ->
            Nothing
