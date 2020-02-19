-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.ContributionOrderField exposing (ContributionOrderField(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| Properties by which contribution connections can be ordered.

  - OccurredAt - Order contributions by when they were made.

-}
type ContributionOrderField
    = OccurredAt


list : List ContributionOrderField
list =
    [ OccurredAt ]


decoder : Decoder ContributionOrderField
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "OCCURRED_AT" ->
                        Decode.succeed OccurredAt

                    _ ->
                        Decode.fail ("Invalid ContributionOrderField type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : ContributionOrderField -> String
toString enum =
    case enum of
        OccurredAt ->
            "OCCURRED_AT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ContributionOrderField
fromString enumString =
    case enumString of
        "OCCURRED_AT" ->
            Just OccurredAt

        _ ->
            Nothing
