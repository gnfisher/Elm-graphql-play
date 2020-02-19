-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.RegistryPackageDependencyType exposing (RegistryPackageDependencyType(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| The possible types of a registry package dependency.

  - Default - A default registry package dependency type.
  - Dev - A dev registry package dependency type.
  - Test - A test registry package dependency type.
  - Peer - A peer registry package dependency type.
  - Optional - An optional registry package dependency type.
  - Bundled - An optional registry package dependency type.

-}
type RegistryPackageDependencyType
    = Default
    | Dev
    | Test
    | Peer
    | Optional
    | Bundled


list : List RegistryPackageDependencyType
list =
    [ Default, Dev, Test, Peer, Optional, Bundled ]


decoder : Decoder RegistryPackageDependencyType
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DEFAULT" ->
                        Decode.succeed Default

                    "DEV" ->
                        Decode.succeed Dev

                    "TEST" ->
                        Decode.succeed Test

                    "PEER" ->
                        Decode.succeed Peer

                    "OPTIONAL" ->
                        Decode.succeed Optional

                    "BUNDLED" ->
                        Decode.succeed Bundled

                    _ ->
                        Decode.fail ("Invalid RegistryPackageDependencyType type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : RegistryPackageDependencyType -> String
toString enum =
    case enum of
        Default ->
            "DEFAULT"

        Dev ->
            "DEV"

        Test ->
            "TEST"

        Peer ->
            "PEER"

        Optional ->
            "OPTIONAL"

        Bundled ->
            "BUNDLED"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe RegistryPackageDependencyType
fromString enumString =
    case enumString of
        "DEFAULT" ->
            Just Default

        "DEV" ->
            Just Dev

        "TEST" ->
            Just Test

        "PEER" ->
            Just Peer

        "OPTIONAL" ->
            Just Optional

        "BUNDLED" ->
            Just Bundled

        _ ->
            Nothing
