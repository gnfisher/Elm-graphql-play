-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.ActionExecutionCapabilitySetting exposing (ActionExecutionCapabilitySetting(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| The possible capabilities for action executions setting.

  - Disabled - All action executions are disabled.
  - AllActions - All action executions are enabled.
  - LocalActionsOnly - Only actions defined within the repo are allowed.
  - NoPolicy - Organization administrators action execution capabilities.

-}
type ActionExecutionCapabilitySetting
    = Disabled
    | AllActions
    | LocalActionsOnly
    | NoPolicy


list : List ActionExecutionCapabilitySetting
list =
    [ Disabled, AllActions, LocalActionsOnly, NoPolicy ]


decoder : Decoder ActionExecutionCapabilitySetting
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "DISABLED" ->
                        Decode.succeed Disabled

                    "ALL_ACTIONS" ->
                        Decode.succeed AllActions

                    "LOCAL_ACTIONS_ONLY" ->
                        Decode.succeed LocalActionsOnly

                    "NO_POLICY" ->
                        Decode.succeed NoPolicy

                    _ ->
                        Decode.fail ("Invalid ActionExecutionCapabilitySetting type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : ActionExecutionCapabilitySetting -> String
toString enum =
    case enum of
        Disabled ->
            "DISABLED"

        AllActions ->
            "ALL_ACTIONS"

        LocalActionsOnly ->
            "LOCAL_ACTIONS_ONLY"

        NoPolicy ->
            "NO_POLICY"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe ActionExecutionCapabilitySetting
fromString enumString =
    case enumString of
        "DISABLED" ->
            Just Disabled

        "ALL_ACTIONS" ->
            Just AllActions

        "LOCAL_ACTIONS_ONLY" ->
            Just LocalActionsOnly

        "NO_POLICY" ->
            Just NoPolicy

        _ ->
            Nothing
