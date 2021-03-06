-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.EnterpriseDefaultRepositoryPermissionSettingValue exposing (EnterpriseDefaultRepositoryPermissionSettingValue(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| The possible values for the enterprise default repository permission setting.

  - NoPolicy - Organizations in the enterprise choose default repository permissions for their members.
  - Admin - Organization members will be able to clone, pull, push, and add new collaborators to all organization repositories.
  - Write - Organization members will be able to clone, pull, and push all organization repositories.
  - Read - Organization members will be able to clone and pull all organization repositories.
  - None - Organization members will only be able to clone and pull public repositories.

-}
type EnterpriseDefaultRepositoryPermissionSettingValue
    = NoPolicy
    | Admin
    | Write
    | Read
    | None


list : List EnterpriseDefaultRepositoryPermissionSettingValue
list =
    [ NoPolicy, Admin, Write, Read, None ]


decoder : Decoder EnterpriseDefaultRepositoryPermissionSettingValue
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "NO_POLICY" ->
                        Decode.succeed NoPolicy

                    "ADMIN" ->
                        Decode.succeed Admin

                    "WRITE" ->
                        Decode.succeed Write

                    "READ" ->
                        Decode.succeed Read

                    "NONE" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail ("Invalid EnterpriseDefaultRepositoryPermissionSettingValue type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : EnterpriseDefaultRepositoryPermissionSettingValue -> String
toString enum =
    case enum of
        NoPolicy ->
            "NO_POLICY"

        Admin ->
            "ADMIN"

        Write ->
            "WRITE"

        Read ->
            "READ"

        None ->
            "NONE"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe EnterpriseDefaultRepositoryPermissionSettingValue
fromString enumString =
    case enumString of
        "NO_POLICY" ->
            Just NoPolicy

        "ADMIN" ->
            Just Admin

        "WRITE" ->
            Just Write

        "READ" ->
            Just Read

        "NONE" ->
            Just None

        _ ->
            Nothing
